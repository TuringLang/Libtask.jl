# Helper struct used in `derive_copyable_task_ir`.
struct TupleRef
    n::Int
end

# Unclear whether this is needed.
get_value(x::GlobalRef) = getglobal(x.mod, x.name)
get_value(x::QuoteNode) = x.value
get_value(x) = x

"""
    is_produce_stmt(x)::Bool

`true` if `x` is an expression of the form `Expr(:call, produce, %x)` or a similar `:invoke`
expression, otherwise `false`.
"""
function is_produce_stmt(x)::Bool
    if Meta.isexpr(x, :invoke) &&
        length(x.args) == 3 &&
        x.args[1] isa Union{Core.MethodInstance,Core.CodeInstance}
        return get_mi(x.args[1]).specTypes <: Tuple{typeof(produce),Any}
    elseif Meta.isexpr(x, :call) && length(x.args) == 2
        return get_value(x.args[1]) === produce
    else
        return false
    end
end

"""
    stmt_might_produce(x, ret_type::Type; assume_returns::Bool=false)::Bool

`true` if `x` might contain a call to `produce`, and `false` otherwise.

By default, a statement whose inferred return type is `Union{}` is assumed not to produce:
it terminates in an unusual fashion (e.g. by throwing), so the splitting logic does not need
to treat it as a possible `produce` site. Pass `assume_returns=true` to skip this
short-circuit -- a call which produces *and then* throws still produces, which matters when
deciding whether a `produce` could suspend inside an exception handler (see
[`assert_can_handle_control_flow`](@ref)).
"""
function stmt_might_produce(x, ret_type::Type; assume_returns::Bool=false)::Bool

    # Statement will terminate in an unusual fashion, so don't bother recursing.
    # This isn't _strictly_ correct (there could be a `produce` statement before the
    # `throw` call is hit), but for the splitting logic this seems unlikely to happen in
    # practice. Exception-handler safety analysis sets `assume_returns` to be exact here.
    !assume_returns && ret_type == Union{} && return false

    # Statement will terminate in the usual fashion, so _do_ bother recusing.
    is_produce_stmt(x) && return true
    if Meta.isexpr(x, :invoke)
        mi_sig = get_mi(x.args[1]).specTypes
        return (
            might_produce(mi_sig) || any(might_produce_if_sig_contains, mi_sig.parameters)
        )
    end
    if Meta.isexpr(x, :call)
        # This is a hack -- it's perfectly possible for `DataType` calls to produce in general.
        f = get_function(x.args[1])
        _might_produce = !isa(f, Union{Core.IntrinsicFunction,Core.Builtin,DataType})
        return _might_produce
    end
    return false
end

get_function(x) = x
get_function(x::Expr) = eval(x)
get_function(x::GlobalRef) = isconst(x) ? getglobal(x.mod, x.name) : x.binding

"""
    produce_value(x::Expr)

Returns the value that a `produce` statement returns. For example, for the statment
`produce(%x)`, this function will return `%x`.
"""
function produce_value(x::Expr)
    is_produce_stmt(x) || throw(error("Not a produce statement. Please report this error."))
    Meta.isexpr(x, :invoke) && return x.args[3]
    return x.args[2] # must be a `:call` Expr.
end

struct ProducedValue{T}
    x::T
end
ProducedValue(::Type{T}) where {T} = ProducedValue{Type{T}}(T)

@inline Base.getindex(x::ProducedValue) = x.x

"""
    inc_args(stmt::T)::T where {T}

Returns a new `T` which is equal to `stmt`, except any `Argument`s present in `stmt` are
incremented by `1`. For example
```jldoctest
julia> Libtask.inc_args(Core.ReturnNode(Core.Argument(1)))
:(return _2)
```
"""
inc_args(x::Expr) = Expr(x.head, map(__inc, x.args)...)
inc_args(x::ReturnNode) = isdefined(x, :val) ? ReturnNode(__inc(x.val)) : x
inc_args(x::IDGotoIfNot) = IDGotoIfNot(__inc(x.cond), x.dest)
inc_args(x::IDGotoNode) = x
function inc_args(x::IDPhiNode)
    new_values = Vector{Any}(undef, length(x.values))
    for n in eachindex(x.values)
        if isassigned(x.values, n)
            new_values[n] = __inc(x.values[n])
        end
    end
    return IDPhiNode(x.edges, new_values)
end
inc_args(::Nothing) = nothing
inc_args(x::GlobalRef) = x
inc_args(x::Core.PiNode) = Core.PiNode(__inc(x.val), __inc(x.typ))
inc_args(x::IDEnterNode) = remake_enter(x, x.catch_id, __inc)

__inc(x::Argument) = Argument(x.n + 1)
__inc(x) = x

const TypeInfo = Tuple{Vector{Any},Dict{ID,Type}}

"""
    _typeof(x)

Central definition of typeof, which is specific to the use-required in this package.
Largely the same as `Base._stable_typeof`, differing only in a handful of
situations, for example:
```jldoctest
julia> Base._stable_typeof((Float64,))
Tuple{DataType}

julia> Libtask._typeof((Float64,))
Tuple{Type{Float64}}
```
"""
_typeof(x) = Base._stable_typeof(x)
_typeof(x::Tuple) = Tuple{map(_typeof, x)...}
_typeof(x::NamedTuple{names}) where {names} = NamedTuple{names,_typeof(Tuple(x))}

"""
    get_type(info::ADInfo, x)

Returns the static / inferred type associated to `x`.
"""
get_type(info::TypeInfo, x::Argument) = info[1][x.n - 1]
get_type(info::TypeInfo, x::ID) = CC.widenconst(info[2][x])
get_type(::TypeInfo, x::QuoteNode) = _typeof(x.value)
get_type(::TypeInfo, x) = _typeof(x)
function get_type(::TypeInfo, x::GlobalRef)
    return isconst(x) ? _typeof(getglobal(x.mod, x.name)) : x.binding.ty
end
function get_type(::TypeInfo, x::Expr)
    x.head === :boundscheck && return Bool
    return error("Unrecognised expression $x found in argument slot.")
end

"""
    assert_can_handle_control_flow(ir::BBCode)

Check that any `try` / `catch` / `finally` control flow in `ir` is something a `TapedTask`
can faithfully reproduce, throwing an informative `ArgumentError` otherwise.

A `TapedTask` is a suspend/resume state machine: a `produce` returns from the underlying
`OpaqueClosure`, and the next `consume` re-enters it and jumps straight to the resumption
point. The exception-handler frame established by an `enter` lives on that closure's call
stack, so it does not survive a suspend/resume boundary. This splits exception handling
into three cases:

  - Safe — every `enter` and its matching `pop_exception` / `leave` execute within a single
    `consume` call (no `produce` suspends while a handler is active). The handler frame is
    created and torn down within one closure invocation, so the control flow can be
    reproduced directly. This is the case we support.

  - Unsafe — a `produce` can suspend while a handler is active (a `produce` in the `try`
    body, or in the `catch` before `pop_exception`). On resume the handler frame is gone and
    the stale exception token would be used, so this cannot work without re-architecting how
    task state is captured. This is the case in issue #194's MWE; we reject it here.

  - Already fine — a `try` / `catch` the compiler can prove never throws is optimised away
    before we ever see it, so it generates no exception IR and needs no handling.

The design decision is therefore to support the safe subset and reject the unsafe subset
with a clear error rather than miscompile it. Value-carrying `catch` blocks (which lower to
`UpsilonNode` / `PhiCNode`) are not yet supported even when safe, and are rejected too; see
the follow-up tracked alongside issue #194.

Detection works directly on the `BBCode`, which represents every `enter` (`Core.EnterNode`
on Julia 1.11+, `Expr(:enter)` on Julia 1.10) uniformly as an `IDEnterNode` and
already carries the exception (catch) edges in its CFG. We run a forward data-flow pass over
the blocks tracking how many exception-handler frames are live; a `produce` (or a call which
might `produce`) reached while at least one frame is live is unsafe. This is version-
agnostic and handles `try` / `catch` inside loops, unlike `Core.Compiler.compute_trycatch`
whose shape and handler-region marking differ across the supported Julia versions.
"""
function assert_can_handle_control_flow(ir::BBCode)
    # Single pass to detect the start of a `try` (`IDEnterNode`) and whether a value flows out
    # of one (`UpsilonNode` / `PhiCNode`, which only arise from exception handling).
    has_enter = false
    has_value_carrying = false
    for blk in ir.blocks, inst in blk.insts
        stmt = inst.stmt
        stmt isa IDEnterNode && (has_enter = true)
        (stmt isa Core.UpsilonNode || stmt isa Core.PhiCNode) && (has_value_carrying = true)
    end

    # No exception-handling control flow at all (the common case).
    has_enter || return nothing

    # Before Julia 1.12 the compiler passes we run over the derived IR (constant propagation,
    # inlining, codegen) do not reliably handle exception-handling nodes -- depending on the
    # control-flow shape they either error or miscompile to a crash. Rather than risk that,
    # reject `try` / `catch` outright on those versions with a clear message. From 1.12 the
    # compiler handles these nodes and the safe subset below is supported.
    @static if VERSION < v"1.12-"
        throw(ArgumentError(_TRY_CATCH_VERSION_MSG))
    end

    # Not-yet-supported case: a value defined in a `try` / `catch` and used afterwards lowers
    # to `UpsilonNode` / `PhiCNode`, which the transformation does not yet handle.
    has_value_carrying && throw(ArgumentError(_VALUE_CARRYING_CATCH_MSG))

    # Unsafe case: a `produce` (or a call which might `produce`) which can run while an
    # exception-handler frame is live cannot be reproduced across a suspend/resume boundary.
    # Compute, by forward data-flow to a fixed point, the number of handler frames live on
    # entry to each block, then scan for an offending `produce`.
    handler_depth_in = _handler_depth_on_entry(ir)
    for blk in ir.blocks
        depth = handler_depth_in[blk.id]
        for inst in blk.insts
            stmt = inst.stmt
            if depth > 0 && (
                is_produce_stmt(stmt) ||
                stmt_might_produce(stmt, CC.widenconst(inst.type); assume_returns=true)
            )
                throw(ArgumentError(_PRODUCE_IN_HANDLER_MSG))
            end
            depth = _apply_handler_delta(depth, stmt)
        end
    end
    return nothing
end

# Number of exception-handler frames opened (`enter`) minus closed (`leave` /
# `:pop_exception`) by `stmt`, applied to a running `depth` (clamped at zero).
function _apply_handler_delta(depth::Int, @nospecialize(stmt))
    stmt isa IDEnterNode && return depth + 1
    if Meta.isexpr(stmt, :leave)
        # On Julia 1.12+ (the only versions on which this data-flow runs -- earlier versions
        # reject `try` / `catch` before reaching here) a `:leave` lists one enter token per
        # frame it closes, so the number of frames closed is the token count. The integer
        # form `Expr(:leave, n)` is the older lowering and is handled defensively only.
        n = if length(stmt.args) == 1 && stmt.args[1] isa Integer
            Int(stmt.args[1])
        else
            count(a -> a !== nothing, stmt.args)
        end
        return max(depth - n, 0)
    end
    Meta.isexpr(stmt, :pop_exception) && return max(depth - 1, 0)
    return depth
end

# Forward data-flow: the number of handler frames live on entry to each block. At a join we
# take the maximum over predecessors -- structured exception handling keeps the depth equal
# across merging paths, and the maximum is the conservative (never-under-count) choice.
function _handler_depth_on_entry(ir::BBCode)
    preds = compute_all_predecessors(ir)
    depth_in = Dict{ID,Int}(blk.id => 0 for blk in ir.blocks)
    depth_out = Dict{ID,Int}(blk.id => 0 for blk in ir.blocks)
    changed = true
    while changed
        changed = false
        for blk in ir.blocks
            new_in = isempty(preds[blk.id]) ? 0 : maximum(p -> depth_out[p], preds[blk.id])
            d = new_in
            for inst in blk.insts
                d = _apply_handler_delta(d, inst.stmt)
            end
            if new_in != depth_in[blk.id] || d != depth_out[blk.id]
                depth_in[blk.id] = new_in
                depth_out[blk.id] = d
                changed = true
            end
        end
    end
    return depth_in
end

const _PRODUCE_IN_HANDLER_MSG = """
    `TapedTask` does not support a `produce` inside a `try` / `catch` / `finally` block \
    (including a `produce` in the `try` body or in the `catch` before the block finishes). \
    A `produce` suspends the task, but the exception-handler frame set up by the `try` \
    cannot be restored when the task is resumed, so this cannot be reproduced faithfully. \
    Move the `produce` outside the `try` / `catch` block (for example, by capturing a value \
    inside the block and calling `produce` after it). See \
    https://github.com/TuringLang/Libtask.jl/issues/194 for details."""

const _VALUE_CARRYING_CATCH_MSG = """
    `TapedTask` does not yet support a `try` / `catch` / `finally` block that defines a \
    value used after the block (such code lowers to `UpsilonNode` / `PhiCNode` IR nodes). \
    `try` / `catch` blocks which do not feed a value into later code are supported. See \
    https://github.com/TuringLang/Libtask.jl/issues/194 for details."""

const _TRY_CATCH_VERSION_MSG = """
    `TapedTask` only supports functions containing `try` / `catch` / `finally` blocks on \
    Julia 1.12 or later; the Julia compiler does not reliably handle exception-handling IR \
    in the derived task on earlier versions. Upgrade to Julia 1.12+, or rewrite the function \
    to avoid `try` / `catch` (for example, by using explicit conditional checks). See \
    https://github.com/TuringLang/Libtask.jl/issues/194 for details."""

# `true` for statements which Julia's IR verifier treats as block-terminating but which
# `BBCode` does not model as `Terminator`s, namely `IDEnterNode` (the start of a `try`) and
# `Expr(:leave)` (the normal exit from a `try`). Both must remain the final statement of
# their block, falling through to the next block (an `enter` additionally branches to its
# catch block). We therefore neither append a `GotoNode` after them nor read their operands
# from `Ref`s.
function ends_block_implicitly(@nospecialize(stmt))
    return stmt isa IDEnterNode || Meta.isexpr(stmt, :leave)
end

function derive_copyable_task_ir(ir::BBCode)::Tuple{BBCode,Tuple,Vector{Any}}
    # The location from which all state can be retrieved. Since we're using `OpaqueClosure`s
    # to implement `TapedTask`s, this appears via the first argument.
    refs_id = Argument(1)

    # Increment all arguments by 1.
    for bb in ir.blocks, (n, inst) in enumerate(bb.insts)
        bb.insts[n] = CC.NewInstruction(
            inc_args(inst.stmt), inst.type, inst.info, inst.line, inst.flag
        )
    end

    # Construct map between SSA IDs and their index in the state data structure and back.
    # Also construct a map from each ref index to its type. We only construct `Ref`s
    # for statements which return a value e.g. `IDGotoIfNot`s do not have a meaningful
    # return value, so there's no need to allocate a `Ref` for them.
    ssa_id_to_ref_index_map = Dict{ID,Int}()
    ref_index_to_ssa_id_map = Dict{Int,ID}()
    ref_index_to_type_map = Dict{Int,Type}()
    id_to_type_map = Dict{ID,Type}()
    is_used_dict = characterise_used_ids(collect_stmts(ir))
    n = 0
    for bb in ir.blocks
        for (id, stmt) in zip(bb.inst_ids, bb.insts)
            id_to_type_map[id] = CC.widenconst(stmt.type)
            stmt.stmt isa IDGotoNode && continue
            stmt.stmt isa IDGotoIfNot && continue
            stmt.stmt === nothing && continue
            stmt.stmt isa ReturnNode && continue
            # The token produced by an `enter` identifies a live exception-handler frame. It
            # is meaningful only within a single execution of the closure (a `:leave` /
            # `:pop_exception` consuming it always runs in the same `consume` call), so it is
            # referenced directly as an SSA rather than round-tripped through a `Ref`.
            stmt.stmt isa IDEnterNode && continue
            is_used_dict[id] || continue
            n += 1
            ssa_id_to_ref_index_map[id] = n
            ref_index_to_ssa_id_map[n] = id
            ref_index_to_type_map[n] = CC.widenconst(stmt.type)
        end
    end

    # Specify data structure containing `Ref`s for all of the SSAs.
    _refs = Any[Ref{ref_index_to_type_map[p]}() for p in 1:length(ref_index_to_ssa_id_map)]

    # Ensure that each basic block ends with a non-producing statement. This is achieved by
    # replacing any fall-through terminators with `IDGotoNode`s. This is not strictly
    # necessary, but simplifies later stages of the pipeline, as discussed variously below.
    for (n, block) in enumerate(ir.blocks)
        if terminator(block) === nothing
            # A block ending in an `IDEnterNode` or `:leave` is left as-is: Julia's IR
            # verifier treats both as block-terminating, so they must remain the final
            # statement of their block (each with an implicit fall-through edge to the next
            # block; an `enter` additionally has its catch edge). We must not append a
            # `GotoNode` after them. Such a block always falls through to post-`try` code, so
            # it is never the structurally-final block; assert that so any future change which
            # broke the invariant fails loudly here rather than silently dropping the edge.
            if !isempty(block.insts) && ends_block_implicitly(block.insts[end].stmt)
                @assert n < length(ir.blocks) "`enter` / `:leave` in the final basic block"
                continue
            end
            # Fall-through terminator, so next block in `ir.blocks` is the unique successor
            # block of `block`. Final block cannot have a fall-through terminator, so asking
            # for element `n + 1` is always going to be valid.
            successor_id = ir.blocks[n + 1].id
            push!(block.insts, new_inst(IDGotoNode(successor_id)))
            push!(block.inst_ids, ID())
        end
    end

    # For each existing basic block, create a sequence of `NamedTuple`s which
    # define the manner in which it must be split.
    # A block will in general be split as follows:
    # 1 - %1 = φ(...)
    # 1 - %2 = φ(...)
    # 1 - %3 = call_which_must_not_produce(...)
    # 1 - %4 = produce(%3)
    # 2 - %5 = call_which_must_not_produce(...)
    # 2 - %6 = call_which_might_produce(...)
    # 3 - %7 = call_which_must_not_produce(...)
    # 3 - terminator (GotoIfNot, GotoNode, etc)
    #
    # The numbers on the left indicate which split each statement falls. The first
    # split comprises all statements up until the first produce / call-which-might-produce.
    # Consequently, the first split will always contain any `PhiNode`s present in the block.
    # The next set of statements up until the next produce / call-which-might-produce form
    # the second split, and so on.
    # We enforced above the condition that the final statement in a basic block must not
    # produce. This ensures that the final split does not produce. While not strictly
    # necessary, this simplifies the implementation (see below).
    #
    # As a result of the above, a basic block will be associated to exactly one split if it
    # does not contain any statements which may produce.
    #
    # Each `NamedTuple` contains a `start` index and `last` index, indicating the position
    # in the block at which the corresponding split starts and finishes.
    all_splits = map(ir.blocks) do block
        split_ends = vcat(
            findall(
                inst -> stmt_might_produce(inst.stmt, CC.widenconst(inst.type)),
                block.insts,
            ),
            length(block),
        )
        return map(enumerate(split_ends)) do (n, split_end)
            return (start=(n == 1 ? 0 : split_ends[n - 1]) + 1, last=split_end)
        end
    end

    # Owing to splitting blocks up, we will need to re-label some `GotoNode`s and
    # `GotoIfNot`s. To understand this, consider the following block, whose original `ID`
    # we assume to be `ID(old_id)`.
    # ID(new_id) - %1 = φ(ID(3) => ...)
    # ID(new_id) - %3 = call_which_must_not_produce(...)
    # ID(new_id) - %4 = produce(%3)
    # ID(old_id) - GotoNode(ID(5))
    #
    # In the above, the entire block was original associated to a single ID, `ID(old_id)`,
    # but is now split into two sections. We keep the original ID for the final split, and
    # assign a new one to the first split. As a result, any `PhiNode`s in other blocks
    # which have edges incoming from `ID(old_id)` will remain valid.
    # However, if we adopt this strategy for all blocks, `ID(5)` in the `GotoNode` at the
    # end of the block will refer to the wrong block if the block original associated to
    # `ID(5)` was itself split, since the "top" of that block will have a new `ID`.
    #
    # To resolve this, we:
    # 1. Associate an ID to each split in each block, ensuring that the ID for the final
    #   split of each block is the same ID as that of the original block.
    all_split_ids = map(zip(ir.blocks, all_splits)) do (block, splits)
        return vcat([ID() for _ in splits[1:(end - 1)]], block.id)
    end

    # 2. Construct a map between the ID of each block and the ID associated to its split.
    top_split_id_map = Dict{ID,ID}(b.id => x[1] for (b, x) in zip(ir.blocks, all_split_ids))

    # 3. Update all `GotoNode`s, `GotoIfNot`s, and `IDEnterNode` catch edges to refer to
    #   these new names. An `enter`'s catch edge points at the catch block, which must be
    #   re-pointed at that block's top split if it was split (a no-op when it was not).
    for block in ir.blocks
        t = terminator(block)
        if t isa IDGotoNode
            block.insts[end] = new_inst(IDGotoNode(top_split_id_map[t.label]))
        elseif t isa IDGotoIfNot
            block.insts[end] = new_inst(IDGotoIfNot(t.cond, top_split_id_map[t.dest]))
        end
        for (i, inst) in enumerate(block.insts)
            s = inst.stmt
            s isa IDEnterNode || continue
            # A scope-only `enter` (`catch_id === nothing`) has no catch edge to relabel.
            s.catch_id === nothing && continue
            new_enter = remake_enter(s, top_split_id_map[s.catch_id])
            block.insts[i] = new_inst(new_enter, inst.type)
        end
    end

    # A set of blocks from which we might wish to resume computation.
    resume_block_ids = Vector{ID}()

    # A list onto which we'll push the type of any statement which might produce.
    possible_produce_types = Any[]

    # This where most of the action happens.
    #
    # For each split of each block, we must
    # 1. translate all statements which accept any SSAs as arguments, or return a value,
    #   into statements which read in data from the `Ref`s containing the value associated
    #   to each SSA, and write the result to `Ref`s associated to the SSA of the line in
    #   question.
    # 2. add additional code at the end of the split to handle the possibility that the
    #   last statement produces (per the definition of the splits above). This applies to
    #   all splits except the last, which cannot produce by construction. Exactly what
    #   happens here depends on whether the last statement is a `produce` call, or a
    #   call-which-might-produce -- see below for specifics.
    #
    # This code transforms each block (and its splits) into a new collection of blocks.
    # Note that the total number of new blocks may be greater than the total number of
    # splits, because each split ending in a call-which-might-produce requires more than a
    # single block to implement the required resumption functionality.
    new_bblocks = map(zip(ir.blocks, all_splits, all_split_ids)) do (bb, splits, splits_ids)
        new_blocks = map(enumerate(splits)) do (n, split)
            # We'll push ID-NewInstruction pairs to this as we proceed through the split.
            inst_pairs = IDInstPair[]

            # PhiNodes:
            #
            # A single `PhiNode`
            #
            #   ID(%1) = φ(ID(#1) => 1, ID(#2) => ID(%n))
            #
            # sets `ID(%1)` to either `1` or whatever value is currently associated to
            # `ID(%n)`, depending upon whether the predecessor block was `ID(#1)` or
            # `ID(#2)`. Consequently, a single `PhiNode` can be transformed into something
            # along the lines of:
            #
            #   ID(%1) = φ(ID(#1) => 1, ID(#2) => TupleRef(ref_ind_for_ID(%n)))
            #   ID(%2) = deref_phi(refs, ID(%1))
            #            set_ref_at!(refs, ref_ind_for_ID(%1), ID(%2))
            #
            # where `deref_phi` retrieves the value in position `ref_ind_for_ID(%n)` if
            # ID(%1) is a `TupleRef`, and `1` otherwise, and `set_ref_at!` sets the `Ref`
            # at position `ref_ind_for_ID(%1)` to the value of `ID(%2)`. See the actual
            # implementations below.
            #
            # If we have multiple `PhiNode`s at the start of a block, we must run all of
            # them, then dereference all of their values, and finally write all of the
            # de-referenced values to the appropriate locations. This is because
            # a. we require all `PhiNode`s appear together at the top of a given basic
            #   block, and
            # b. the semantics of `PhiNode`s is that they are all "run" simultaneously. This
            #   only matters if one `PhiNode` in the block can refer to the value stored in
            #   the SSA associated to another. For example, something along the lines of:
            #
            #       ID(%1) = φ(ID(#1) => 1, ID(#2) => ID(%2))
            #       ID(%2) = φ(ID(#1) => 1, ID(#2) => 2)
            #
            #   (we leave it as an exercise for the reader to figure out why this particular
            #   semantic feature of `PhiNode`s is relevant in this specific case).
            #
            # So, in general, the code produced by this block will look roughly like
            #
            #   ID(%1) = φ(...)
            #   ID(%2) = φ(...)
            #   ID(%3) = φ(...)
            #   ID(%4) = deref_phi(refs, ID(%1))
            #   ID(%5) = deref_phi(refs, ID(%2))
            #   ID(%6) = deref_phi(refs, ID(%3))
            #            set_ref_at!(refs, ref_ind_for_ID(%1), ID(%4))
            #            set_ref_at!(refs, ref_ind_for_ID(%2), ID(%5))
            #            set_ref_at!(refs, ref_ind_for_ID(%3), ID(%6))
            if n == 1
                # Find all PhiNodes in the block -- will definitely be in this split.
                phi_inds = findall(x -> x.stmt isa IDPhiNode, bb.insts)

                # Replace SSA IDs with `TupleRef`s, and record these instructions.
                phi_ids = map(phi_inds) do n
                    phi = bb.insts[n].stmt
                    for i in eachindex(phi.values)
                        isassigned(phi.values, i) || continue
                        v = phi.values[i]
                        v isa ID || continue
                        phi.values[i] = TupleRef(ssa_id_to_ref_index_map[v])
                    end
                    phi_id = ID()
                    push!(inst_pairs, (phi_id, new_inst(phi, Any)))
                    return phi_id
                end

                # De-reference values associated to `IDPhiNode`s.
                deref_ids = map(phi_inds) do n
                    id = bb.inst_ids[n]
                    phi_id = phi_ids[n]
                    ref_ind = ssa_id_to_ref_index_map[id]
                    push!(
                        inst_pairs,
                        # The last argument, ref_index_to_type_map[ref_ind], is a
                        # performance optimisation. The idea is that we know the inferred
                        # type of the PhiNode from the original IR, and by passing it to
                        # deref_phi we can type annotate the element type of the Ref
                        # that it's being dereferenced, resulting in more concrete types
                        # in the generated IR.
                        (
                            id,
                            new_inst(
                                Expr(
                                    :call,
                                    deref_phi,
                                    refs_id,
                                    phi_id,
                                    ref_index_to_type_map[ref_ind],
                                ),
                            ),
                        ),
                    )
                    return id
                end

                # Update values stored in `Ref`s associated to `PhiNode`s.
                for n in phi_inds
                    ref_ind = ssa_id_to_ref_index_map[bb.inst_ids[n]]
                    expr = Expr(:call, set_ref_at!, refs_id, ref_ind, deref_ids[n])
                    push!(inst_pairs, (ID(), new_inst(expr)))
                end
            end

            # Statements which do not produce:
            #
            # Iterate every statement in the split other than the final one, replacing uses
            # of SSAs with de-referenced `Ref`s, and writing the results of statements to
            # the corresponding `Ref`s.
            _ids = view(bb.inst_ids, (split.start):(split.last - 1))
            _insts = view(bb.insts, (split.start):(split.last - 1))
            for (id, inst) in zip(_ids, _insts)
                stmt = inst.stmt
                if Meta.isexpr(stmt, :invoke) ||
                    Meta.isexpr(stmt, :call) ||
                    Meta.isexpr(stmt, :new) ||
                    Meta.isexpr(stmt, :foreigncall) ||
                    Meta.isexpr(stmt, :throw_undef_if_not)

                    # Find any `ID`s and replace them with calls to read whatever is stored
                    # in the `Ref`s that they are associated to.
                    for (n, arg) in enumerate(stmt.args)
                        arg isa ID || continue

                        new_id = ID()
                        ref_ind = ssa_id_to_ref_index_map[arg]
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (new_id, new_inst(expr)))
                        stmt.args[n] = new_id
                    end

                    # Push the target instruction to the list.
                    push!(inst_pairs, (id, inst))

                    # If we know it is not possible for this statement to contain any calls
                    # to produce, then simply write out the result to its `Ref`. If it is
                    # never used, then there is no need to store it.
                    if is_used_dict[id]
                        out_ind = ssa_id_to_ref_index_map[id]
                        set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, id)
                        push!(inst_pairs, (ID(), new_inst(set_ref)))
                    end
                elseif Meta.isexpr(stmt, :boundscheck)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :code_coverage_effect)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :gc_preserve_begin)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :gc_preserve_end)
                    push!(inst_pairs, (id, inst))
                elseif stmt isa Nothing
                    push!(inst_pairs, (id, inst))
                elseif stmt isa GlobalRef
                    ref_ind = ssa_id_to_ref_index_map[id]
                    # We can only use `stmt` as an argument to `set_ref_at!` if it is a
                    # `const` binding. If it's not const, then we need to generate a new SSA
                    # value for it.
                    set_ref_at_arg = if isconst(stmt)
                        stmt
                    else
                        new_id = ID()
                        push!(inst_pairs, (new_id, new_inst(stmt)))
                        new_id
                    end
                    expr = Expr(:call, set_ref_at!, refs_id, ref_ind, set_ref_at_arg)
                    push!(inst_pairs, (id, new_inst(expr)))
                elseif stmt isa Core.PiNode
                    if stmt.val isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.val]
                        val_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (val_id, new_inst(expr)))
                        push!(inst_pairs, (id, new_inst(Core.PiNode(val_id, stmt.typ))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                    set_ind = ssa_id_to_ref_index_map[id]
                    set_expr = Expr(:call, set_ref_at!, refs_id, set_ind, id)
                    push!(inst_pairs, (ID(), new_inst(set_expr)))
                elseif stmt isa IDPhiNode
                    # do nothing -- we've already handled any `PhiNode`s.
                elseif Meta.isexpr(stmt, :loopinfo)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :pop_exception)
                    # Pops the exception stack at the end of a `catch`. Its token argument is
                    # the SSA produced by the corresponding `enter`; it is left as a direct
                    # reference rather than read from a `Ref`. (An `IDEnterNode` and a
                    # `:leave` are always block-terminating, so they are handled in the
                    # terminator branch below rather than here.)
                    push!(inst_pairs, (id, inst))
                else
                    throw(error("Unhandled stmt $stmt of type $(typeof(stmt))"))
                end
            end

            # TODO: explain this better.
            new_blocks = BBlock[]

            # Produce and Terminators:
            #
            # Handle the last statement in the split.
            id = bb.inst_ids[split.last]
            inst = bb.insts[split.last]
            stmt = inst.stmt
            if n == length(splits)
                # This is the last split in the block, so it must end with a non-producing
                # terminator. We handle this in a similar way to the statements above.

                if stmt isa ReturnNode
                    # Reset the position counter to `-1`, so that if this function gets
                    # called again, execution starts from the beginning.
                    expr = Expr(:call, set_resume_block!, refs_id, Int32(-1))
                    push!(inst_pairs, (ID(), new_inst(expr)))
                    # If returning an SSA, it might be one whose value was restored from
                    # before. Therefore, grab it out of storage, rather than assuming that
                    # it is def-ed.
                    if isdefined(stmt, :val) && stmt.val isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.val]
                        val_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (val_id, new_inst(expr)))
                        push!(inst_pairs, (ID(), new_inst(ReturnNode(val_id))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                elseif stmt isa IDGotoIfNot
                    # If the condition is an SSA, it might be one whose value was restored
                    # from before. Therefore, grab it out of storage, rather than assuming
                    # that it is defined.
                    if stmt.cond isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.cond]
                        cond_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (cond_id, new_inst(expr)))
                        push!(inst_pairs, (ID(), new_inst(IDGotoIfNot(cond_id, stmt.dest))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                elseif stmt isa IDGotoNode
                    push!(inst_pairs, (id, inst))
                elseif stmt isa IDEnterNode
                    # The `enter` terminates its block with an implicit fall-through to the
                    # next block plus a catch edge (encoded in the `IDEnterNode` itself). It
                    # must remain the final statement of the block, so we emit it without
                    # appending a `GotoNode`; the fall-through is provided by block layout.
                    # If the enter carries a `scope` that is an SSA, restore it from storage.
                    if isdefined(stmt, :scope) && stmt.scope isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.scope]
                        scope_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (scope_id, new_inst(expr)))
                        push!(
                            inst_pairs,
                            (id, new_inst(IDEnterNode(stmt.catch_id, scope_id))),
                        )
                    else
                        push!(inst_pairs, (id, inst))
                    end
                elseif Meta.isexpr(stmt, :leave)
                    # The normal exit from a `try`. Like `enter`, it terminates its block (it
                    # must be the block's final statement) and falls through to the next block
                    # by layout, so we emit it without appending a `GotoNode`. Its token
                    # operand is the SSA produced by the corresponding `enter` and is left as a
                    # direct reference.
                    push!(inst_pairs, (id, inst))
                else
                    error("Unexpected terminator $stmt")
                end
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            elseif is_produce_stmt(stmt)
                # This is a statement of the form
                # %n = produce(arg)
                #
                # We transform this into
                # Libtask.set_resume_block!(refs_id, id_of_next_block)
                # return ProducedValue(arg)
                #
                # The point is to ensure that, next time that this `TapedTask` is called,
                # computation is resumed from the statement _after_ this produce statement,
                # and to return whatever this produce statement returns.

                # Log the result type of this statement.
                arg = stmt.args[Meta.isexpr(stmt, :invoke) ? 3 : 2]
                push!(possible_produce_types, get_type((ir.argtypes, id_to_type_map), arg))

                # When this TapedTask is next called, we should resume from the first
                # statement of the next split.
                resume_id = splits_ids[n + 1]
                push!(resume_block_ids, resume_id)

                # Insert statement to enforce correct resumption behaviour.
                resume_stmt = Expr(:call, set_resume_block!, refs_id, resume_id.id)
                push!(inst_pairs, (ID(), new_inst(resume_stmt)))

                # Insert statement to construct a `ProducedValue` from the value.
                # Could be that the produce references an SSA, in which case we need to
                # de-reference, rather than just return the thing.
                prod_val = produce_value(stmt)
                if prod_val isa ID
                    deref_id = ID()
                    ref_ind = ssa_id_to_ref_index_map[prod_val]
                    expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                    push!(inst_pairs, (deref_id, new_inst(expr)))
                    prod_val = deref_id
                end

                # Set the ref for this statement, as we would for any other call or invoke.
                # The TapedTask may need to read this ref when it resumes, if the return
                # value of `produce` is used within the original function.
                if is_used_dict[id]
                    out_ind = ssa_id_to_ref_index_map[id]
                    set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, prod_val)
                    push!(inst_pairs, (ID(), new_inst(set_ref)))
                end

                # Construct a `ProducedValue`.
                val_id = ID()
                push!(inst_pairs, (val_id, new_inst(Expr(:call, ProducedValue, prod_val))))

                # Insert statement to return the `ProducedValue`.
                push!(inst_pairs, (ID(), new_inst(ReturnNode(val_id))))

                # Construct a single new basic block from all of the inst-pairs.
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            else
                # The final statement is one which might produce, but is not itself a
                # `produce` statement. For example
                # y = f(x)
                #
                # becomes (morally speaking)
                # y = f(x)
                # if y isa ProducedValue
                #   set_resume_block!(refs_id, id_of_current_block)
                #   return y
                # end
                #
                # The point is to ensure that, if `f` "produces" (as indicated by `y` being
                # a `ProducedValue`) then the next time that this TapedTask is called, we
                # must resume from the call to `f`, as subsequent runs might also produce.
                # On the other hand, if anything other than a `ProducedValue` is returned,
                # we know that `f` has nothing else to produce, and execution can safely
                # continue to the next split.
                # In addition to the above, we must do the usual thing and ensure that any
                # ssas are read from storage, and write the result of this computation to
                # storage before continuing to the next instruction.
                #
                # You should look at the IR generated by a simple example in the test suite
                # which involves calls that might produce, in order to get a sense of what
                # the resulting code looks like prior to digging into the code below.

                # At present, we're not able to properly infer the values which might
                # potentially be produced by a call-which-might-produce. Consequently, we
                # have to assume they can produce anything.
                #
                # This `Any` only affects the return type of the function being derived
                # here. Importantly, it does not affect the type stability of subsequent
                # statements in this function. As a result, the impact ought to be
                # reasonably limited.
                push!(possible_produce_types, Any)

                # Create a new basic block from the existing statements, since all new
                # statement need to live in their own basic blocks.
                callable_block_id = ID()
                push!(inst_pairs, (ID(), new_inst(IDGotoNode(callable_block_id))))
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))

                # Derive TapedTask for this statement.
                (callable, callable_args) = if Meta.isexpr(stmt, :invoke)
                    sig = get_mi(stmt.args[1]).specTypes
                    v = Any[Any]
                    (LazyCallable{sig,callable_ret_type(sig, v)}(), stmt.args[2:end])
                elseif Meta.isexpr(stmt, :call)
                    (DynamicCallable(), stmt.args)
                else
                    display(stmt)
                    println()
                    error("unhandled statement which might produce $stmt")
                end

                # Find any `ID`s and replace them with calls to read whatever is stored
                # in the `Ref`s that they are associated to.
                callable_inst_pairs = IDInstPair[]
                for (n, arg) in enumerate(callable_args)
                    arg isa ID || continue

                    new_id = ID()
                    ref_ind = ssa_id_to_ref_index_map[arg]
                    expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                    push!(callable_inst_pairs, (new_id, new_inst(expr)))
                    callable_args[n] = new_id
                end

                # Allocate a slot in the _refs vector for this callable.
                push!(_refs, Ref(callable))
                callable_ind = length(_refs)

                # Retrieve the callable from the refs.
                callable_id = ID()
                callable_stmt = Expr(:call, get_ref_at, refs_id, callable_ind)
                push!(callable_inst_pairs, (callable_id, new_inst(callable_stmt)))

                # Call the callable.
                result_id = ID()
                result_stmt = Expr(:call, callable_id, callable_args...)
                push!(callable_inst_pairs, (result_id, new_inst(result_stmt)))

                # Determine whether this TapedTask has produced a not-a-`ProducedValue`.
                not_produced_id = ID()
                not_produced_stmt = Expr(:call, not_a_produced, result_id)
                push!(callable_inst_pairs, (not_produced_id, new_inst(not_produced_stmt)))

                # Go to a block which just returns the `ProducedValue`, if a
                # `ProducedValue` is returned, otherwise continue to the next split.
                is_produced_block_id = ID()
                is_not_produced_block_id = ID()
                switch = Switch(
                    Any[not_produced_id],
                    [is_produced_block_id],
                    is_not_produced_block_id,
                )
                push!(callable_inst_pairs, (ID(), new_inst(switch)))

                # Push the above statements onto a new block.
                push!(new_blocks, BBlock(callable_block_id, callable_inst_pairs))

                # Construct block which handles the case that we got a `ProducedValue`. If
                # this happens, it means that `callable` has more things to produce still.
                # This means that we need to call it again next time we enter this function.
                # To achieve this, we set the resume block to the `callable_block_id`,
                # and return the `ProducedValue` currently located in `result_id`.
                push!(resume_block_ids, callable_block_id)
                set_res = Expr(:call, set_resume_block!, refs_id, callable_block_id.id)
                return_id = ID()
                produced_block_inst_pairs = IDInstPair[
                    (ID(), new_inst(set_res)),
                    (return_id, new_inst(ReturnNode(result_id))),
                ]
                push!(new_blocks, BBlock(is_produced_block_id, produced_block_inst_pairs))

                # Construct block which handles the case that we did not get a
                # `ProducedValue`. In this case, we must first push the result to the `Ref`
                # associated to the call, and goto the next split.
                next_block_id = splits_ids[n + 1] # safe since the last split ends with a terminator
                if is_used_dict[id]
                    result_ref_ind = ssa_id_to_ref_index_map[id]
                    set_ref = Expr(:call, set_ref_at!, refs_id, result_ref_ind, result_id)
                else
                    set_ref = nothing
                end
                not_produced_block_inst_pairs = IDInstPair[
                    (ID(), new_inst(set_ref))
                    (ID(), new_inst(IDGotoNode(next_block_id)))
                ]
                push!(
                    new_blocks,
                    BBlock(is_not_produced_block_id, not_produced_block_inst_pairs),
                )
            end
            return new_blocks
        end
        return reduce(vcat, new_blocks)
    end
    new_bblocks = reduce(vcat, new_bblocks)

    # Insert statements at the top.
    cases = map(resume_block_ids) do id
        return ID(), id, Expr(:call, resume_block_is, refs_id, id.id)
    end
    cond_ids = ID[x[1] for x in cases]
    cond_dests = ID[x[2] for x in cases]
    cond_stmts = Any[x[3] for x in cases]
    switch_stmt = Switch(Any[x for x in cond_ids], cond_dests, first(new_bblocks).id)
    entry_stmts = vcat(cond_stmts, nothing, switch_stmt)
    entry_block = BBlock(ID(), vcat(cond_ids, ID(), ID()), map(new_inst, entry_stmts))
    new_bblocks = vcat(entry_block, new_bblocks)

    # New argtypes are the same as the old ones, except we have `Ref`s in the first argument
    # rather than nothing at all.
    new_argtypes = copy(ir.argtypes)
    refs = (_refs..., Ref{Int32}(-1))
    new_argtypes = vcat(typeof(refs), copy(ir.argtypes))

    # Return BBCode and the `Ref`s.
    @static if VERSION >= v"1.12-"
        new_ir = BBCode(
            new_bblocks, new_argtypes, ir.sptypes, ir.debuginfo, ir.meta, ir.valid_worlds
        )
    else
        new_ir = BBCode(new_bblocks, new_argtypes, ir.sptypes, ir.linetable, ir.meta)
    end
    return new_ir, refs, possible_produce_types
end

# Helper used in `derive_copyable_task_ir`.
@inline get_ref_at(refs::R, n::Int) where {R<:Tuple} = refs[n][]

# Helper used in `derive_copyable_task_ir`.
@inline function set_ref_at!(refs::R, n::Int, val) where {R<:Tuple}
    refs[n][] = val
    return nothing
end

# Helper used in `derive_copyable_task_ir`.
@inline function set_resume_block!(refs::R, id::Int32) where {R<:Tuple}
    refs[end][] = id
    return nothing
end

# Helper used in `derive_copyable_task_ir`.
@inline resume_block_is(refs::R, id::Int32) where {R<:Tuple} = !(refs[end][] === id)

# Helper used in `derive_copyable_task_ir`.
@inline function deref_phi(refs::R, n::TupleRef, ::Type{T}) where {R<:Tuple,T}
    ref = refs[n.n]
    return ref[]::T
end
@inline deref_phi(::R, x, t::Type) where {R<:Tuple} = x

# Helper used in `derived_copyable_task_ir`.
@inline not_a_produced(x) = !(isa(x, ProducedValue))

# Implement iterator interface.
function Base.iterate(t::TapedTask, state::Nothing=nothing)
    v = consume(t)
    return v === nothing ? nothing : (v, nothing)
end
Base.IteratorSize(::Type{<:TapedTask}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:TapedTask}) = Base.EltypeUnknown()

"""
    LazyCallable

Used to implement static dispatch, while avoiding the need to construct the
callable immediately. When constructed, just stores the signature of the
callable and its return type. Constructs the callable when first called.

All type information is known, so it is possible to make this callable type
stable provided that the return type is concrete.
"""
mutable struct LazyCallable{sig<:Tuple,Tret}
    mc::MistyClosure
    position::Base.RefValue{Int32}
    LazyCallable{sig,Tret}() where {sig,Tret} = new{sig,Tret}()
end

function (l::LazyCallable)(args::Vararg{Any,N}) where {N}
    isdefined(l, :mc) || construct_callable!(l)
    return l.mc(args...)
end

function construct_callable!(l::LazyCallable{sig}) where {sig}
    mc, pos = build_callable(sig)
    l.mc = mc
    l.position = pos
    return nothing
end

"""
    DynamicCallable

Like [`LazyCallable`](@ref), but without any type information. Used to implement
dynamic dispatch.
"""
mutable struct DynamicCallable{V}
    cache::V
end

DynamicCallable() = DynamicCallable(Dict{Any,Any}())

function (dynamic_callable::DynamicCallable)(args::Vararg{Any,N}) where {N}
    sig = _typeof(args)
    callable = get(dynamic_callable.cache, sig, nothing)
    if callable === nothing
        callable = build_callable(sig)
        dynamic_callable.cache[sig] = callable
    end
    return callable[1](args...)
end
