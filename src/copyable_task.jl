const dynamic_scope = ScopedValue{Any}(0)

"""
    get_dynamic_scope()

Returns the dynamic scope associated to `Libtask`. If called from inside a `TapedTask`, this
will return whatever is contained in its `dynamic_scope` field.

See also [`set_dynamic_scope!`](@ref).
"""
get_dynamic_scope() = dynamic_scope[]

__v::Int = 5

"""
    produce(x)

When run inside a [`TapedTask`](@ref), will immediately yield to the caller, producing value
`x`.

See also: [`Libtask.consume`](@ref)
"""
@noinline function produce(x)
    global __v = 4 # silly side-effect to prevent this call getting constant-folded away. Should really use the effects system.
    return ProducedValue(x)
end

function callable_ret_type(sig)
    return Union{Base.code_ircode_by_type(sig)[1][2],ProducedValue}
end

function build_callable(sig::Type{<:Tuple})
    ir = Base.code_ircode_by_type(sig)[1][1]
    bb, refs = derive_copyable_task_ir(BBCode(ir))
    unoptimised_ir = IRCode(bb)
    optimised_ir = Mooncake.optimise_ir!(unoptimised_ir)
    mc_ret_type = callable_ret_type(sig)
    mc = Mooncake.misty_closure(mc_ret_type, optimised_ir, refs...; do_compile=true)
    return mc, refs[end]
end

mutable struct TapedTask{Tdynamic_scope,Targs,Tmc<:MistyClosure}
    dynamic_scope::Tdynamic_scope
    args::Targs
    const mc::Tmc
    const position::Base.RefValue{Int32}
end

"""
    TapedTask(dynamic_scope::Any, f, args...)

Construct a `TapedTask` with the specified `dynamic_scope`, for function `f` and positional
arguments `args`.

# Extended Help

There are three central features of a `TapedTask`, which we demonstrate via three examples.

## Resumption

The function [`Libtask.produce`](@ref) has a special meaning in Libtask. You can insert it
into regular Julia functions anywhere that you like. For example
```jldoctest tt
julia> function f()
           for t in 1:2
               produce(t)
               t += 1
           end
           return nothing
       end
f (generic function with 1 method)
```

If you construct a `TapedTask` from `f`, and call [`Libtask.consume`](@ref) on it, you'll
see
```jldoctest tt
julia> t = TapedTask(nothing, f);

julia> consume(t)
1
```
The semantics of this are that [`Libtask.consume`](@ref) runs the function `f` until it
reaches the call to [`Libtask.produce`](@ref), at which point it will return the argument
to [`Libtask.produce`](@ref).

Subsequent calls to [`Libtask.produce`](@ref) will _resume_ execution of `f` immediately
after the last [`Libtask.produce`](@ref) statement that was hit.
```jldoctest tt
julia> consume(t)
2
```

When there are no more [`Libtask.produce`](@ref) statements to hit, calling
[`Libtask.consume`](@ref) will return `nothing`:
```jldoctest tt
julia> consume(t)

```

## Copying

[`TapedTask`](@ref)s can be copied. Doing so creates a completely independent object.
For example:
```jldoctest tt
julia> t2 = TapedTask(nothing, f);

julia> consume(t2)
1
```

If we make a copy and advance its state, it produces the same value that the original would
have produced:
```jldoctest tt
julia> t3 = copy(t2);

julia> consume(t3)
2
```

Moreover, advancing the state of the copy has not advanced the state of the original,
because they are completely independent copies:
```jldoctest tt
julia> consume(t2)
2
```

## Scoped Values

It is often desirable to permit a copy of a task and the original to differ in very specific
ways. For example, in the context of Sequential Monte Carlo, you might want the only
difference between two copies to be their random number generator.

A generic mechanism is available to achieve this. [`Libtask.get_dynamic_scope`](@ref) and
[`Libtask.set_dynamic_scope!`](@ref) let you set and retrieve a variable which is specific
to a given [`Libtask.TapedTask`](@ref). The former can be called inside a function:
```jldoctest sv
julia> function f()
           produce(get_dynamic_scope())
           produce(get_dynamic_scope())
           return nothing
       end
f (generic function with 1 method)
```

The first argument to [`Libtask.TapedTask`](@ref) is the value that
[`Libtask.get_dynamic_scope`](@ref) will return:
```jldoctest sv
julia> t = TapedTask(1, f);

julia> consume(t)
1
```

The value that it returns can be changed between [`Libtask.consume`](@ref) calls:
```jldoctest sv
julia> set_dynamic_scope!(t, 2)

julia> consume(t)
2
```

`Int`s have been used here, but it is permissible to set the value returned by
[`Libtask.get_dynamic_scope`](@ref) to anything you like.
"""
function TapedTask(dynamic_scope::Any, fargs...)
    seed_id!()
    mc, count_ref = build_callable(typeof(fargs))
    return TapedTask(dynamic_scope, fargs[2:end], mc, count_ref)
end

"""
    set_dynamic_scope!(t::TapedTask, new_dynamic_scope)::Nothing

Set the `dynamic_scope` of `t` to `new_dynamic_scope`. Any references to 
`LibTask.dynamic_scope` in future calls to `consume(t)` (either directly, or implicitly via
iteration) will see this new value.

See also: [`get_dynamic_scope`](@ref).
"""
function set_dynamic_scope!(t::TapedTask{T}, new_dynamic_scope::T)::Nothing where {T}
    t.dynamic_scope = new_dynamic_scope
    return nothing
end

"""
    Base.copy(t::TapedTask)

Makes a completely independent copy of `t`. `consume` can be applied to either the copy of
`t` or the original without advancing the state of the other.
"""
Base.copy(t::T) where {T<:TapedTask} = deepcopy(t)

"""
    consume(t::TapedTask)

Run `t` until it makes a call to `produce`. If this is the first time that `t` has been
called, it start execution from the entry point. If `consume` has previously been called on
`t`, it will resume from the last `produce` call. If there are no more `produce` calls,
`nothing` will be returned.
"""
@inline function consume(t::TapedTask)
    v = with(() -> t.mc(t.args...), dynamic_scope => t.dynamic_scope)
    return v isa ProducedValue ? v[] : nothing
end

"""
    might_produce(sig::Type{<:Tuple})::Bool

`true` if a call to method with signature `sig` is permitted to contain
`Libtask.produce` statements.

This is an opt-in mechanism. the fallback method of this function returns `false` indicating
that, by default, we assume that calls do not contain `Libtask.produce` statements.
"""
might_produce(::Type{<:Tuple}) = false

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
    if Meta.isexpr(x, :invoke) && length(x.args) == 3
        return get_value(x.args[2]) === produce
    elseif Meta.isexpr(x, :call) && length(x.args) == 2
        return get_value(x.args[1]) === produce
    else
        return false
    end
end

"""
    stmt_might_produce(x)::Bool

`true` if `x` might contain a call to `produce`, and `false` otherwise.
"""
function stmt_might_produce(x)::Bool
    is_produce_stmt(x) && return true
    Meta.isexpr(x, :invoke) && return might_produce(x.args[1].specTypes)
    return false

    # # TODO: make this correct
    # Meta.isexpr(x, :call) &&
    #     return !isa(x.args[1], Union{Core.IntrinsicFunction,Core.Builtin})
    # Meta.isexpr(x, :invoke) && return false # todo: make this more accurate
    # return false
end

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

@inline Base.getindex(x::ProducedValue) = x.x

function derive_copyable_task_ir(ir::BBCode)::Tuple{BBCode,Tuple}

    # The location from which all state can be retrieved. Since we're using `OpaqueClosure`s
    # to implement `TapedTask`s, this appears via the first argument.
    refs_id = Argument(1)

    # Construct map between SSA IDs and their index in the state data structure and back.
    # Also construct a map from each ref index to its type. We only construct `Ref`s
    # for statements which return a value e.g. `IDGotoIfNot`s do not have a meaningful
    # return value, so there's no need to allocate a `Ref` for them.
    ssa_id_to_ref_index_map = Dict{ID,Int}()
    ref_index_to_ssa_id_map = Dict{Int,ID}()
    ref_index_to_type_map = Dict{Int,Type}()
    n = 0
    for bb in ir.blocks
        for (id, stmt) in zip(bb.inst_ids, bb.insts)
            stmt.stmt isa IDGotoNode && continue
            stmt.stmt isa IDGotoIfNot && continue
            stmt.stmt === nothing && continue
            stmt.stmt isa ReturnNode && continue
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
            # Fall-through terminator, so next block in `ir.blocks` is the unique successor
            # block of `block`. Final block cannot have a fall-through terminator, so asking
            # for element `n + 1` is always going to be valid.
            successor_id = ir.blocks[n + 1].id
            push!(block.insts, new_inst(IDGotoNode(successor_id)))
            push!(block.inst_ids, ID())
        end
    end

    # For each existing basic block, produce a sequence of `NamedTuple`s which
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
            findall(inst -> stmt_might_produce(inst.stmt), block.insts), length(block)
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

    # 3. Update all `GotoNode`s and `GotoIfNot`s to refer to these new names.
    for block in ir.blocks
        t = terminator(block)
        if t isa IDGotoNode
            block.insts[end] = new_inst(IDGotoNode(top_split_id_map[t.label]))
        elseif t isa IDGotoIfNot
            block.insts[end] = new_inst(IDGotoIfNot(t.cond, top_split_id_map[t.dest]))
        end
    end

    # A set of blocks from which we might wish to resume computation.
    resume_block_ids = Vector{ID}()

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
            inst_pairs = Mooncake.IDInstPair[]

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
            # where `deref_phi` retrives the value in position `ref_ind_for_ID(%n)` if
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
                    push!(
                        inst_pairs,
                        (id, new_inst(Expr(:call, deref_phi, refs_id, phi_id))),
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
                    Meta.isexpr(stmt, :foreigncall)

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
                    # to produce, then simply write out the result to its `Ref`.
                    out_ind = ssa_id_to_ref_index_map[id]
                    set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, id)
                    push!(inst_pairs, (ID(), new_inst(set_ref)))
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
                    expr = Expr(:call, set_ref_at!, refs_id, ref_ind, stmt)
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
                else
                    error("Unexpected terminator $stmt")
                end
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            elseif is_produce_stmt(stmt)

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

                # Construct a `ProducedValue`.
                val_id = ID()
                push!(inst_pairs, (val_id, new_inst(Expr(:call, ProducedValue, prod_val))))

                # Insert statement to return the `ProducedValue`.
                push!(inst_pairs, (ID(), new_inst(ReturnNode(val_id))))

                # Construct a single new basic block from all of the inst-pairs.
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            else
                # The final statement is one which might produce, but is not itself a
                # `produce` statement.

                # Create a new basic block from the existing statements, since all new
                # statement need to live in their own basic blocks.
                callable_block_id = ID()
                push!(inst_pairs, (ID(), new_inst(IDGotoNode(callable_block_id))))
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))

                # Derive TapedTask for this statement.
                callable = if Meta.isexpr(stmt, :invoke)
                    sig = stmt.args[1].specTypes
                    LazyCallable{sig,callable_ret_type(sig)}()
                else
                    error("unhandled statement which might produce $stmt")
                end

                # Allocate a slot in the _refs vector for this callable.
                push!(_refs, Ref(callable))
                callable_ind = length(_refs)

                # Retrieve the callable from the refs.
                callable_id = ID()
                callable = Expr(:call, get_ref_at, refs_id, callable_ind)

                # Call the callable.
                result = Expr(:call, callable_id, stmt.args[3:end]...)
                result_id = ID()

                # Determine whether this TapedTask has produced a not-a-`ProducedValue`.
                not_produced = Expr(:call, not_a_produced, result_id)
                not_produced_id = ID()

                # Go to a block which just returns the `ProducedValue`, if a
                # `ProducedValue` is returned, otherwise continue to the next split.
                is_produced_block_id = ID()
                next_block_id = splits_ids[n + 1] # safe since the last split ends with a terminator
                # switch = Switch(Any[not_produced_id], [is_produced_block_id], next_block_id)
                switch = IDGotoIfNot(not_produced_id, is_produced_block_id)

                # Insert a new block to hold the three previous statements.
                callable_inst_pairs = Mooncake.IDInstPair[
                    (callable_id, new_inst(callable)),
                    (result_id, new_inst(result)),
                    (not_produced_id, new_inst(not_produced)),
                    (ID(), new_inst(switch)),
                ]
                push!(new_blocks, BBlock(callable_block_id, callable_inst_pairs))

                goto_block = BBlock(ID(), [(ID(), new_inst(IDGotoNode(next_block_id)))])
                push!(new_blocks, goto_block)

                # Construct block which handles the case that we got a `ProducedValue`. If
                # this happens, it means that `callable` has more things to produce still.
                # This means that we need to call it again next time we enter this function.
                # To achieve this, we set the resume block to the `callable_block_id`,
                # and return the `ProducedValue` currently located in `result_id`.
                push!(resume_block_ids, callable_block_id)
                set_res = Expr(:call, set_resume_block!, refs_id, callable_block_id.id)
                return_id = ID()
                produced_block_inst_pairs = Mooncake.IDInstPair[
                    (ID(), new_inst(set_res)),
                    (return_id, new_inst(ReturnNode(result_id))),
                ]
                push!(new_blocks, BBlock(is_produced_block_id, produced_block_inst_pairs))
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
    new_argtypes[1] = typeof(refs)

    # Return BBCode and the `Ref`s.
    return BBCode(new_bblocks, new_argtypes, ir.sptypes, ir.linetable, ir.meta), refs
end

# Helper used in `derive_copyable_task_ir`.
@inline function get_ref_at(refs::R, n::Int) where {R<:Tuple}
    return refs[n][]
end

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
@inline deref_phi(refs::R, n::TupleRef) where {R<:Tuple} = refs[n.n][]
@inline deref_phi(::R, x) where {R<:Tuple} = x

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
