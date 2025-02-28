__v::Int = 5
@noinline function produce(x)
    global __v = 4
    return nothing
end

mutable struct TapedTask{Tmc<:MistyClosure,Targs}
    const mc::Tmc
    args::Targs
    const position::Base.RefValue{Int32}
    const deepcopy_types::Type
end

"""
    Base.copy(t::TapedTask)

Makes a copy of `t` which can be run. For the most part, calls to [`consume`](@ref) on the
copied task will give the same results as the original. There are, however, substantial
limitations to this, detailed in the extended help.

# Extended Help

We call a copy of a `TapedTask` _consistent_ with the original if the call to `==` in the
loop below always returns `true`:
```julia
t = <some_TapedTask>
tc = copy(t)
for (v, vc) in zip(t, tc)
    v == vc
end
```
(provided that `==` is implemented for all `v` that are produced). Convesely, we refer to a
copy as _inconsistent_ if this property doesn't hold. In order to ensure
consistency, we need to ensure that independent copies are made of anything which might be
mutated by the task or its copy during subsequent `consume` calls. Failure to do this can
cause problems if, for example, a task reads-to and writes-from some memory.
If we call `consume` on the original task, and then on a copy of it, any changes made by the
original will be visible to the copy, potentially causing its behaviour to differ. This can
manifest itself as a race condition if the task and its copies are run concurrently.

To understand a bit more about when a task is / is not consistent, we need to dig into the
rather specific semantics of `copy`. Calling `copy` on a `TapedTask` does the following:
1. `copy` the `position` field,
2. `map`s `_tape_copy` over the `args` field, and
3. `map`s `_tape_copy` over the all of the data closed over in the `OpaqueClosure` which
    implements the task (specifically the values _inside_ the `Ref`s) -- call these the
    `captures`. Except the last elements of this data, because this is `===` to the
    `position` field -- for this element we use the copy we made in step 1.

`_tape_copy` doesn't actually make a copy of the object at all if it is not either an
`Array`, a `Ref`, or an instance of one of the types listed in the task's `deepcopy_type`
field. If it is an instance of one of these types then `_tape_copy` just calls `deepcopy`.

This behaviour is plainly entirely acceptable if the argument to `_tape_copy` is a bits
type. For any `mutable struct`s which aren't flagged for `deepcopy`ing, we have an immediate
risk of inconsistency. Similarly, for any `struct` types which aren't bits types (e.g.
those which contain an `Array`, `Ref`, or some other `mutable struct` either directly as one
of their fields, or as a field of a field, etc), we have an inconsistency risk.

Furthermore, for anything which _is_ `deepcopy`ed we introduce inconsistency risks. If, for
example, two elements of the data closed over by the task alias one another, calling
`deepcopy` on them separately will cause the copies to _not_ alias one another.
The same thing can happen if one element is `deepcopy`ed and the other not. For example, if
we have both an `Array` `x` and `view(x, inds)` stored in separate elements of `captures`,
`x` will be `deepcopy`ed, while `view(x, inds)` will not. In the copy of `captures`, the
`view` will still be a view into the original `x`, not the `deepcopy`ed version. Again, this
introduces inconsistency.

Why do we have these semantics? We have them because Libtask has always had them, and at the
time of writing we're unsure whether AdvancedPS.jl, and by extension Turing.jl rely on this
behaviour.

What other options do we have? Simply calling `deepcopy` on a `TapedTask` works fine, and
should reliably result in consistent behaviour between a `TapedTask` and any copies of it.
This would, therefore, be a preferable implementation. We should try to determine whether
this is a viable option.
"""
function Base.copy(t::T) where {T<:TapedTask}
    captures = t.mc.oc.captures
    new_captures = map(Base.Fix2(_tape_copy, t.deepcopy_types), captures)
    new_position = new_captures[end] # baked in later on.
    new_args = map(Base.Fix2(_tape_copy, t.deepcopy_types), t.args)
    new_mc = Mooncake.replace_captures(t.mc, new_captures)
    return T(new_mc, new_args, new_position, t.deepcopy_types)
end

_tape_copy(v, deepcopy_types::Type) = v isa deepcopy_types ? deepcopy(v) : v

# Not sure that we need this in the new implementation.
_tape_copy(box::Core.Box, deepcopy_types::Type) = error("Found a box")

@inline consume(t::TapedTask) = t.mc(t.args...)

function initialise!(t::TapedTask, args::Vararg{Any,N})::Nothing where {N}
    t.position[] = -1
    t.args = args
    return nothing
end

function TapedTask(fargs...; deepcopy_types::Type=Union{})
    sig = typeof(fargs)
    mc, count_ref = build_callable(Base.code_ircode_by_type(sig)[1][1])
    return TapedTask(mc, fargs[2:end], count_ref, Union{deepcopy_types, Array, Ref})
end

function build_callable(ir::IRCode)
    seed_id!()
    bb, refs = derive_copyable_task_ir(BBCode(ir))
    ir = IRCode(bb)
    optimised_ir = Mooncake.optimise_ir!(ir)
    return MistyClosure(optimised_ir, refs...; do_compile=true), refs[end]
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
    produce_value(x::Expr)

Returns the value that a `produce` statement returns. For example, for the statment
`produce(%x)`, this function will return `%x`.
"""
function produce_value(x::Expr)
    is_produce_stmt(x) || throw(error("Not a produce statement. Please report this error."))
    Meta.isexpr(x, :invoke) && return x.args[3]
    return x.args[2] # must be a `:call` Expr.
end

function derive_copyable_task_ir(ir::BBCode)::Tuple{BBCode,Tuple}

    # Replace all existing `ReturnNode`s with `ReturnNode(nothing)` in order to provide the
    # same semantics as `Libtask`.
    for bb in ir.blocks
        for (n, inst) in enumerate(bb.insts)
            stmt = inst.stmt
            if stmt isa ReturnNode
                bb.insts[n] = new_inst(ReturnNode(nothing))
            end
        end
    end

    # The location at which `refs` will be stored.
    refs_id = Argument(1)

    # Mapping in which each key-value pairs says: "if we exited from block `key`, we must
    # resume by jumping to basic block `value`".
    resume_block_ids = Dict{ID,ID}()

    # For each basic block `bb`:
    # - count the number of produce statements, `n_produce`.
    # - construct `n_produce + 1` new basic blocks. The 1st new basic block runs from the
    #   first stmt in `bb` to the first `produce(%x)` statement (inclusive), the second
    #   from the next statement after the first `produce(%x)` statement until the next
    #   `produce(%x)` statement, etc. The final new basic block runs from the statement
    #   following the final `produce(%x)` statment, until the end of `bb`.
    #   Furthermore, each `produce(%x)` statement is replaced with a `ReturnNode(%x)`.
    #   We log the `ID`s of each of these new basic blocks, for use later.
    replacements = Dict{ID,ID}()
    new_bblocks = map(ir.blocks) do bb

        # If the final statement in the block is a `produce` statement, insert an additional
        # statement afterwards.
        if is_produce_stmt(bb.insts[end].stmt)
            push!(bb.inst_ids, ID())
            push!(bb.insts, new_inst(nothing, Nothing))
        end

        # Find all of the `produce` statements.
        produce_indices = findall(x -> is_produce_stmt(x.stmt), bb.insts)
        terminator_indices = vcat(produce_indices, length(bb))

        # The `ID`s of the new basic blocks.
        old_id = bb.id
        new_block_ids = vcat([ID() for _ in produce_indices], bb.id)
        new_id = first(new_block_ids)
        replacements[old_id] = new_id

        # Construct `n_produce + 1` new basic blocks. The last basic block retains the
        # `ID` of `bb`, the remaining `n_produce` blocks get new `ID`s (which we log).
        # All `produce(%x)` statements are replaced with `Return(%x)` statements.
        return map(enumerate(terminator_indices)) do (n, term_ind)

            # The last new block has the same `ID` as `bb`. The others gets new ones.
            block_id = new_block_ids[n]

            # Pull out the instructions and their `ID`s for the new block.
            start_ind = n == 1 ? 1 : terminator_indices[n - 1] + 1
            inst_ids = bb.inst_ids[start_ind:term_ind]
            insts = bb.insts[start_ind:term_ind]

            # If n < length(terminator_indices) then it must end with a `produce` statement.
            # In this case, we replace the `produce(%x)` statement with a call to set the
            # `resume_block` to the next block, which ensures that execution jumps to the
            # statement immediately following this `produce(%x)` statement next time the
            # function is called. We also insert a `ReturnNode(%x)` i.e. to implement the
            # `produce` statement.
            # Also log the mapping between the current new block ID, and the ID of the block
            # we should resume to.
            if n < length(terminator_indices)
                resume_id = new_block_ids[n + 1]
                resume_block_ids[block_id] = resume_id
                set_resume = Expr(:call, set_resume_block!, refs_id, resume_id.id)
                return_node = ReturnNode(produce_value(insts[end].stmt))
                inst_ids = vcat(inst_ids[1:(end - 1)], [ID(), ID()]) # actual ID values are irrelevant (no uses).
                insts = vcat(
                    insts[1:(end - 1)], [new_inst(set_resume), new_inst(return_node)]
                )
            end

            # Construct + return new basic block.
            return BBlock(block_id, inst_ids, insts)
        end
    end
    new_bblocks = reduce(vcat, new_bblocks)

    # Hunt for `IDGotoNode`s and `IDGotoIfNot`s, and replace them with the new ID of the
    # start of these blocks.
    for (old_id, new_id) in replacements, bb in new_bblocks
        inst = last(bb.insts)
        stmt = inst.stmt
        new_stmt = if stmt isa IDGotoNode && stmt.label == old_id
            IDGotoNode(new_id)
        elseif stmt isa IDGotoIfNot && stmt.dest == old_id
            IDGotoIfNot(stmt.cond, new_id)
        else
            continue
        end
        bb.insts[end] = CC.NewInstruction(
            new_stmt, inst.type, inst.info, inst.line, inst.flag
        )
    end

    # Construct map between SSA IDs and their index in the state data structure and back.
    # Optimisation TODO: don't create an entry for literally every line in the IR, just the
    # ones which produce values that might be needed later.
    ssa_id_to_ref_index_map = Dict{ID,Int}()
    ref_index_to_ssa_id_map = Dict{Int,ID}()
    ref_index_to_type_map = Dict{Int,Type}()
    n = 0
    for bb in new_bblocks
        for (id, stmt) in zip(bb.inst_ids, bb.insts)
            stmt.stmt isa IDGotoNode && continue
            stmt.stmt isa IDGotoIfNot && continue
            stmt.stmt === nothing && continue
            stmt.stmt isa ReturnNode && continue
            n += 1
            ssa_id_to_ref_index_map[id] = n
            ref_index_to_ssa_id_map[n] = id
            ref_index_to_type_map[n] = stmt.type
        end
    end

    # Specify data structure containing `Ref`s for all of the SSAs.
    # Optimisation TODO: permit users to construct custom data structures to make their
    # lives involve less indirection.
    # Optimisation TODO: make there be only one `Ref` per basic block, and only write to it
    # at the end of basic block execution (or something like that). Probably need to base
    # this on what the basic blocks _will_ _be_ after we've transformed everything, so need
    # to figure out when this can happen.
    _refs = map(p -> Ref{ref_index_to_type_map[p]}(), 1:length(ref_index_to_ssa_id_map))
    refs = (_refs..., Ref{Int32}(-1))

    # For each instruction in each basic block, replace it with a call to the refs.
    new_bblocks = map(new_bblocks) do bb
        inst_pairs = Mooncake.IDInstPair[]

        #
        # Handle all other nodes in the block.
        #

        foreach(zip(bb.inst_ids, bb.insts)) do (id, inst)
            stmt = inst.stmt
            if Meta.isexpr(stmt, :invoke) ||
                Meta.isexpr(stmt, :call) ||
                Meta.isexpr(stmt, :new) ||
                Meta.isexpr(stmt, :foreigncall)

                # Skip over set_resume_block! statements inserted in the previous pass.
                if stmt.args[1] == set_resume_block!
                    push!(inst_pairs, (id, inst))
                    return nothing
                end

                # Find any `ID`s and replace them with calls to read whatever is stored in
                # the `Ref`s that they are associated to.
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

                # Push the result to its `Ref`.
                out_ind = ssa_id_to_ref_index_map[id]
                set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, id)
                push!(inst_pairs, (ID(), new_inst(set_ref)))
            elseif Meta.isexpr(stmt, :boundscheck)
                push!(inst_pairs, (id, inst))
            elseif Meta.isexpr(stmt, :code_coverage_effect)
                push!(inst_pairs, (id, inst))
            elseif stmt isa ReturnNode
                # If returning an SSA, it might be one whose value was restored from before.
                # Therefore, grab it out of storage, rather than assuming that it is def-ed.
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
                # If the condition is an SSA, it might be one whose value was restored from
                # before. Therefore, grab it out of storage, rather than assuming that it is
                # defined.
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
            elseif stmt isa IDPhiNode
                # we'll fix up the PhiNodes after this, so identity transform for now.
                push!(inst_pairs, (id, inst))
            elseif stmt isa Nothing
                push!(inst_pairs, (id, inst))
            else
                throw(error("Unhandled stmt $stmt"))
            end
        end

        #
        # Handle `(ID)PhiNode`s.
        #

        phi_inds = findall(x -> x.stmt isa IDPhiNode, bb.insts)
        phi_inst_pairs = Mooncake.IDInstPair[]

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
            push!(phi_inst_pairs, (phi_id, new_inst(phi, Any)))
            return phi_id
        end

        # De-reference values associated to `IDPhiNode`s.
        deref_ids = map(phi_inds) do n
            id = bb.inst_ids[n]
            phi_id = phi_ids[n]
            push!(phi_inst_pairs, (id, new_inst(Expr(:call, deref_phi, refs_id, phi_id))))
            return id
        end

        # Update values stored in `Ref`s associated to `PhiNode`s.
        for n in phi_inds
            ref_ind = ssa_id_to_ref_index_map[bb.inst_ids[n]]
            expr = Expr(:call, set_ref_at!, refs_id, ref_ind, deref_ids[n])
            push!(phi_inst_pairs, (ID(), new_inst(expr)))
        end

        # Concatenate new phi stmts, removing old ones.
        inst_pairs = vcat(phi_inst_pairs, inst_pairs[(length(phi_inds) + 1):end])

        return BBlock(bb.id, inst_pairs)
    end

    # Insert statements at the top.
    cases = map(collect(resume_block_ids)) do (pred, succ)
        return ID(), succ, Expr(:call, resume_block_is, refs_id, succ.id)
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
    new_argtypes[1] = typeof(refs)

    # Return BBCode and the `Ref`s.
    return BBCode(new_bblocks, new_argtypes, ir.sptypes, ir.linetable, ir.meta), refs
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
@inline deref_phi(refs::R, n::TupleRef) where {R<:Tuple} = refs[n.n][]
@inline deref_phi(::R, x) where {R<:Tuple} = x

# Implement iterator interface.
function Base.iterate(t::TapedTask, state::Nothing=nothing)
    v = consume(t)
    return v === nothing ? nothing : (v, nothing)
end
Base.IteratorSize(::Type{<:TapedTask}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:TapedTask}) = Base.EltypeUnknown()
