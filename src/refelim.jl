"""
    eliminate_refs(ir::BBCode, refs::Tuple)

Transform the `BBCode` to remove redundant `get_ref_at` / `set_ref_at!` calls.

This optimises both within a single basic block, and across basic blocks. As an example of
the former, if we have something like

    %11 = set_ref_at!(_1, 1, %1)
    %12 = get_ref_at(_1, 1)
    %13 = f(%12)

then we can replace `%12` with `%1`, and eliminate the load entirely:

    %11 = set_ref_at!(_1, 1, %1)
    %13 = f(%1)

As for an example of optimising across basic blocks, consider a function like

    function f(x)
        a = x + 1
        b = a + 2
        produce(b)
        c = b * 3
        produce(c)
        return nothing
    end

Libtask's transformation pass recognises that `a`, `b`, and `c` all constitute the state of
the function, and creates `Ref`s to store all of those values, and to read/write from them
across produce boundaries. However, in this example, `a` is actually not needed after the
first produce, because it is only used to compute `b`. Thus, it is not necessary to retain
its state.

This function performs a classic live-variable analysis to identify which `Ref`s are
actually needed across boundaries, and eliminates all calls to `set_ref_at!` that don't need
to be retained.

Returns a tuple of the modified `BBCode` and the modified `refs` tuple.

!!! note
    Right now, `eliminate_refs` does not remove dead refs from the `refs` tuple itself (so the 
    TapedTask will be constructed with the same `refs` tuple as before). We simply leave those
    refs as unused (i.e., they will be initialised with nothing, and never read from or
    written to.) In principle, we could also slim down the `refs` tuple itself by removing
    the dead refs from it. This is left as a future optimisation (and the signature of this
    function is designed to allow for this in the future).
"""
function eliminate_refs(ir::BBCode, refs::Tuple)
    # The `refs` tuple contains a series of `Ref`s which are used to maintain function state
    # across produce boundaries. This mechanism is, in general, necessary as it allows
    # resumption of a function from a produce point with the correct state. However,
    # sometimes not all previously defined variables are used in the rest of the function.
    # This implies that it is not necessary to maintain the state of those variables across
    # produce points. 
    #
    # This function identifies which `Ref`s really do not need to be kept at all, and
    # removes them from the IR. The algorithm we use here is a classic 'live variable
    # analysis', see e.g., Chapter 8 of Cooper & Torczon 'Engineering a Compiler', 3rd ed.

    # Slightly faffy setup needed here, as we need a function that returns IDs given an
    # integer, but we can't call `ID(int)` to get the block ID from the integer; we need to
    # locate it from the set of existing IDs.
    all_block_ids = Set(block.id for block in ir.blocks)
    get_block_id_from_int(i::Integer)::ID =
        let
            matching_ids = filter(id -> id.id == i, all_block_ids)
            isempty(matching_ids) && error("No block ID found for integer $i")
            return only(matching_ids)
        end

    # Start by constructing, for each basic block `i`, the set of `Ref`s for which the value
    # is read in that basic block before being written to. Annoyingly, the literature is
    # quite inconsistent in its notation. Cooper & Torczon call this UEVar[i] (i.e.,
    # 'upward exposed variables'). Sometimes it's called USE[i], and on Wikipedia it's called
    # GEN[i].
    #
    # Furthermore, we also construct the set of `Ref`s for which the value is written to in
    # that basic block. This is `VarKill[i]` in C&T, or `DEF[i]`, or KILL[i] on Wikipedia.
    use = Dict{ID,Set{Int}}()
    def = Dict{ID,Set{Int}}()
    # In this pass through the IR, we also capture the resume blocks that Libtask sets in
    # each block. This is explained later on.
    set_resume_blocks = Dict{ID,Set{ID}}()
    for block in ir.blocks
        def_i = Set{Int}() # All variables defined in this block
        use_i = Set{Int}() # All variables used in this block before being defined in this block
        resume_block_i = Set{ID}()
        for inst in block.insts
            if Meta.isexpr(inst.stmt, :call)
                call_func = inst.stmt.args[1]
                if call_func == Libtask.set_ref_at!
                    ref_n = inst.stmt.args[3]
                    push!(def_i, ref_n)
                elseif call_func == Libtask.get_ref_at
                    ref_n = inst.stmt.args[3]
                    if !(ref_n in def_i)
                        push!(use_i, ref_n)
                    end
                elseif call_func == Libtask.set_resume_block!
                    return_block = inst.stmt.args[3]
                    # A return block of `-1` means the function is ending naturally, and not
                    # resuming again, so we don't need to add a synthetic edge.
                    if return_block != -1
                        push!(resume_block_i, get_block_id_from_int(return_block))
                    end
                end
            elseif inst.stmt isa IDPhiNode
                # We might have wrapped a ref value inside a TupleRef as one of the phi
                # node's values. These also count as uses of the ref, so we need to add
                # those to the use set.
                for i in eachindex(inst.stmt.values)
                    if isassigned(inst.stmt.values, i)
                        # We don't need to check def_i because phi nodes always occur at the
                        # start of a block, so they can't have been defined before this.
                        val = inst.stmt.values[i]
                        if val isa Libtask.TupleRef
                            push!(use_i, val.n)
                        end
                    end
                end
            end
        end
        use[block.id] = use_i
        def[block.id] = def_i
        set_resume_blocks[block.id] = resume_block_i
    end

    # Get a map of successors.
    successor_map = compute_all_successors(ir)
    # The tricky thing here is that although successor_map is *technically* correct from the
    # perspective of the IR, it doesn't capture the fact that we have synthetic edges from
    # one block to the other, mediated by calls to `set_resume_block!`. For example, if we
    # have something like
    #
    # #11 ─
    # │   %1 = ...
    # │   %2 = Libtask.set_ref_at!(_1, n, %1)
    # │   %3 = Libtask.set_resume_block!(_1, 12)
    # │   %4 = ...
    # └── return %4
    # #12 ─
    # │   %5 = Libtask.get_ref_at(_1, n)
    # │   %6 = ...
    # └── return %6
    #
    # then compute_all_successors will happily say that 12 is *not* a successor of 11,
    # because the function returns. However, it's obviously important here that the
    # set value of ref `n` is live in block 12, because when the function resumes it
    # can jump straight to 12.
    # To handle this we need to add synthetic edges from 11 to 12. We do this by parsing
    # the IR to find all calls to `set_resume_block!` (which explains the
    # `set_resume_blocks` dictionary we constructed above), and then combining that with
    # the successor map.
    successors = Dict(
        id => union(set_resume_blocks[id], Set(natural_successors)) for
        (id, natural_successors) in pairs(successor_map)
    )

    # Now we have all the information needed to run the live variable analysis, which is a
    # fixed-point iteration. This algorithm is lifted straight from Cooper & Torczon (figure
    # 8.15).
    changed = true
    live_out = Dict{ID,Set{Int}}(id => Set{Int}() for id in all_block_ids)
    # Inside the next loop, the 'correct' iteration order over blocks is reverse postorder
    # on the reverse CFG (see Cooper & Torczon's sections on efficiency of iterative
    # dataflow analysis). This does not affect correctness (there is a guaranteed unique
    # solution regardless of iteration order) but impacts how quickly we will converge to
    # the solution. Note that we need to pass the 'augmented' successors map constructed
    # above.
    ordered_block_ids = rpo_reverse_cfg(successors)
    while (changed)
        changed = false
        for i in ordered_block_ids
            # Recompute live_out
            live_out_i_new = Set{Int}()
            for succ_id in successors[i]
                live_in_succ = union(use[succ_id], setdiff(live_out[succ_id], def[succ_id]))
                live_out_i_new = union(live_out_i_new, live_in_succ)
            end
            # If it's changed from the previous one, then we need to run another iteration
            if live_out_i_new != live_out[i]
                changed = true
            end
            live_out[i] = live_out_i_new
        end
    end

    # Only the refs that are live at the end of some basic block anywhere in the function
    # need to be kept. Note that the last ref in `refs` is always mandatory: it's the one
    # that stores the return block (i.e., how far through the function it's progressed).
    necessary_ref_ids = sort!(collect(union(values(live_out)...)))
    unnecessary_ref_ids = setdiff(1:(length(refs) - 1), necessary_ref_ids)

    # TODO(penelopeysm): We could reduce the size of the ref tuple itself, by dropping refs
    # that are never used. I think this is not super important right now: it doesn't really
    # hurt to have extra refs lying around in the tuple, because they're just initialised to
    # essentially null pointers and never read/written to. But in principle we could get rid
    # of them too.
    #
    # new_refs = tuple(
    #     [ref for (i, ref) in enumerate(refs) if !(i in unnecessary_ref_ids)]...
    # )
    # refid_to_new_refid_map = Dict{Int,Int}(
    #     necessary_ref_ids[i] => i for i in eachindex(necessary_ref_ids)
    # )

    # We now need to go through the IR and remove calls that get/set the unnecessary refs.
    new_bblocks = map(ir.blocks) do block
        new_insts = IDInstPair[]
        # Map, from ref numbers, to the SSA ID that contains the definition of the value
        # that would have been stored in that ref.
        refid_to_ssaid_map = Dict{Int,ID}()
        # Map, from SSA IDs that used to contain get_ref_at(refid) values, to the new SSA
        # IDs that contain the value itself.
        old_ssaid_to_new_ssaid_map = Dict{ID,ID}()

        for (id, inst) in zip(block.inst_ids, block.insts)
            if Meta.isexpr(inst.stmt, :call)
                call_func = inst.stmt.args[1]

                if call_func == Libtask.set_ref_at!
                    refid = inst.stmt.args[3]
                    value_arg = inst.stmt.args[4]
                    if refid in unnecessary_ref_ids
                        # We can skip this instruction, but first we need to record which
                        # SSA ID contains the value that we would have set in this ref, so
                        # that if we encounter a get_ref_at, we can replace it with this
                        # value.
                        if value_arg isa ID
                            # That value might itself be something that needs to be
                            # replaced.
                            ssaid = get(old_ssaid_to_new_ssaid_map, value_arg, value_arg)
                            refid_to_ssaid_map[refid] = ssaid
                        elseif value_arg isa GlobalRef || value_arg isa Argument
                            # If it's a GlobalRef that's being stored in the ref, we just
                            # need to store the GlobalRef itself inside the SSA ID. In other
                            # words:
                            #     %1 = set_ref_at!(_1, refid, Main.a)
                            # can be replaced with
                            #     %1 = Main.a
                            refid_to_ssaid_map[refid] = id
                            push!(new_insts, (id, new_inst(value_arg)))
                        else
                            error("Unexpected value argument to set_ref_at!: $value_arg")
                        end
                    else
                        # It's a set that we still need. However, we additionally want to
                        # track the SSA ID that contains the value being set, so that if we
                        # encounter a get_ref_at in the same block, we can replace the
                        # get_ref_at with that value directly.
                        if value_arg isa ID
                            ssaid = get(old_ssaid_to_new_ssaid_map, value_arg, value_arg)
                            refid_to_ssaid_map[refid] = ssaid
                        elseif value_arg isa GlobalRef || value_arg isa Argument
                            # Create a new SSA ID that points to the GlobalRef.
                            new_id = ID()
                            push!(new_insts, (new_id, new_inst(value_arg)))
                            refid_to_ssaid_map[refid] = new_id
                        else
                            error("Unexpected value argument to set_ref_at!: $value_arg")
                        end
                        ninst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                        push!(new_insts, (id, ninst))
                    end
                elseif call_func == Libtask.get_ref_at
                    refid = inst.stmt.args[3]
                    if haskey(refid_to_ssaid_map, refid)
                        # If `refid` was found in the `refid_to_ssaid_map`, that means we
                        # have an SSA ID that contains the value that `get_ref_at` would
                        # have returned anyway. So, we can skip this instruction entirely.
                        #
                        # However, we need to record that the SSA ID of the current
                        # `get_ref_at` instruction should be replaced with that SSA ID, so
                        # that future instructions don't reference the current ID.
                        old_ssaid_to_new_ssaid_map[id] = refid_to_ssaid_map[refid]
                    else
                        # It's a get that we legitimately still need.
                        ninst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                        push!(new_insts, (id, ninst))
                    end
                else
                    # Some other call instruction.
                    ninst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                    push!(new_insts, (id, ninst))
                end
            else
                # Some other (non-call) instruction.
                ninst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                push!(new_insts, (id, ninst))
            end
        end
        return BBlock(block.id, new_insts)
    end

    new_ir = @static if VERSION >= v"1.12-"
        BBCode(new_bblocks, ir.argtypes, ir.sptypes, ir.debuginfo, ir.meta, ir.valid_worlds)
    else
        BBCode(new_bblocks, ir.argtypes, ir.sptypes, ir.linetable, ir.meta)
    end
    # return ir, refs
    return new_ir, refs
end

# Return a vector of block IDs in reverse postorder on the reverse CFG (i.e., the CFG where
# all edges are reversed). Postorder means that children are visited before their parents;
# reverse postorder just takes that order and reverses it. A good resource:
# https://eli.thegreenplace.net/2015/directed-graph-traversal-orderings-and-applications-to-data-flow-analysis/
function rpo_reverse_cfg(successor_map::Dict{ID,Set{ID}})
    # If block A has a successor (i.e., 'child') B in the original IR, then in the reverse
    # IR, block B will have a 'child' A. We can calculate this by inverting the successor
    # map. (Note that we can't use `compute_all_predecessors` here, because that only
    # computes the 'natural' predecessors, and doesn't capture the synthetic edges from
    # `set_resume_block!` calls.)
    predecessor_map = Dict{ID,Set{ID}}(id => Set{ID}() for id in keys(successor_map))
    for (id, succs) in pairs(successor_map)
        for succ in succs
            push!(predecessor_map[succ], id)
        end
    end
    all_ids = Set(keys(successor_map))
    exit_blocks = filter(id -> isempty(successor_map[id]), all_ids)
    all_other_blocks = setdiff(all_ids, exit_blocks)

    # This function constructs a postorder traversal of the reverse CFG
    order = ID[]
    visited = Set{ID}()
    function visit(id::ID)
        id in visited && return nothing
        push!(visited, id)
        for pred in predecessor_map[id]
            visit(pred)
        end
        return push!(order, id)
    end
    # Visit the exit blocks first, because they are the 'roots' of the reverse CFG
    for id in exit_blocks
        visit(id)
    end
    for id in all_other_blocks
        visit(id)
    end
    # Then reverse it to get RPO on the reverse CFG
    return reverse(order)
end
