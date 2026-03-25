"""
    eliminate_refs(ir::BBCode, refs::Vector)

Transform the `BBCode` to remove redundant `get_ref_at` / `set_ref_at!` calls.

TODO: Explain more about how this happens

Returns a tuple of the modified `BBCode` and the modified `refs` vector.
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
    #
    # TODO: Handle phi nodes.
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
                    push!(def_i, inst.stmt.args[3])
                elseif call_func == Libtask.get_ref_at
                    if !(inst.stmt.args[3] in def_i)
                        push!(use_i, inst.stmt.args[3])
                    end
                elseif call_func == Libtask.set_resume_block!
                    return_block = inst.stmt.args[3]
                    # A return block of `-1` means the function is ending naturally, and not
                    # resuming again.
                    if return_block != -1
                        push!(resume_block_i, get_block_id_from_int(return_block))
                    end
                end
            elseif inst.stmt isa IDPhiNode
                # For a phi node, we might have wrapped a ref value inside a TupleRef. These
                # also count as uses of the ref, so we need to add those to the use set.
                for val in inst.stmt.values
                    if val isa Libtask.TupleRef
                        push!(use_i, val.n)
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
    # fixed-point iteration. This algorithm is lifted straight from Cooper & Torczon.
    changed = true
    live_out = Dict{ID,Set{Int}}(id => Set{Int}() for id in all_block_ids)
    while (changed)
        changed = false
        for i in all_block_ids
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
    # old_refid_to_new_refid_map = Dict{Int,Int}(
    #     necessary_ref_ids[i] => i for i in eachindex(necessary_ref_ids)
    # )

    # We now need to go through the IR and remove calls that get/set the unnecessary refs.
    new_bblocks = map(ir.blocks) do block
        new_insts = IDInstPair[]
        # Map, from ref numbers, to the SSA ID that contains the definition of the value
        # that would have been stored in that ref.
        old_refid_to_ssaid_map = Dict{Int,ID}()
        # Map, from SSA IDs that used to contain get_ref_at(refid) values, to the new SSA
        # IDs that contain the value itself.
        old_ssaid_to_new_ssaid_map = Dict{ID,ID}()

        for (id, inst) in zip(block.inst_ids, block.insts)
            if Meta.isexpr(inst.stmt, :call)
                call_func = inst.stmt.args[1]

                if call_func == Libtask.set_ref_at!
                    old_refid = inst.stmt.args[3]
                    if old_refid in unnecessary_ref_ids
                        # We can skip this instruction, but first we need to record which
                        # SSA ID contains the value that we would have set in this ref, so
                        # that if we encounter a get_ref_at, we can replace it with this
                        # value.
                        ssaid = inst.stmt.args[4]
                        # That value might itself be something that needs to be replaced.
                        ssaid = get(old_ssaid_to_new_ssaid_map, ssaid, ssaid)
                        old_refid_to_ssaid_map[old_refid] = inst.stmt.args[4]
                    else
                        # It's a set that we still need.
                        push!(new_insts, (id, inst))
                    end
                elseif call_func == Libtask.get_ref_at
                    old_refid = inst.stmt.args[3]
                    if old_refid in unnecessary_ref_ids
                        # Eliminate it entirely.
                        old_ssaid_to_new_ssaid_map[id] = old_refid_to_ssaid_map[old_refid]
                    else
                        # It's a get that we still need.
                        inst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                        push!(new_insts, (id, inst))
                    end
                else
                    # Some other call instruction.
                    inst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                    push!(new_insts, (id, inst))
                end
            else
                # Some other (non-call) instruction.
                inst = replace_ids(old_ssaid_to_new_ssaid_map, inst)
                push!(new_insts, (id, inst))
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
