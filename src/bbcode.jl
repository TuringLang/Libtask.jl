"""
    module BasicBlockCode

Copied over from Mooncake.jl in order to avoid making this package depend on Mooncake.
Refer to Mooncake's developer docs for context on this file.
"""
module BasicBlockCode

using Core.Compiler:
    ReturnNode,
    PhiNode,
    GotoIfNot,
    GotoNode,
    NewInstruction,
    IRCode,
    SSAValue,
    PiNode,
    Argument
const CC = Core.Compiler

export ID,
    seed_id!,
    IDPhiNode,
    IDGotoNode,
    IDGotoIfNot,
    Switch,
    BBlock,
    phi_nodes,
    terminator,
    insert_before_terminator!,
    collect_stmts,
    compute_all_predecessors,
    BBCode,
    characterise_used_ids,
    characterise_unique_predecessor_blocks,
    InstVector,
    IDInstPair,
    __line_numbers_to_block_numbers!,
    is_reachable_return_node,
    new_inst

const _id_count::Dict{Int,Int32} = Dict{Int,Int32}()

function new_inst(@nospecialize(stmt), @nospecialize(type)=Any, flag=CC.IR_FLAG_REFINED)
    return NewInstruction(stmt, type, CC.NoCallInfo(), Int32(1), flag)
end

const InstVector = Vector{NewInstruction}

struct ID
    id::Int32
    function ID()
        current_thread_id = Threads.threadid()
        id_count = get(_id_count, current_thread_id, Int32(0))
        _id_count[current_thread_id] = id_count + Int32(1)
        return new(id_count)
    end
end

Base.copy(id::ID) = id

function seed_id!()
    return global _id_count[Threads.threadid()] = 0
end

struct IDPhiNode
    edges::Vector{ID}
    values::Vector{Any}
end

Base.:(==)(x::IDPhiNode, y::IDPhiNode) = x.edges == y.edges && x.values == y.values

Base.copy(node::IDPhiNode) = IDPhiNode(copy(node.edges), copy(node.values))

struct IDGotoNode
    label::ID
end

Base.copy(node::IDGotoNode) = IDGotoNode(copy(node.label))

struct IDGotoIfNot
    cond::Any
    dest::ID
end

Base.copy(node::IDGotoIfNot) = IDGotoIfNot(copy(node.cond), copy(node.dest))

struct Switch
    conds::Vector{Any}
    dests::Vector{ID}
    fallthrough_dest::ID
    function Switch(conds::Vector{Any}, dests::Vector{ID}, fallthrough_dest::ID)
        @assert length(conds) == length(dests)
        return new(conds, dests, fallthrough_dest)
    end
end

const Terminator = Union{Switch,IDGotoIfNot,IDGotoNode,ReturnNode}

mutable struct BBlock
    id::ID
    inst_ids::Vector{ID}
    insts::InstVector
    function BBlock(id::ID, inst_ids::Vector{ID}, insts::InstVector)
        @assert length(inst_ids) == length(insts)
        return new(id, inst_ids, insts)
    end
end

const IDInstPair = Tuple{ID,NewInstruction}

function BBlock(id::ID, inst_pairs::Vector{IDInstPair})
    return BBlock(id, first.(inst_pairs), last.(inst_pairs))
end

Base.length(bb::BBlock) = length(bb.inst_ids)

Base.copy(bb::BBlock) = BBlock(bb.id, copy(bb.inst_ids), copy(bb.insts))

function phi_nodes(bb::BBlock)
    n_phi_nodes = findlast(x -> x.stmt isa IDPhiNode, bb.insts)
    if n_phi_nodes === nothing
        n_phi_nodes = 0
    end
    return bb.inst_ids[1:n_phi_nodes], bb.insts[1:n_phi_nodes]
end

function Base.insert!(bb::BBlock, n::Int, id::ID, inst::NewInstruction)::Nothing
    insert!(bb.inst_ids, n, id)
    insert!(bb.insts, n, inst)
    return nothing
end

terminator(bb::BBlock) = isa(bb.insts[end].stmt, Terminator) ? bb.insts[end].stmt : nothing

function insert_before_terminator!(bb::BBlock, id::ID, inst::NewInstruction)::Nothing
    insert!(bb, length(bb.insts) + (terminator(bb) === nothing ? 1 : 0), id, inst)
    return nothing
end

collect_stmts(bb::BBlock)::Vector{IDInstPair} = collect(zip(bb.inst_ids, bb.insts))

@static if VERSION >= v"1.12-"
    struct BBCode
        blocks::Vector{BBlock}
        argtypes::Vector{Any}
        sptypes::Vector{CC.VarState}
        debuginfo::CC.DebugInfoStream
        meta::Vector{Expr}
        valid_worlds::CC.WorldRange
    end

    function BBCode(ir::Union{IRCode,BBCode}, new_blocks::Vector{BBlock})
        return BBCode(
            new_blocks,
            CC.copy(ir.argtypes),
            CC.copy(ir.sptypes),
            CC.copy(ir.debuginfo),
            CC.copy(ir.meta),
            ir.valid_worlds,
        )
    end
else
    struct BBCode
        blocks::Vector{BBlock}
        argtypes::Vector{Any}
        sptypes::Vector{CC.VarState}
        linetable::Vector{Core.LineInfoNode}
        meta::Vector{Expr}
    end

    function BBCode(ir::Union{IRCode,BBCode}, new_blocks::Vector{BBlock})
        return BBCode(
            new_blocks,
            CC.copy(ir.argtypes),
            CC.copy(ir.sptypes),
            CC.copy(ir.linetable),
            CC.copy(ir.meta),
        )
    end
end

# Makes use of the above outer constructor for `BBCode`.
Base.copy(ir::BBCode) = BBCode(ir, copy(ir.blocks))

compute_all_successors(ir::BBCode)::Dict{ID,Vector{ID}} = _compute_all_successors(ir.blocks)

@noinline function _compute_all_successors(blks::Vector{BBlock})::Dict{ID,Vector{ID}}
    succs = map(enumerate(blks)) do (n, blk)
        is_final_block = n == length(blks)
        t = terminator(blk)
        if t === nothing
            return is_final_block ? ID[] : ID[blks[n + 1].id]
        elseif t isa IDGotoNode
            return [t.label]
        elseif t isa IDGotoIfNot
            return is_final_block ? ID[t.dest] : ID[t.dest, blks[n + 1].id]
        elseif t isa ReturnNode
            return ID[]
        elseif t isa Switch
            return vcat(t.dests, t.fallthrough_dest)
        else
            error("Unhandled terminator $t")
        end
    end
    return Dict{ID,Vector{ID}}((b.id, succ) for (b, succ) in zip(blks, succs))
end

function compute_all_predecessors(ir::BBCode)::Dict{ID,Vector{ID}}
    return _compute_all_predecessors(ir.blocks)
end

function _compute_all_predecessors(blks::Vector{BBlock})::Dict{ID,Vector{ID}}
    successor_map = _compute_all_successors(blks)

    # Initialise predecessor map to be empty.
    ks = collect(keys(successor_map))
    predecessor_map = Dict{ID,Vector{ID}}(zip(ks, map(_ -> ID[], ks)))

    # Find all predecessors by iterating through the successor map.
    for (k, succs) in successor_map
        for succ in succs
            push!(predecessor_map[succ], k)
        end
    end

    return predecessor_map
end

collect_stmts(ir::BBCode)::Vector{IDInstPair} = reduce(vcat, map(collect_stmts, ir.blocks))

function id_to_line_map(ir::BBCode)
    lines = collect_stmts(ir)
    lines_and_line_numbers = collect(zip(lines, eachindex(lines)))
    ids_and_line_numbers = map(x -> (x[1][1], x[2]), lines_and_line_numbers)
    return Dict(ids_and_line_numbers)
end

concatenate_ids(bb_code::BBCode) = reduce(vcat, map(b -> b.inst_ids, bb_code.blocks))
concatenate_stmts(bb_code::BBCode) = reduce(vcat, map(b -> b.insts, bb_code.blocks))

control_flow_graph(bb_code::BBCode)::Core.Compiler.CFG = _control_flow_graph(bb_code.blocks)

function _control_flow_graph(blks::Vector{BBlock})::Core.Compiler.CFG

    # Get IDs of predecessors and successors.
    preds_ids = _compute_all_predecessors(blks)
    succs_ids = _compute_all_successors(blks)

    # Construct map from block ID to block number.
    block_ids = map(b -> b.id, blks)
    id_to_num = Dict{ID,Int}(zip(block_ids, collect(eachindex(block_ids))))

    # Convert predecessor and successor IDs to numbers.
    preds = map(id -> sort(map(p -> id_to_num[p], preds_ids[id])), block_ids)
    succs = map(id -> sort(map(s -> id_to_num[s], succs_ids[id])), block_ids)

    # Predecessor of entry block is `0`. This needs to be added in manually.
    @static if VERSION >= v"1.11.6"
        push!(preds[1], 0)
    end

    # Compute the statement numbers associated to each basic block.
    index = vcat(0, cumsum(map(length, blks))) .+ 1
    basic_blocks = map(eachindex(blks)) do n
        stmt_range = Core.Compiler.StmtRange(index[n], index[n + 1] - 1)
        return Core.Compiler.BasicBlock(stmt_range, preds[n], succs[n])
    end
    return Core.Compiler.CFG(basic_blocks, index[2:(end - 1)])
end

function _lines_to_blocks(insts::InstVector, cfg::CC.CFG)::InstVector
    stmts = __line_numbers_to_block_numbers!(Any[x.stmt for x in insts], cfg)
    return map((inst, stmt) -> NewInstruction(inst; stmt), insts, stmts)
end

function __line_numbers_to_block_numbers!(insts::Vector{Any}, cfg::CC.CFG)
    for i in eachindex(insts)
        stmt = insts[i]
        if isa(stmt, GotoNode)
            insts[i] = GotoNode(CC.block_for_inst(cfg, stmt.label))
        elseif isa(stmt, GotoIfNot)
            insts[i] = GotoIfNot(stmt.cond, CC.block_for_inst(cfg, stmt.dest))
        elseif isa(stmt, PhiNode)
            insts[i] = PhiNode(
                Int32[CC.block_for_inst(cfg, Int(edge)) for edge in stmt.edges], stmt.values
            )
        elseif Meta.isexpr(stmt, :enter)
            stmt.args[1] = CC.block_for_inst(cfg, stmt.args[1]::Int)
            insts[i] = stmt
        end
    end
    return insts
end

#
# Converting from IRCode to BBCode
#

function BBCode(ir::IRCode)

    # Produce a new set of statements with `IDs` rather than `SSAValues` and block numbers.
    insts = new_inst_vec(ir.stmts)
    ssa_ids, stmts = _ssas_to_ids(insts)
    block_ids, stmts = _block_nums_to_ids(stmts, ir.cfg)

    # Chop up the new statements into `BBlocks`, according to the `CFG` in `ir`.
    blocks = map(zip(ir.cfg.blocks, block_ids)) do (bb, id)
        return BBlock(id, ssa_ids[bb.stmts], stmts[bb.stmts])
    end
    return BBCode(ir, blocks)
end

function new_inst_vec(x::CC.InstructionStream)
    stmt = @static VERSION < v"1.11.0-rc4" ? x.inst : x.stmt
    return map((v...,) -> NewInstruction(v...), stmt, x.type, x.info, x.line, x.flag)
end

# Maps from positional names (SSAValues for nodes, Integers for basic blocks) to IDs.
const SSAToIdDict = Dict{SSAValue,ID}
const BlockNumToIdDict = Dict{Integer,ID}

function _ssas_to_ids(insts::InstVector)::Tuple{Vector{ID},InstVector}
    ids = map(_ -> ID(), insts)
    val_id_map = SSAToIdDict(zip(SSAValue.(eachindex(insts)), ids))
    return ids, map(Base.Fix1(_ssa_to_ids, val_id_map), insts)
end

function _ssa_to_ids(d::SSAToIdDict, inst::NewInstruction)
    return NewInstruction(inst; stmt=_ssa_to_ids(d, inst.stmt))
end
function _ssa_to_ids(d::SSAToIdDict, x::ReturnNode)
    return isdefined(x, :val) ? ReturnNode(get(d, x.val, x.val)) : x
end
_ssa_to_ids(d::SSAToIdDict, x::Expr) = Expr(x.head, map(a -> get(d, a, a), x.args)...)
_ssa_to_ids(d::SSAToIdDict, x::PiNode) = PiNode(get(d, x.val, x.val), get(d, x.typ, x.typ))
_ssa_to_ids(d::SSAToIdDict, x::QuoteNode) = x
_ssa_to_ids(d::SSAToIdDict, x) = x
function _ssa_to_ids(d::SSAToIdDict, x::PhiNode)
    new_values = Vector{Any}(undef, length(x.values))
    for n in eachindex(x.values)
        if isassigned(x.values, n)
            new_values[n] = get(d, x.values[n], x.values[n])
        end
    end
    return PhiNode(x.edges, new_values)
end
_ssa_to_ids(d::SSAToIdDict, x::GotoNode) = x
_ssa_to_ids(d::SSAToIdDict, x::GotoIfNot) = GotoIfNot(get(d, x.cond, x.cond), x.dest)

function _block_nums_to_ids(insts::InstVector, cfg::CC.CFG)::Tuple{Vector{ID},InstVector}
    ids = map(_ -> ID(), cfg.blocks)
    block_num_id_map = BlockNumToIdDict(zip(eachindex(cfg.blocks), ids))
    return ids, map(Base.Fix1(_block_num_to_ids, block_num_id_map), insts)
end

function _block_num_to_ids(d::BlockNumToIdDict, x::NewInstruction)
    return NewInstruction(x; stmt=_block_num_to_ids(d, x.stmt))
end
function _block_num_to_ids(d::BlockNumToIdDict, x::PhiNode)
    return IDPhiNode(ID[d[e] for e in x.edges], x.values)
end
_block_num_to_ids(d::BlockNumToIdDict, x::GotoNode) = IDGotoNode(d[x.label])
_block_num_to_ids(d::BlockNumToIdDict, x::GotoIfNot) = IDGotoIfNot(x.cond, d[x.dest])
_block_num_to_ids(d::BlockNumToIdDict, x) = x

#
# Converting from BBCode to IRCode
#

function CC.IRCode(bb_code::BBCode)
    bb_code = _lower_switch_statements(bb_code)
    bb_code = _remove_double_edges(bb_code)
    insts = _ids_to_line_numbers(bb_code)
    cfg = control_flow_graph(bb_code)
    insts = _lines_to_blocks(insts, cfg)
    @static if VERSION >= v"1.12-"
        # See e.g. here for how the NTuple{3,Int}s get flattened for InstructionStream:
        # https://github.com/JuliaLang/julia/blob/16a2bf0a3b106b03dda23b8c9478aab90ffda5e1/Compiler/src/ssair/ir.jl#L299
        lines = map(x -> x.line, insts)
        lines = collect(Iterators.flatten(lines))
        return IRCode(
            CC.InstructionStream(
                map(x -> x.stmt, insts),
                collect(Any, map(x -> x.type, insts)),
                collect(CC.CallInfo, map(x -> x.info, insts)),
                lines,
                map(x -> x.flag, insts),
            ),
            cfg,
            CC.copy(bb_code.debuginfo),
            CC.copy(bb_code.argtypes),
            CC.copy(bb_code.meta),
            CC.copy(bb_code.sptypes),
            bb_code.valid_worlds,
        )
    else
        return IRCode(
            CC.InstructionStream(
                map(x -> x.stmt, insts),
                map(x -> x.type, insts),
                map(x -> x.info, insts),
                map(x -> x.line, insts),
                map(x -> x.flag, insts),
            ),
            cfg,
            CC.copy(bb_code.linetable),
            CC.copy(bb_code.argtypes),
            CC.copy(bb_code.meta),
            CC.copy(bb_code.sptypes),
        )
    end
end

function _lower_switch_statements(bb_code::BBCode)
    new_blocks = Vector{BBlock}(undef, 0)
    for block in bb_code.blocks
        t = terminator(block)
        if t isa Switch

            # Create new block without the `Switch`.
            bb = BBlock(block.id, block.inst_ids[1:(end - 1)], block.insts[1:(end - 1)])
            push!(new_blocks, bb)

            # Create new blocks for each `GotoIfNot` from the `Switch`.
            foreach(t.conds, t.dests) do cond, dest
                blk = BBlock(ID(), [ID()], [new_inst(IDGotoIfNot(cond, dest), Any)])
                push!(new_blocks, blk)
            end

            # Create a new block for the fallthrough dest.
            fallthrough_inst = new_inst(IDGotoNode(t.fallthrough_dest), Any)
            push!(new_blocks, BBlock(ID(), [ID()], [fallthrough_inst]))
        else
            push!(new_blocks, block)
        end
    end
    return BBCode(bb_code, new_blocks)
end

function _ids_to_line_numbers(bb_code::BBCode)::InstVector

    # Construct map from `ID`s to `SSAValue`s.
    block_ids = [b.id for b in bb_code.blocks]
    block_lengths = map(length, bb_code.blocks)
    block_start_ssas = SSAValue.(vcat(1, cumsum(block_lengths)[1:(end - 1)] .+ 1))
    line_ids = concatenate_ids(bb_code)
    line_ssas = SSAValue.(eachindex(line_ids))
    id_to_ssa_map = Dict(zip(vcat(block_ids, line_ids), vcat(block_start_ssas, line_ssas)))

    # Apply map.
    return [_to_ssas(id_to_ssa_map, stmt) for stmt in concatenate_stmts(bb_code)]
end

_to_ssas(d::Dict, inst::NewInstruction) = NewInstruction(inst; stmt=_to_ssas(d, inst.stmt))
_to_ssas(d::Dict, x::ReturnNode) = isdefined(x, :val) ? ReturnNode(get(d, x.val, x.val)) : x
_to_ssas(d::Dict, x::Expr) = Expr(x.head, map(a -> get(d, a, a), x.args)...)
_to_ssas(d::Dict, x::PiNode) = PiNode(get(d, x.val, x.val), get(d, x.typ, x.typ))
_to_ssas(d::Dict, x::QuoteNode) = x
_to_ssas(d::Dict, x) = x
function _to_ssas(d::Dict, x::IDPhiNode)
    new_values = Vector{Any}(undef, length(x.values))
    for n in eachindex(x.values)
        if isassigned(x.values, n)
            new_values[n] = get(d, x.values[n], x.values[n])
        end
    end
    return PhiNode(map(e -> Int32(getindex(d, e).id), x.edges), new_values)
end
_to_ssas(d::Dict, x::IDGotoNode) = GotoNode(d[x.label].id)
_to_ssas(d::Dict, x::IDGotoIfNot) = GotoIfNot(get(d, x.cond, x.cond), d[x.dest].id)

function _remove_double_edges(ir::BBCode)
    new_blks = map(enumerate(ir.blocks)) do (n, blk)
        t = terminator(blk)
        if t isa IDGotoIfNot && t.dest == ir.blocks[n + 1].id
            new_insts = vcat(blk.insts[1:(end - 1)], NewInstruction(t; stmt=IDGotoNode(t.dest)))
            return BBlock(blk.id, blk.inst_ids, new_insts)
        else
            return blk
        end
    end
    return BBCode(ir, new_blks)
end

function characterise_unique_predecessor_blocks(
    blks::Vector{BBlock}
)::Tuple{Dict{ID,Bool},Dict{ID,Bool}}

    # Obtain the block IDs in order -- this ensures that we get the entry block first.
    blk_ids = ID[b.id for b in blks]
    preds = _compute_all_predecessors(blks)
    succs = _compute_all_successors(blks)

    # The bulk of blocks can be hanled by this general loop.
    is_unique_pred = Dict{ID,Bool}()
    for id in blk_ids
        ss = succs[id]
        is_unique_pred[id] = !isempty(ss) && all(s -> length(preds[s]) == 1, ss)
    end

    # If there is a single reachable return node, then that block is treated as a unique
    # pred, since control can only pass "out" of the function via this block. Conversely,
    # if there are multiple reachable return nodes, then execution can return to the calling
    # function via any of them, so they are not unique predecessors.
    # Note that the previous block sets is_unique_pred[id] to false for all nodes which
    # end with a reachable return node, so the value only needs changing if there is a
    # unique reachable return node.
    reachable_return_blocks = filter(blks) do blk
        is_reachable_return_node(terminator(blk))
    end
    if length(reachable_return_blocks) == 1
        is_unique_pred[only(reachable_return_blocks).id] = true
    end

    # pred_is_unique_pred is true if the unique predecessor to a block is a unique pred.
    pred_is_unique_pred = Dict{ID,Bool}()
    for id in blk_ids
        pred_is_unique_pred[id] = length(preds[id]) == 1 && is_unique_pred[only(preds[id])]
    end

    # If the entry block has no predecessors, then it can only be entered once, when the
    # function is first entered. In this case, we treat it as having a unique predecessor.
    entry_id = blk_ids[1]
    pred_is_unique_pred[entry_id] = isempty(preds[entry_id])

    return is_unique_pred, pred_is_unique_pred
end

is_reachable_return_node(x::ReturnNode) = isdefined(x, :val)
is_reachable_return_node(x) = false

function characterise_used_ids(stmts::Vector{IDInstPair})::Dict{ID,Bool}
    ids = first.(stmts)
    insts = last.(stmts)

    # Initialise to false.
    is_used = Dict{ID,Bool}(zip(ids, fill(false, length(ids))))

    # Hunt through the instructions, flipping a value in is_used to true whenever an ID
    # is encountered which corresponds to an SSA.
    for inst in insts
        _find_id_uses!(is_used, inst.stmt)
    end
    return is_used
end

function _find_id_uses!(d::Dict{ID,Bool}, x::Expr)
    for arg in x.args
        in(arg, keys(d)) && setindex!(d, true, arg)
    end
end
function _find_id_uses!(d::Dict{ID,Bool}, x::IDGotoIfNot)
    return in(x.cond, keys(d)) && setindex!(d, true, x.cond)
end
_find_id_uses!(::Dict{ID,Bool}, ::IDGotoNode) = nothing
function _find_id_uses!(d::Dict{ID,Bool}, x::PiNode)
    return in(x.val, keys(d)) && setindex!(d, true, x.val)
end
function _find_id_uses!(d::Dict{ID,Bool}, x::IDPhiNode)
    v = x.values
    for n in eachindex(v)
        isassigned(v, n) && in(v[n], keys(d)) && setindex!(d, true, v[n])
    end
end
function _find_id_uses!(d::Dict{ID,Bool}, x::ReturnNode)
    return isdefined(x, :val) && in(x.val, keys(d)) && setindex!(d, true, x.val)
end
_find_id_uses!(d::Dict{ID,Bool}, x::QuoteNode) = nothing
_find_id_uses!(d::Dict{ID,Bool}, x) = nothing

end
