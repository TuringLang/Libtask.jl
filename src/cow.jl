using IRTools
using MacroTools

using IRTools: BasicBlock, stmt, var

function copy_object(obj)
    ct = _current_task()
    ct.storage === nothing && (ct.storage = IdDict())
    if !haskey(ct.storage, :_cow_asset)
        ct.storage[:_cow_asset] = IdDict{Any, Bool}()
    end
    if !haskey(ct.storage, :_cow_copylog)
        ct.storage[:_cow_copylog] = Dict{UInt64, Any}()
    end

    haskey(ct.storage[:_cow_asset], obj) && (return obj) # own this obj
    if haskey(ct.storage[:_cow_copylog], objectid(obj)) # had copied this obj
        ret = ct.storage[:_cow_copylog][objectid(obj)]
        ct.storage[:_cow_asset][ret] = true
        return ret
    end

    new_obj = deepcopy(obj)
    ct.storage[:_cow_asset][new_obj] = true
    ct.storage[:_cow_copylog][objectid(obj)] = new_obj
    return new_obj
end

maybe_copy(x) = x
maybe_copy(x::AbstractArray{<:Number}) = copy_object(x)

# TODO: this should be list of (op => mutated_arg_pos)
const MUTATING_OPS = (:push!, :pop!, :setindex!)

function mutating(e)
    isa(e, Expr) || return (false, 0)
    e.head == :call || return (false, 0)
    if isa(e.args[1], GlobalRef) && e.args[1].name in MUTATING_OPS
        return (true, 1)
    end

    # IRTools.Inner.Statement(:((IRTools.Inner.Self())(Main.push!, %2, 1)), Any, 2)
    if isa(e.args[1], IRTools.Inner.Self) && isa(e.args[2], GlobalRef) &&
        e.args[2].name in MUTATING_OPS
        return (true, 2)
    end
    return (false, 0)
end

function all_successors(b::IRTools.Block, accu::Vector{Int64})
    succs = IRTools.successors(b)
    ids = map(x -> x.id, succs)
    issubset(ids, accu) && return
    for blk in succs
        push!(accu, blk.id)
        all_successors(blk, accu)
    end
end

function all_successors(b::IRTools.Block)
    ret = Int64[]
    all_successors(b, ret)
    return ret
end

function find_mutating_blocks(ir::IRTools.IR)
    vars_to_blocks = Dict{IRTools.Variable, Vector{Int64}}()
    for blk in IRTools.blocks(ir)
        for (v, st) in blk
            mut, mpos = mutating(st.expr)
            mut || continue
            # mpos = 1 when push!(...), mpos = 2 when self(push!, ...)
            isa(st.expr.args[mpos + 1], IRTools.Variable) || continue
            mv = st.expr.args[mpos + 1]
            if haskey(vars_to_blocks, mv)
                push!(vars_to_blocks[mv], blk.id)
            else
                vars_to_blocks[mv] = [blk.id]
            end
            push!(vars_to_blocks[mv], all_successors(blk)...)
        end
    end

    for (v, blk_ids) in vars_to_blocks
        unique!(blk_ids)
    end

    return vars_to_blocks
end

function insert_copy_for_var(ir::IRTools.IR, var, blk_ids::Vector{Int64})
    mutate_instructions = Dict{IRTools.Variable, IRTools.Variable}()

    for blk_id in blk_ids
        blk = IRTools.block(ir, blk_id)
        for (v, st) in blk
            isa(st.expr, Expr) || continue
            (var in st.expr.args) || continue
            rk = insert!(blk, v, IRTools.xcall(Libtask, :maybe_copy, var))
            for i in 1:length(st.expr.args)
                st.expr.args[i] == var && (st.expr.args[i] = rk)
            end
        end
    end

    return ir
end


function insert_copy_stage_1(ir::IRTools.IR)
    mutate_vars = Dict{IRTools.Variable, Bool}()
    replacements = Dict{IRTools.Variable, IRTools.Variable}()

    for (v, st) in ir
        mut, mpos = mutating(st.expr)
        mut || continue
        # mpos = 1 when push!(...), mpos = 2 when self(push!, ...)
        if isa(st.expr.args[mpos + 1], IRTools.Variable)
            mutate_vars[st.expr.args[mpos + 1]] = true
        end
    end

    for (mv, _) in mutate_vars
        rk = IRTools.insertafter!(ir, mv, :(_placeholder()))
        replacements[mv] = rk
    end

    ir_new = MacroTools.prewalk(ir) do x
        if (x isa IRTools.Variable) && haskey(replacements, x)
            return replacements[x]
        end
        return x
    end

    for (mv, newk) in replacements
        ir_new[newk] = IRTools.xcall(Libtask, :maybe_copy, mv)
    end

    return ir_new
end

function insert_copy_stage_2(ir::IRTools.IR)
    mv_blocks = find_mutating_blocks(ir)
    for (var, blk_ids) in mv_blocks
        insert_copy_for_var(ir, var, blk_ids)
    end
    return ir
end

insert_copy(ir) = ir |> insert_copy_stage_1 |> insert_copy_stage_2

IRTools.@dynamo function cow(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    IRTools.recurse!(ir)
    ir = insert_copy(ir)
    # @show ir
    return ir
end

cow(::typeof(Base.zeros), a...) = Base.zeros(a...)
cow(::typeof(Base.push!), a...) = Base.push!(a...)
cow(::typeof(Base.pop!), a...) = Base.pop!(a...)
cow(::typeof(Base.setindex!), a...) = Base.setindex!(a...)
cow(::typeof(produce), a...) = produce(a...)
