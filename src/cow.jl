using IRTools
using MacroTools

using IRTools: BasicBlock, stmt, var

const MUTATING_OPS = (:push!, :pop!, :setindex!)

function copy_object(obj)
    ct = _current_task()
    ct.storage == nothing && (ct.storage = IdDict())
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
maybe_copy(x::AbstractArray{T}) where {T<:Number} = copy_object(x)

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

function insert_copy_stage_1(ir::IRTools.IR)
    mutate_vars = Dict{IRTools.Variable, Bool}()
    replacements = Dict{IRTools.Variable, IRTools.Variable}()

    for (v, st) in ir
        isa(v, IRTools.Variable) || continue
        isa(st, IRTools.Statement) || continue
        mut, mpos = mutating(st.expr)
        mut || continue
        if mpos == 1 && isa(st.expr.args[2], IRTools.Variable) # push!(...)
            mutate_vars[st.expr.args[2]] = true
        elseif mpos == 2 && isa(st.expr.args[3], IRTools.Variable)  # self(push!, ...)
            mutate_vars[st.expr.args[3]] = true
        end
    end

    for (mv, _) in mutate_vars
        # rk = insert!(ir, ov, :(_()))
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
        # ir_new[newk] = :(Libtask.maybe_copy($(ov)))
        ir_new[newk] = IRTools.xcall(GlobalRef(Libtask, :maybe_copy), mv)
    end

    return ir_new
end

function update_var(ir, start, from, to)
    started = false
    for (v, st) in ir
        if v == start
            started = true
        end
        started || continue

        st_new = MacroTools.prewalk(st) do x
            return x == from ? to : x
        end

        ir[v] = st_new
    end
end

function insert_copy_stage_2(ir::IRTools.IR)
    mutate_instructions = Dict{IRTools.Variable, IRTools.Variable}()

    for (v, st) in ir
        isa(v, IRTools.Variable) || continue
        isa(st, IRTools.Statement) || continue
        mut, mpos = mutating(st.expr)
        mut || continue
        if mpos == 1 && isa(st.expr.args[2], IRTools.Variable) # push!(...)
            mutate_instructions[v] = st.expr.args[2]
        elseif mpos == 2 && isa(st.expr.args[3], IRTools.Variable)  # self(push!, ...)
            mutate_instructions[v] = st.expr.args[3]
        end
    end

    for (mres, mv) in mutate_instructions
        rk = insert!(ir, mres, IRTools.xcall(GlobalRef(Libtask, :maybe_copy), mv))
        st = ir[mres]
        update_var(ir, mres, mv, rk)
    end

    return ir
end

insert_copy(ir) = ir |> insert_copy_stage_1 |> insert_copy_stage_2

IRTools.@dynamo function cow(a...)
    ir = IRTools.IR(a...)
    ir == nothing && return
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
