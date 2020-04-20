using IRTools
using MacroTools

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
maybe_copy(x::AbstractDict) = begin
    ct = _current_task()
    if x === ct.storage
        return x
    end
    copy_object(x)
end

# (func_name => mutated_arg_pos)
const MUTATING_OPS = Dict{Symbol, Int}(
    :setindex! => 1,
    :push! => 1, :pushfirst! => 1,
    :pop! => 1, :popfirst! => 1,
    :append! => 1,
    :delete! => 1, :deleteat! => 1,
    :setdiff! => 1,
)

function mutating(e)
    isa(e, Expr) || return (false, 0)
    e.head == :call || return (false, 0)
    if isa(e.args[1], GlobalRef) && haskey(MUTATING_OPS, e.args[1].name)
        return (true, MUTATING_OPS[e.args[1].name])
    end

    # IRTools.Statement(:((IRTools.Self())(Main.push!, %2, 1)), Any, 2)
    if (e.args[1] === IRTools.self) && isa(e.args[2], GlobalRef) &&
        haskey(MUTATING_OPS, e.args[2].name)
        return (true, MUTATING_OPS[e.args[2].name] + 1)
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

function recurse_no_builtin!(ir, to = IRTools.self)
    for (x, st) in ir
        IRTools.isexpr(st.expr, :call) || continue
        func = st.expr.args[1]
        if isa(func, GlobalRef) && func.mod in (Base, Core)
            continue
        end
        ir[x] = Expr(:call, to, st.expr.args...)
    end
    return ir
end

IRTools.@dynamo function cow(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    recurse_no_builtin!(ir)
    ir = insert_copy(ir)
    return ir
end

macro non_cow_func(func)
    if isa(func, Symbol) || (isa(func, Expr) && func.head === :.)
        return :(cow(::typeof($(esc(func))), a...) = $(esc(func))(a...))
    end

    @capture(shortdef(func), (name_(args__) = body_) |
             (name_(args__) where {T__} = body_)) || error("Need a function definition")
    return quote
        $(esc(func))
        $(esc(:(Libtask.cow(::typeof($(name)), a...) = $(name)(a...))))
    end
end

macro non_cow(expr)
    quote
        f = () -> begin
            $(esc(expr))
        end
        Libtask.non_cow_call(f)
    end
end

@non_cow_func(produce)
@non_cow_func(consume)
@non_cow_func non_cow_call(func, args...) = func(args...)


# debug
function cmp_cow_ir(func, args...)
    ir = IRTools.@code_ir func(args...)
    print("\nIR before transformation --------\n")
    print(ir)
    ir = insert_copy(ir)
    print("\nIR after transformation --------\n")
    print(ir)
end
