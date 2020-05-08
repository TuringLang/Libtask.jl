using IRTools
using MacroTools

const COW_DISPATCH_INFO = Dict{Expr, Vector{Int}}()

"""
    _recurse!(ir, to=IRTools.self)

Like `IRTools.recurse!`, for every statement in `ir`, if it is a
`:call` expression, change the statment to a call to the `dynamo`
`self`, and so to all the nested function calls.

The difference to `IRTools.recurse!` is that for the function calls to
the functions in `COW_DISPATCH_INFO`, we skip them because we
overwrite these functions to do the COW.

"""
function _recurse!(ir, to=IRTools.self)
    for (x, st) in ir
        IRTools.isexpr(st.expr, :call) || continue
        func = st.expr.args[1]
        isa(func, IRTools.Variable) && (func = ir[func].expr)
        if isa(func, GlobalRef)
            # don't capture functions in Base and Core
            # except the ones in COW_DISPATCH_INFO
            if func.mod in (Base, Core)
                fexpr = Expr(:., Symbol(func.mod), QuoteNode(func.name))
                haskey(COW_DISPATCH_INFO, fexpr) || continue
            end
        end
        ir[x] = Expr(:call, to, st.expr.args...)
    end
    return ir
end

IRTools.@dynamo function _cow(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    # IRTools.recurse!(ir)
    _recurse!(ir)
    return ir
end

_cow_func_name(s::Symbol) = Symbol("_" * string(s) * "_maybecopy")

"""
    @maybecopy(func, arg_positions)

Redispatch methods of a functon based on the types of its arguments.
`@maybecopy(Base.push!, [1])` will generate:

``` julia
COW_DISPATCH_INFO[:(Base.push!)] = [1]
_cow(::typeof(Base.push!), args...) =
    _push!_maybecopy(typeof(args[1]), args...)
_push!_maybecopy(::Type, args...) = Base.push!(args...)
```
"""
macro maybecopy(func, arg_positions)
    if func isa Symbol
        cow_func = _cow_func_name(func)
    elseif Meta.isexpr(func, :.)
        cow_func = _cow_func_name(func.args[2].value)
    else
        error("need a function name.")
    end

    type_args = map(arg_positions.args) do i
        :(typeof(args[$i]))
    end
    types = repeat([:(::Type)], length(arg_positions.args))
    expr_func = Expr(:quote, func)
    quote
        Libtask.COW_DISPATCH_INFO[$(expr_func)] = $(arg_positions)
        Libtask._cow(::typeof($(func)), args...) =
            $(cow_func)($(type_args...), args...)
        $(cow_func)($(types...), args...) = $(func)(args...)
    end |> esc
end


"""
    @nevercopy

Make a function skip the COW mechanism.

```julia
@nevercopy function f1()
    # ...
end
```
"""
macro nevercopy(func)
    if isa(func, Symbol) || (isa(func, Expr) && func.head === :.)
        return :(_cow(::typeof($(esc(func))), a...) = $(esc(func))(a...))
    end

    if @capture(shortdef(func),
                (name_(args__) = body_) | (name_(args__) where {T__} = body_))
        # function definition
        return quote
            $(func)
            (Libtask._cow(::typeof($(name)), a...) = $(name)(a...))
        end |> esc
    end

    # else, func is an expression
    quote
        f = () -> begin
            $(esc(func))
        end
        nevercopy(f)
    end
end

@nevercopy(produce)
@nevercopy(consume)
@nevercopy nevercopy(f, args...) = f(args...)

function _obj_for_reading(obj)
    oid = objectid(obj)
    ct = _current_task()
    n, d = get(Base.get_task_tls(ct), oid, (0, obj))
    return d
end
obj_for_reading(obj) = _obj_for_reading(obj)
Base.get(obj) = obj_for_reading(obj)

function _obj_for_writing(obj)
    oid = objectid(obj)
    ct = _current_task()
    n, d = get(Base.get_task_tls(ct), oid, (0, obj))
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(oid, (cn, newd))
    end
    return newd
end
obj_for_writing(obj) = _obj_for_writing(obj)

### COW DISPATCH
# write
@maybecopy(Base.setindex!, [1])
@maybecopy(Base.push!, [1])
@maybecopy(Base.pushfirst!, [1])
@maybecopy(Base.pop!, [1])
@maybecopy(Base.popfirst!, [1])
@maybecopy(Base.delete!, [1])
@maybecopy(Base.deleteat!, [1])
# read
@maybecopy(Base.getindex, [1])
@maybecopy(Base.iterate, [1])
@maybecopy(Base.eltype, [1])
@maybecopy(Base.length, [1])
@maybecopy(Base.size, [1])
@maybecopy(Base.firstindex, [1])
@maybecopy(Base.lastindex, [1])
@maybecopy(Base.ndims, [1])
@maybecopy(Base.axes, [1])

### COW for Array
for F in (:setindex!, :push!, :pushfirst!, :pop!, :popfirst!,
          :deleteat!)
    cow_func = _cow_func_name(F)
    @eval function $cow_func(::Type{Array{T, N}}, array, args...) where {T, N}
        obj = obj_for_writing(array)
        return $F(obj, args...)
    end
end

for F in (:getindex, :iterate, :eltype, :length, :size,
          :firstindex, :lastindex, :ndims, :axes)
    cow_func = _cow_func_name(F)
    @eval function $cow_func(::Type{Array{T, N}}, array, args...) where {T, N}
        obj = obj_for_reading(array)
        return $F(obj, args...)
    end
end

### COW for Dict
function obj_for_writing(obj::Dict{K, V}) where {K, V}
    ct = _current_task()
    obj === ct.storage && return obj
    return _obj_for_writing(obj)
end

for F in (:setindex!, :push!, :pop!, :delete!)
    cow_func = _cow_func_name(F)
    @eval function $cow_func(::Type{Dict{K, V}}, dict, args...) where {K, V}
        obj = obj_for_writing(dict)
        return $F(obj, args...)
    end
end

for F in (:getindex, :iterate, :eltype, :length)
    cow_func = _cow_func_name(F)
    @eval function $cow_func(::Type{Dict{K, V}}, dict, args...) where {K, V}
        obj = obj_for_reading(dict)
        return $F(obj, args...)
    end
end
