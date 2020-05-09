using IRTools
using MacroTools


function _cow_func_name(func)
    fname = if func isa Symbol
        string(func)
    elseif Meta.isexpr(func, :.)
        string(func.args[1]) * "_" * string(func.args[2].value)
    else
        error("need a function name.")
    end
    Symbol("_$(fname)_maybecopy")
end

"""
    _recurse!(ir, to=IRTools.self)

Like `IRTools.recurse!`, for every statement in `ir`, if it is a
`:call` expression, change the statment to a call to the `dynamo`
`self`, and so to all the nested function calls.

The difference to `IRTools.recurse!` is that for the calls to the
functions we passed to @maybecopy, we skip them because we overwrite
these functions to do the COW.

"""
function _recurse!(ir, to=IRTools.self)
    for (x, st) in ir
        IRTools.isexpr(st.expr, :call) || continue
        func = st.expr.args[1]
        isa(func, IRTools.Variable) && (func = ir[func].expr)
        if isa(func, GlobalRef)
            # don't capture functions in Base and Core
            # except the ones for which we implement COW
            if func.mod in (Base, Core)
                fexpr = Expr(:., Symbol(func.mod), QuoteNode(func.name))
                isdefined(Libtask, _cow_func_name(fexpr)) || continue
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


"""
    @maybecopy(funcall)

Redispatch methods of a functon based on the types of its arguments.

Examples:

`@maybecopy Base.push!(w_1::AbstractArray, x)` will be expanded as:

``` julia
_cow(::typeof(Base.push!), args...) = Base.push!(args...) # fallback

function _push!_maybecopy(w_1::AbstractArray, x)
    w_1_c = obj_for_writing(w_1)
    Base.push!(w_1_c, x)
end
_cow(::typeof(Base.push!), w_1::AbstractArray, x) = _push!_maybecopy(w_1, x)
```

`@maybecopy func(w_v::T1, x, y::T2, z)` will be expanded as:

``` julia
_cow(::typeof(func), args...) = func(args...) # fallback

function _func_maybecopy(w_v::T1, x, y::T2, z)
    w_v_c = obj_for_writing(w_v)
    y_c = obj_for_reading(y)
    func(w_v_c, x, y_c, z)
end
_cow(::typeof(func), w_v::T1, x, y::T2, z) = _func_maybecopy(w_v, x, y, z)
```

All the arguments with type annotation will be ensured to be task local:
- if the variable name starts with `w_`, it will be passed to
  `obj_for_writing`
- else it will be passed to `obj_for_reading`
- arguments without type annotation will be untouched

"""
macro maybecopy(funcall)
    (@capture funcall fname_(args__)) || error("need a function call")

    cow_func = _cow_func_name(fname)
    args_transfer, forward_args = [], []
    for arg in args
        if Meta.isexpr(arg, :(::))
            new_var = gensym(arg.args[1])
            trans = startswith(string(arg.args[1]), "w_") ?
                :($new_var = obj_for_writing($(arg.args[1]))) :
                :($new_var = obj_for_reading($(arg.args[1])))
            push!(args_transfer, trans)
            push!(forward_args, new_var)
        else
            push!(forward_args, arg)
        end
    end

    fallback = if isdefined(Libtask, cow_func)
        quote end
    else
        quote
            Libtask._cow(::typeof($(fname)), args...) = $(fname)(args...)
        end
    end

    quote
        $(fallback)
        function $(cow_func)($(args...))
            $(args_transfer...)
            $(fname)($(forward_args...))
        end
        Libtask._cow(::typeof($(fname)), $(args...)) = $(cow_func)($(args...))

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

### COW for Array
@maybecopy Base.setindex!(w_1::AbstractArray, args...)
@maybecopy Base.push!(w_1::AbstractArray, args...)
@maybecopy Base.pushfirst!(w_1::AbstractArray, args...)
@maybecopy Base.pop!(w_1::AbstractArray, args...)
@maybecopy Base.popfirst!(w_1::AbstractArray, args...)
@maybecopy Base.deleteat!(w_1::AbstractArray, args...)

for F in (:getindex, :iterate, :eltype, :length, :size,
          :firstindex, :lastindex, :ndims, :axes)
    @eval @maybecopy Base.$F(read::AbstractArray, args...)
end

### COW for Dict
function obj_for_writing(obj::Dict{K, V}) where {K, V}
    ct = _current_task()
    obj === ct.storage && return obj
    return _obj_for_writing(obj)
end

for F in (:setindex!, :push!, :pop!, :delete!)
    @eval @maybecopy Base.$F(w_1::Dict, args...)
end

for F in (:getindex, :iterate, :eltype, :length)
    @eval @maybecopy Base.$F(read::Dict, args...)
end
