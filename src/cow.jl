using IRTools
using MacroTools

const COW_DISPATCH_INFO = Dict{Expr, Vector{Int}}()

function recurse_no_builtin!(ir, to = IRTools.self)
    for (x, st) in ir
        IRTools.isexpr(st.expr, :call) || continue
        func = st.expr.args[1]
        if isa(func, GlobalRef)
            if func.mod in (Base, Core)
                fexpr = Expr(:., Symbol(func.mod), QuoteNode(func.name))
                if !haskey(COW_DISPATCH_INFO, fexpr)
                    # don't capture function in Base and Core
                    # except the ones in COW_DISPATCH_INFO
                    continue
                end
            end
        end
        ir[x] = Expr(:call, to, st.expr.args...)
    end
    return ir
end

IRTools.@dynamo function cow(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    # IRTools.recurse!(ir)
    recurse_no_builtin!(ir)
    return ir
end

"""
    @cow_dispatch(func, arg_positions)

Redispatch methods of a functon based on the types of its arguments.
`@cow_dispatch(Base.push!, [1])` will generate:

``` julia
COW_DISPATCH_INFO[:(Base.push!)] = [1]
cow(::typeof(Base.push!), args...) =
    Base.push!(Val(:COW), typeof(args[1]), args...)
Base.push!(::Val{:COW}, ::Type, args...) = Base.push!(args...)
```
"""
macro cow_dispatch(func, arg_positions)
    if !(isa(func, Symbol) || (isa(func, Expr) && func.head === :.))
        error("need a function name.")
    end

    type_args = map(arg_positions.args) do i
        :(typeof(args[$i]))
    end
    types = repeat([:(::Type)], length(arg_positions.args))
    expr_func = Expr(:quote, func)
    quote
        Libtask.COW_DISPATCH_INFO[$(expr_func)] = $(arg_positions)
        Libtask.cow(::typeof($(func)), args...) =
            $(func)(Val(:COW), $(type_args...), args...)
        $(func)(::Val{:COW}, $(types...), args...) = $(func)(args...)
    end |> esc
end


"""
    @non_cow_func

Make a function skip the COW mechanism.

```julia
@non_cow_func function f1()
    # ...
end
```
"""
macro non_cow_func(func)
    if isa(func, Symbol) || (isa(func, Expr) && func.head === :.)
        return :(cow(::typeof($(esc(func))), a...) = $(esc(func))(a...))
    end

    @capture(shortdef(func), (name_(args__) = body_) |
             (name_(args__) where {T__} = body_)) ||
             error("Need a function definition")
    return quote
        $(func)
        (Libtask.cow(::typeof($(name)), a...) = $(name)(a...))
    end |> esc
end

"""
    @non_cow

Make an expression skip the COW mechanism.

```julia
@now_cow data[idx] = value
```
"""
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

function obj_for_reading(obj)
    oid = objectid(obj)
    n, d = try
        task_local_storage(oid)
    catch
        (0, obj)
    end
    return d
end
Base.get(obj) = obj_for_reading(obj)

function obj_for_writing(obj)
    oid = objectid(obj)
    n, d = try
        task_local_storage(oid)
    catch
        (0, obj)
    end
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(oid, (cn, newd))
    end
    return newd
end


### COW DISPATCH
# write
@cow_dispatch(Base.setindex!, [1])
@cow_dispatch(Base.push!, [1])
@cow_dispatch(Base.pushfirst!, [1])
@cow_dispatch(Base.pop!, [1])
@cow_dispatch(Base.popfirst!, [1])
@cow_dispatch(Base.deleteat!, [1])
# read
@cow_dispatch(Base.getindex, [1])
@cow_dispatch(Base.iterate, [1])
@cow_dispatch(Base.eltype, [1])
@cow_dispatch(Base.length, [1])
@cow_dispatch(Base.size, [1])
@cow_dispatch(Base.firstindex, [1])
@cow_dispatch(Base.lastindex, [1])
@cow_dispatch(Base.ndims, [1])
@cow_dispatch(Base.axes, [1])

### COW for Array
for F in (:setindex!, :push!, :pushfirst!, :pop!, :popfirst!,
          :deleteat!)
    @eval function Base.$F(::Val{:COW}, ::Type{Array{T, N}},
                           array, args...) where {T, N}
        obj = obj_for_writing(array)
        return $F(obj, args...)
    end
end

for F in (:getindex, :iterate, :eltype, :length, :size,
          :firstindex, :lastindex, :ndims, :axes)
    @eval function Base.$F(::Val{:COW}, ::Type{Array{T, N}},
                           array, args...) where {T, N}
        obj = obj_for_reading(array)
        return $F(obj, args...)
    end
end
