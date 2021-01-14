##########
# TArray #
##########

"""
    TArray{T}(dims, ...)

Implementation of data structures that automatically perform copy-on-write after task copying.

Underlying `TArray`, we have an concrete array object for each task, i,e, if you access (read
or write) a same `TArray` is different tasks, you may get different result because you are
eventaully accessing different arrays wrapped by the `TArray` object.

Now we store the underlying arrays in the field `TArray.data`, a dict whose keys are tasks,
instead of storing them in the task local storage, because the latter way can't provide
assistance for type inference thus may bring a perfomance penalty.

Usage:

```julia
TArray(dim)
```

Example:

```julia
ta = TArray(4)              # init
for i in 1:4 ta[i] = i end  # assign
Array(ta)                   # convert to 4-element Array{Int64,1}: [1, 2, 3, 4]
```
"""
struct TArray{T, N, A <: AbstractArray{T, N}} <: AbstractArray{T, N}
    orig_task :: Task
    data::Dict{Task, Tuple{Int, A}}
    function TArray{T, N, A}() where {T, N, A <: AbstractArray{T, N}}
        d = Dict{Task, Tuple{Int, A}}()
        res = new(current_task(), d)
        keep(res)
        return res
    end
end

TArray{T}(d::Integer...) where T = TArray(T, d)
TArray{T}(::UndefInitializer, d::Integer...) where T = TArray(T, d)
TArray{T}(::UndefInitializer, dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)
TArray{T,N}(d::Vararg{<:Integer,N}) where {T,N} = TArray(T, d)
TArray{T,N}(::UndefInitializer, d::Vararg{<:Integer,N}) where {T,N} = TArray{T,N}(d)
TArray{T,N}(dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)

function TArray(T::Type, dim)
    N_dim = length(dim)
    res = TArray{T, N_dim, Array{T, N_dim}}()
    n = n_copies()
    d = Array{T}(undef, dim)
    _local_storage(res, (n, d))
    res
end

TArray(x::AbstractArray) = convert(TArray, x)

# TArray House-Keeper

const TArrayKeeper = Vector{WeakRef}()
keep(x::TArray) = push!(TArrayKeeper, WeakRef(x))
function copy_tarrays(task1::Task, task2::Task)
    filter!(x -> x.value !== nothing, TArrayKeeper)
    for wref in TArrayKeeper
        ta = wref.value
        if haskey(ta.data, task1) && !haskey(ta.data, task2)
            ta.data[task2] = ta.data[task1]
        end
    end
end

# _local_storage
_local_storage(x::TArray) = x.data[current_task()]
function _local_storage(x::TArray{T, N}, v::Tuple{Int, AbstractArray{T, N}}) where {T, N}
    x.data[current_task()] = v
end

_get(x) = x
function _get(x::TArray)
    n, d = x.data[current_task()]
    return d
end
function _get_for_write(x::TArray)
    n, d = x.data[current_task()]
    cn   = n_copies()
    newd = d
    if cn > n
        # println("[setindex!]: $(x.ref) copying data")
        newd = deepcopy(d)
        x.data[current_task()] = (cn, newd)
    end
    return newd
end

localize(x) = x
localize(x::AbstractArray) = TArray(x)

# Constructors
"""
     tzeros(dims, ...)

Construct a distributed array of zeros.
Trailing arguments are the same as those accepted by `TArray`.

```julia
tzeros(dim)
```

Example:

```julia
tz = tzeros(4)              # construct
Array(tz)                   # convert to 4-element Array{Int64,1}: [0, 0, 0, 0]
```
"""
function tzeros(T::Type, dim)
    res = TArray{T,length(dim), Array{T, length(dim)}}();
    n = n_copies()
    d = zeros(T,dim)
    _local_storage(res, (n, d))
    return res
end

tzeros(::Type{T}, d1::Integer, drest::Integer...) where T = tzeros(T, convert(Dims, tuple(d1, drest...)))
tzeros(d1::Integer, drest::Integer...) = tzeros(Float64, convert(Dims, tuple(d1, drest...)))
tzeros(d::Dims) = tzeros(Float64, d)

"""
     tfill(val, dim, ...)

Construct a TArray of a specified value.

```julia
tfill(val, dim)
```

Example:

```julia
tz = tfill(9.0, 4)            # construct
Array(tz)                     # convert to 4-element Array{Float64,1}:  [9.0  9.0  9.0  9.0]
```
"""
function tfill(val::Real, dim)
    res = TArray{typeof(val), length(dim), Array{typeof(val), length(dim)}}();
    n = n_copies()
    d = fill(val,dim)
    _local_storage(res, (n, d))
    return res
end

#
# Conversion between TArray and Array
#

function Base.convert(::Type{Array}, x::TArray)
    return convert(Array{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{Array{T, N}}, x::TArray{T, N}) where {T, N}
    c = convert(Array{T, N}, deepcopy(_get(x)))
    return c
end

function Base.convert(::Type{TArray}, x::AbstractArray)
    return convert(TArray{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{TArray{T, N}}, x::AbstractArray{T, N}) where {T, N}
    res = TArray{T, N, typeof(x)}()
    n   = n_copies()
    _local_storage(res, (n, x))
    return res
end

#
# Representation
#
function Base.show(io::IO, ::MIME"text/plain", x::TArray)
    arr = x.data[x.orig_task][2]
    @warn "Here shows the originating task's storage, " *
        "not the current task's storage. " *
        "Please explicitly call show(::TArray) to display the current task's version of a TArray."
    show(io,  MIME("text/plain"), arr)
end

Base.show(io::IO, x::TArray) = show(io::IO, _get(x))

function Base.summary(io::IO, x::TArray)
  print(io, "Task Local Array: ")
  summary(io, _get(x))
end

#
# Forward many methods to the underlying array
#
for F in (:size,
          :iterate,
          :firstindex, :lastindex, :axes)
    @eval Base.$F(a::TArray, args...) = $F(_get(a), args...)
end

#
# Similarity implementation
#

Base.similar(x::TArray, ::Type{T}, dims::Dims) where T = TArray(similar(_get(x), T, dims))

for op in [:(==), :≈]
    @eval Base.$op(x::TArray, y::AbstractArray) = Base.$op(_get(x), y)
    @eval Base.$op(x::AbstractArray, y::TArray) = Base.$op(x, _get(y))
    @eval Base.$op(x::TArray, y::TArray) = Base.$op(_get(x), _get(y))
end

#
# Array Stdlib
#

# Indexing Interface
Base.@propagate_inbounds function Base.getindex(x::TArray{T, N}, I::Vararg{Int,N}) where {T, N}
    return _get(x)[I...]
end

Base.@propagate_inbounds function Base.setindex!(x::TArray{T, N}, e, I::Vararg{Int,N}) where {T, N}
    d = _get_for_write(x)
    d[I...] = e
end

function Base.push!(x::TArray, e)
    d = _get_for_write(x)
    push!(d, e)
end

function Base.pop!(x::TArray)
    d = _get_for_write(x)
    pop!(d)
end

# Other methods from stdlib

Base.view(x::TArray, inds...; kwargs...) =
    Base.view(_get(x), inds...; kwargs...) |> localize
Base.:-(x::TArray) = (- _get(x)) |> localize
Base.transpose(x::TArray) = transpose(_get(x)) |> localize
Base.adjoint(x::TArray) = adjoint(_get(x)) |> localize
Base.repeat(x::TArray; kw...) = repeat(_get(x); kw...) |> localize

Base.hcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    hcat(_get.(xs)...) |> localize
Base.vcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    vcat(_get.(xs)...) |> localize
Base.cat(xs::Union{TArray{T,1}, TArray{T,2}}...; dims) where T =
    cat(_get.(xs)...; dims = dims) |> localize


Base.reshape(x::TArray, dims::Union{Colon,Int}...) = reshape(_get(x), dims) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Union{Int,Colon}}}) =
    reshape(_get(x), Base._reshape_uncolon(_get(x), dims)) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Int}}) = reshape(_get(x), dims) |> localize

Base.permutedims(x::TArray, perm) = permutedims(_get(x), perm) |> localize
Base.PermutedDimsArray(x::TArray, perm) = PermutedDimsArray(_get(x), perm) |> localize
Base.reverse(x::TArray; dims) = reverse(_get(x), dims = dims) |> localize

Base.sum(x::TArray; dims = :) = sum(_get(x), dims = dims) |> localize
Base.sum(f::Union{Function,Type},x::TArray) = sum(f.(_get(x))) |> localize
Base.prod(x::TArray; dims=:) = prod(_get(x); dims=dims) |> localize
Base.prod(f::Union{Function, Type}, x::TArray) = prod(f.(_get(x))) |> localize

Base.findfirst(x::TArray, args...) = findfirst(_get(x), args...) |> localize
Base.maximum(x::TArray; dims = :) = maximum(_get(x), dims = dims) |> localize
Base.minimum(x::TArray; dims = :) = minimum(_get(x), dims = dims) |> localize

Base.:/(x::TArray, y::TArray) = _get(x) / _get(y) |> localize
Base.:/(x::AbstractArray, y::TArray) = x / _get(y) |> localize
Base.:/(x::TArray, y::AbstractArray) = _get(x) / y |> localize
Base.:\(x::TArray, y::TArray) = _get(x) \ _get(y) |> localize
Base.:\(x::AbstractArray, y::TArray) = x \ _get(y) |> localize
Base.:\(x::TArray, y::AbstractArray) = _get(x) \ y |> localize
Base.:*(x::TArray, y::TArray) = _get(x) * _get(y) |> localize
Base.:*(x::AbstractArray, y::TArray) = x * _get(y) |> localize
Base.:*(x::TArray, y::AbstractArray) = _get(x) * y |> localize

# broadcast
Base.BroadcastStyle(::Type{TArray{T, N, A}}) where {T, N, A} = Broadcast.ArrayStyle{TArray}()
Broadcast.broadcasted(::Broadcast.ArrayStyle{TArray}, f, args...) = f.(_get.(args)...) |> localize

import LinearAlgebra
import LinearAlgebra:  \, /, inv, det, logdet, logabsdet, norm

LinearAlgebra.inv(x::TArray) = inv(_get(x)) |> localize
LinearAlgebra.det(x::TArray) = det(_get(x)) |> localize
LinearAlgebra.logdet(x::TArray) = logdet(_get(x)) |> localize
LinearAlgebra.logabsdet(x::TArray) = logabsdet(_get(x)) |> localize
LinearAlgebra.norm(x::TArray, p::Real = 2) =
    LinearAlgebra.norm(_get(x), p) |> localize

import LinearAlgebra: dot
dot(x::TArray, ys::TArray) = dot(_get(x), _get(ys)) |> localize
dot(x::AbstractArray, ys::TArray) = dot(x, _get(ys)) |> localize
dot(x::TArray, ys::AbstractArray) = dot(_get(x), ys) |> localize

using Statistics
Statistics.mean(x::TArray; dims = :) = mean(_get(x), dims = dims) |> localize
Statistics.std(x::TArray; kw...) = std(_get(x), kw...) |> localize

# TODO
# * NNlib
