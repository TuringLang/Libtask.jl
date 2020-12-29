##########
# TArray #
##########

"""
    TArray{T}(dims, ...)

Implementation of data structures that automatically perform copy-on-write after task copying.

If current_task is an existing key in `s`, then return `s[current_task]`. Otherwise, return `s[current_task] = s[last_task]`.

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
struct TArray{T,N} <: AbstractArray{T,N}
    ref :: Symbol  # object_id
    orig_task :: Task
    TArray{T,N}() where {T,N} = new(gensym(), current_task())
end

TArray{T}(d::Integer...) where T = TArray(T, d)
TArray{T}(::UndefInitializer, d::Integer...) where T = TArray(T, d)
TArray{T,N}(d::Vararg{<:Integer,N}) where {T,N} = TArray(T, d)
TArray{T,N}(::UndefInitializer, d::Vararg{<:Integer,N}) where {T,N} = TArray{T,N}(d)
TArray{T,N}(dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)

function TArray(T::Type, dim)
    res = TArray{T,length(dim)}();
    n = n_copies()
    d = Array{T}(undef, dim)
    task_local_storage(res.ref, (n,d))
    res
end

TArray(x::AbstractArray) = convert(TArray, x)

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
    res = TArray{T,length(dim)}();
    n = n_copies()
    d = zeros(T,dim)
    task_local_storage(res.ref, (n,d))
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
    res = TArray{typeof(val),length(dim)}();
    n = n_copies()
    d = fill(val,dim)
    task_local_storage(res.ref, (n,d))
    return res
end

#
# Conversion between TArray and Array
#
function Base.get(x::TArray)
    n, d = task_local_storage(x.ref)
    return d
end

_get(x) = x
_get(x::TArray) = get(x)

function Base.convert(::Type{Array}, x::TArray)
    return convert(Array{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{Array{T,N}}, x::TArray{T,N}) where {T,N}
    c = convert(Array{T, N}, deepcopy(get(x)))
    return c
end

function Base.convert(::Type{TArray}, x::AbstractArray)
    return convert(TArray{eltype(x),ndims(x)}, x)
end
function Base.convert(::Type{TArray{T,N}}, x::AbstractArray{T,N}) where {T,N}
    res = TArray{T,N}()
    n   = n_copies()
    task_local_storage(res.ref, (n,x))
    return res
end

#
# Representation
#
function Base.display(x::TArray)
    arr = x.orig_task.storage[x.ref][2]
    @warn "display(::TArray) prints the originating task's storage, " *
        "not the current task's storage. " *
        "Please use show(::TArray) to display the current task's version of a TArray."
    display(arr)
end

Base.show(io::IO, x::TArray) = Base.show(io::IO, task_local_storage(x.ref)[2])

function Base.summary(io::IO, x::TArray)
  print(io, "Task Local Array: ")
  summary(io, get(x))
end

Base.print_array(io::IO, x::TArray) = Base.print_array(io, get(x))

#
# Iterator Interface
#

IteratorSize(::Type{TArray{T, N}}) where {T, N} = HasShape{N}()
IteratorEltype(::Type{TArray}) = HasEltype()

#
# Forward many methods to the underlying array
#
for F in (:iterate, :eltype, :length, :size,
          :firstindex, :lastindex, :ndims, :axes,
          :collect)
    @eval Base.$F(a::TArray, args...) = $F(get(a), args...)
end

#
# Similarity implementation
#

Base.similar(x::TArray) = tzeros(eltype(x), size(x))
Base.similar(x::TArray, ::Type{T}) where {T} = tzeros(T, size(x))
Base.similar(x::TArray, dims::Dims) = tzeros(eltype(x), dims)

for op in [:(==), :≈]
    @eval Base.$op(x::TArray, y::AbstractArray) = Base.$op(get(x), y)
    @eval Base.$op(x::AbstractArray, y::TArray) = Base.$op(x, get(y))
    @eval Base.$op(x::TArray, y::TArray) = Base.$op(get(x), get(y))
end

#
# Array Stdlib
#

# Indexing Interface
function Base.getindex(x::TArray{T, N}, I::Vararg{Int,N}) where {T, N}
    t, d = task_local_storage(x.ref)
    return d[I...]
end

function Base.setindex!(x::TArray{T, N}, e, I::Vararg{Int,N}) where {T, N}
    n, d = task_local_storage(x.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        # println("[setindex!]: $(x.ref) copying data")
        newd = deepcopy(d)
        task_local_storage(x.ref, (cn, newd))
    end
    newd[I...] = e
end

function Base.push!(x::TArray{T}, e) where T
    n, d = task_local_storage(x.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(x.ref, (cn, newd))
    end
    push!(newd, e)
end

function Base.pop!(x::TArray)
    n, d = task_local_storage(x.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(x.ref, (cn, newd))
    end
    pop!(d)
end

# Other methods from stdlib

Base.view(x::TArray, inds...; kwargs...) =
    Base.view(get(x), inds...; kwargs...) |> localize
Base.:-(x::TArray) = (- get(x)) |> localize
Base.transpose(x::TArray) = transpose(get(x)) |> localize
Base.adjoint(x::TArray) = adjoint(get(x)) |> localize
Base.repeat(x::TArray; kw...) = repeat(get(x); kw...) |> localize
Base.hcat(x::TArray, rest...) = hcat(get(x), _get.(rest)...) |> localize
Base.hcat(x::AbstractArray, y::TArray, rest...) = hcat(x, get(y), _get.(rest)...) |> localize
Base.vcat(x::TArray, rest...) = vcat(get(x), _get.(rest)...) |> localize
Base.vcat(x::AbstractArray, y::TArray, rest...) = vcat(x, get(y), _get.(rest)...) |> localize
Base.cat(x::TArray, rest...; dims) = cat(get(x), _get.(rest)...; dims = dims) |> localize
Base.cat(x::AbstractArray, y::TArray, rest...; dims) =
    cat(x, get(y), _get.(rest)...; dims = dims) |> localize

Base.reshape(x::TArray, dims::Union{Colon,Int}...) = reshape(get(x), dims) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Union{Int,Colon}}}) =
    reshape(get(x), Base._reshape_uncolon(get(x), dims)) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Int}}) = reshape(get(x), dims) |> localize

Base.permutedims(x::TArray, perm) = permutedims(get(x), perm) |> localize
Base.PermutedDimsArray(x::TArray, perm) = PermutedDimsArray(get(x), perm) |> localize
Base.reverse(x::TArray; dims) = reverse(get(x), dims = dims) |> localize

Base.sum(x::TArray; dims = :) = sum(get(x), dims = dims) |> localize
Base.sum(f::Union{Function,Type},x::TArray) = sum(f.(get(x))) |> localize
Base.prod(x::TArray; dims=:) = prod(get(x); dims=dims) |> localize
Base.prod(f::Union{Function, Type}, x::TArray) = prod(f.(get(x))) |> localize

Base.findfirst(x::TArray, args...) = findfirst(get(x), args...) |> localize
Base.maximum(x::TArray; dims = :) = maximum(get(x), dims = dims) |> localize
Base.minimum(x::TArray; dims = :) = minimum(get(x), dims = dims) |> localize

Base.:/(x::TArray, y::TArray) = get(x) / get(y) |> localize
Base.:/(x::AbstractArray, y::TArray) = x / get(y) |> localize
Base.:/(x::TArray, y::AbstractArray) = get(x) / y |> localize
Base.:\(x::TArray, y::TArray) = get(x) \ get(y) |> localize
Base.:\(x::AbstractArray, y::TArray) = x \ get(y) |> localize
Base.:\(x::TArray, y::AbstractArray) = get(x) \ y |> localize
Base.:*(x::TArray, y::TArray) = get(x) * get(y) |> localize
Base.:*(x::AbstractArray, y::TArray) = x * get(y) |> localize
Base.:*(x::TArray, y::AbstractArray) = get(x) * y |> localize


import LinearAlgebra
import LinearAlgebra:  \, /, inv, det, logdet, logabsdet, norm

LinearAlgebra.inv(x::TArray) = inv(get(x)) |> localize
LinearAlgebra.det(x::TArray) = det(get(x)) |> localize
LinearAlgebra.logdet(x::TArray) = logdet(get(x)) |> localize
LinearAlgebra.logabsdet(x::TArray) = logabsdet(get(x)) |> localize
LinearAlgebra.norm(x::TArray, p::Real = 2) =
    LinearAlgebra.norm(get(x), p) |> localize

import LinearAlgebra: dot
dot(x::TArray, ys::TArray) = dot(get(x), get(ys)) |> localize
dot(x::AbstractArray, ys::TArray) = dot(x, get(ys)) |> localize
dot(x::TArray, ys::AbstractArray) = dot(get(x), ys) |> localize

using Statistics
Statistics.mean(x::TArray; dims = :) = mean(get(x), dims = dims) |> localize
Statistics.std(x::TArray; kw...) = std(get(x), kw...) |> localize

# TODO
# * NNlib
