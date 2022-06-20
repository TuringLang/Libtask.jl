##########
# TArray #
##########

"""
    TArray{T}(dims, ...)

Implementation of data structures that automatically perform deepcopy during task copying.

More specifically, we store the underlying arrays in the field
`TArray.data`, and this filed will be deepcopied while we are copying
a task who manipulates it.

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
mutable struct TArray{T, N, A <: AbstractArray{T, N}} <: AbstractArray{T, N}
    data::A

    function TArray(data::A) where {T, N, A <: AbstractArray{T, N}}
        Base.depwarn("`TArray` is deprecated, please overload `tape_copy` for `Array` and use `Array` instead.", :TArray)
        new{T, N, A}(data)
    end
end

TArray{T}(d::Integer...) where T = TArray(T, d)
TArray{T}(::UndefInitializer, d::Integer...) where T = TArray(T, d)
TArray{T}(::UndefInitializer, dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)
TArray{T,N}(d::Vararg{<:Integer,N}) where {T,N} = TArray(T, d)
TArray{T,N}(::UndefInitializer, d::Vararg{<:Integer,N}) where {T,N} = TArray{T,N}(d)
TArray{T,N}(dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)
TArray(T::Type, dim) = TArray(Array{T}(undef, dim))

localize(x) = x
localize(x::AbstractArray) = TArray(x)
getdata(x) = x
getdata(x::TArray) = x.data
tape_copy(x::TArray) = TArray(deepcopy(x.data))


# Constructors
"""
     tzeros(dims, ...)

Construct a TArray of zeros.
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
    d = zeros(T, dim)
    TArray(d)
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
    d = fill(val, dim)
    TArray(d)
end

#
# Conversion between TArray and Array
#

function Base.convert(::Type{Array}, x::TArray)
    convert(Array{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{Array{T, N}}, x::TArray{T, N}) where {T, N}
    convert(Array{T, N}, getdata(x))
end

function Base.convert(::Type{TArray}, x::AbstractArray)
    convert(TArray{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{TArray{T, N}}, x::AbstractArray{T, N}) where {T, N}
    TArray(x)
end

#
# Representation
#
function Base.show(io::IO, ::MIME"text/plain", x::TArray)
    show(io,  MIME("text/plain"), getdata(x))
end

Base.show(io::IO, x::TArray) = show(io, getdata(x))

function Base.summary(io::IO, x::TArray)
    print(io, "TArray: ")
    summary(io, getdata(x))
end

#
# Forward many methods to the underlying array
#
for F in (:size,
          :iterate,
          :firstindex, :lastindex, :axes)
    @eval Base.$F(a::TArray, args...) = $F(getdata(a), args...)
end

#
# Similarity implementation
#

Base.similar(x::TArray, ::Type{T}, dims::Dims) where T = TArray(similar(getdata(x), T, dims))

for op in [:(==), :≈]
    @eval Base.$op(x::TArray, y::AbstractArray) = Base.$op(getdata(x), y)
    @eval Base.$op(x::AbstractArray, y::TArray) = Base.$op(x, getdata(y))
    @eval Base.$op(x::TArray, y::TArray) = Base.$op(getdata(x), getdata(y))
end

#
# Array Stdlib
#

# Indexing Interface
Base.@propagate_inbounds function Base.getindex(x::TArray{T, N}, I::Vararg{Int,N}) where {T, N}
    return getdata(x)[I...]
end

Base.@propagate_inbounds function Base.setindex!(x::TArray{T, N}, e, I::Vararg{Int,N}) where {T, N}
    getdata(x)[I...] = e
end

function Base.push!(x::TArray, e)
    push!(getdata(x), e)
end

function Base.pop!(x::TArray)
    pop!(getdata(x))
end

# Other methods from stdlib

Base.view(x::TArray, inds...; kwargs...) =
    Base.view(getdata(x), inds...; kwargs...) |> localize
Base.:-(x::TArray) = (-getdata(x)) |> localize
Base.transpose(x::TArray) = transpose(getdata(x)) |> localize
Base.adjoint(x::TArray) = adjoint(getdata(x)) |> localize
Base.repeat(x::TArray; kw...) = repeat(getdata(x); kw...) |> localize

Base.hcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    hcat(getdata.(xs)...) |> localize
Base.vcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    vcat(getdata.(xs)...) |> localize
Base.cat(xs::Union{TArray{T,1}, TArray{T,2}}...; dims) where T =
    cat(getdata.(xs)...; dims = dims) |> localize


Base.reshape(x::TArray, dims::Union{Colon,Int}...) = reshape(getdata(x), dims) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Union{Int,Colon}}}) =
    reshape(getdata(x), Base._reshape_uncolon(getdata(x), dims)) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Int}}) = reshape(getdata(x), dims) |> localize

Base.permutedims(x::TArray, perm) = permutedims(getdata(x), perm) |> localize
Base.PermutedDimsArray(x::TArray, perm) = PermutedDimsArray(getdata(x), perm) |> localize
Base.reverse(x::TArray; dims) = reverse(getdata(x), dims = dims) |> localize

Base.sum(x::TArray; dims = :) = sum(getdata(x), dims = dims) |> localize
Base.sum(f::Union{Function,Type},x::TArray) = sum(f.(getdata(x))) |> localize
Base.prod(x::TArray; dims=:) = prod(getdata(x); dims=dims) |> localize
Base.prod(f::Union{Function, Type}, x::TArray) = prod(f.(getdata(x))) |> localize

Base.findfirst(x::TArray, args...) = findfirst(getdata(x), args...) |> localize
Base.maximum(x::TArray; dims = :) = maximum(getdata(x), dims = dims) |> localize
Base.minimum(x::TArray; dims = :) = minimum(getdata(x), dims = dims) |> localize

Base.:/(x::TArray, y::TArray) = getdata(x) / getdata(y) |> localize
Base.:/(x::AbstractArray, y::TArray) = x / getdata(y) |> localize
Base.:/(x::TArray, y::AbstractArray) = getdata(x) / y |> localize
Base.:\(x::TArray, y::TArray) = getdata(x) \ getdata(y) |> localize
Base.:\(x::AbstractArray, y::TArray) = x \ getdata(y) |> localize
Base.:\(x::TArray, y::AbstractArray) = getdata(x) \ y |> localize
Base.:*(x::TArray, y::TArray) = getdata(x) * getdata(y) |> localize
Base.:*(x::AbstractArray, y::TArray) = x * getdata(y) |> localize
Base.:*(x::TArray, y::AbstractArray) = getdata(x) * y |> localize

# broadcast
Base.BroadcastStyle(::Type{<:TArray}) = Broadcast.ArrayStyle{TArray}()
Broadcast.broadcasted(::Broadcast.ArrayStyle{TArray}, f, args...) = f.(getdata.(args)...) |> localize

import LinearAlgebra
import LinearAlgebra:  \, /, inv, det, logdet, logabsdet, norm

LinearAlgebra.inv(x::TArray) = inv(getdata(x)) |> localize
LinearAlgebra.det(x::TArray) = det(getdata(x)) |> localize
LinearAlgebra.logdet(x::TArray) = logdet(getdata(x)) |> localize
LinearAlgebra.logabsdet(x::TArray) = logabsdet(getdata(x)) |> localize
LinearAlgebra.norm(x::TArray, p::Real = 2) =
    LinearAlgebra.norm(getdata(x), p) |> localize

import LinearAlgebra: dot
dot(x::TArray, ys::TArray) = dot(getdata(x), getdata(ys)) |> localize
dot(x::AbstractArray, ys::TArray) = dot(x, getdata(ys)) |> localize
dot(x::TArray, ys::AbstractArray) = dot(getdata(x), ys) |> localize

using Statistics
Statistics.mean(x::TArray; dims = :) = mean(getdata(x), dims = dims) |> localize
Statistics.std(x::TArray; kw...) = std(getdata(x), kw...) |> localize

# TODO
# * NNlib
