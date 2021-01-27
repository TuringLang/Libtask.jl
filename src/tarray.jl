##########
# TArray #
##########

"""
    TArray{T}(dims, ...)

Implementation of data structures that automatically perform copy-on-write after task copying.

For each `TArray` object, we have a task-local array object, i.e. if
you access (read or write) the same `TArray` object in different
tasks, you may be dealing with different array objects each belonging
to a different task.  These task-specific array objects, however,
share the same parent `TArray` object.

More specifically, we store the underlying arrays in the field
`TArray.data`, a dictionary whose keys are tasks, instead of storing
them in the task local storage, for performance considerations.

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
        register_to_keeper(res)
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
    _set_local_storage(res, n, d)
    res
end

TArray(x::AbstractArray) = convert(TArray, x)

# TArray House-Keeper

const TArrayKeeper = Vector{WeakRef}()
register_to_keeper(x::TArray) = push!(TArrayKeeper, WeakRef(x))
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

_get_local_storage(x) = x
function _get_local_storage(x::TArray; copydata=false)
    n, d = x.data[current_task()]
    copydata || return d
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        _set_local_storage(x, cn, newd)
    end
    return newd
end

function _set_local_storage(x::TArray{T, N}, n_copies::Int, d::AbstractArray{T, N}) where {T, N}
    x.data[current_task()] = (n_copies, d)
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
    _set_local_storage(res, n, d)
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
    _set_local_storage(res, n, d)
    return res
end

#
# Conversion between TArray and Array
#

function Base.convert(::Type{Array}, x::TArray)
    return convert(Array{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{Array{T, N}}, x::TArray{T, N}) where {T, N}
    c = convert(Array{T, N}, deepcopy(_get_local_storage(x)))
    return c
end

function Base.convert(::Type{TArray}, x::AbstractArray)
    return convert(TArray{eltype(x), ndims(x)}, x)
end
function Base.convert(::Type{TArray{T, N}}, x::AbstractArray{T, N}) where {T, N}
    res = TArray{T, N, typeof(x)}()
    n   = n_copies()
    _set_local_storage(res, n, x)
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

Base.show(io::IO, x::TArray) = show(io, _get_local_storage(x))

function Base.summary(io::IO, x::TArray)
  print(io, "Task Local Array: ")
  summary(io, _get_local_storage(x))
end

#
# Forward many methods to the underlying array
#
for F in (:size,
          :iterate,
          :firstindex, :lastindex, :axes)
    @eval Base.$F(a::TArray, args...) = $F(_get_local_storage(a), args...)
end

#
# Similarity implementation
#

Base.similar(x::TArray, ::Type{T}, dims::Dims) where T = TArray(similar(_get_local_storage(x), T, dims))

for op in [:(==), :≈]
    @eval Base.$op(x::TArray, y::AbstractArray) = Base.$op(_get_local_storage(x), y)
    @eval Base.$op(x::AbstractArray, y::TArray) = Base.$op(x, _get_local_storage(y))
    @eval Base.$op(x::TArray, y::TArray) = Base.$op(_get_local_storage(x), _get_local_storage(y))
end

#
# Array Stdlib
#

# Indexing Interface
Base.@propagate_inbounds function Base.getindex(x::TArray{T, N}, I::Vararg{Int,N}) where {T, N}
    return _get_local_storage(x)[I...]
end

Base.@propagate_inbounds function Base.setindex!(x::TArray{T, N}, e, I::Vararg{Int,N}) where {T, N}
    d = _get_local_storage(x; copydata=true)
    d[I...] = e
end

function Base.push!(x::TArray, e)
    d = _get_local_storage(x; copydata=true)
    push!(d, e)
end

function Base.pop!(x::TArray)
    d = _get_local_storage(x; copydata=true)
    pop!(d)
end

# Other methods from stdlib

Base.view(x::TArray, inds...; kwargs...) =
    Base.view(_get_local_storage(x), inds...; kwargs...) |> localize
Base.:-(x::TArray) = (- _get_local_storage(x)) |> localize
Base.transpose(x::TArray) = transpose(_get_local_storage(x)) |> localize
Base.adjoint(x::TArray) = adjoint(_get_local_storage(x)) |> localize
Base.repeat(x::TArray; kw...) = repeat(_get_local_storage(x); kw...) |> localize

Base.hcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    hcat(_get_local_storage.(xs)...) |> localize
Base.vcat(xs::Union{TArray{T,1}, TArray{T,2}}...) where T =
    vcat(_get_local_storage.(xs)...) |> localize
Base.cat(xs::Union{TArray{T,1}, TArray{T,2}}...; dims) where T =
    cat(_get_local_storage.(xs)...; dims = dims) |> localize


Base.reshape(x::TArray, dims::Union{Colon,Int}...) = reshape(_get_local_storage(x), dims) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Union{Int,Colon}}}) =
    reshape(_get_local_storage(x), Base._reshape_uncolon(_get_local_storage(x), dims)) |> localize
Base.reshape(x::TArray, dims::Tuple{Vararg{Int}}) = reshape(_get_local_storage(x), dims) |> localize

Base.permutedims(x::TArray, perm) = permutedims(_get_local_storage(x), perm) |> localize
Base.PermutedDimsArray(x::TArray, perm) = PermutedDimsArray(_get_local_storage(x), perm) |> localize
Base.reverse(x::TArray; dims) = reverse(_get_local_storage(x), dims = dims) |> localize

Base.sum(x::TArray; dims = :) = sum(_get_local_storage(x), dims = dims) |> localize
Base.sum(f::Union{Function,Type},x::TArray) = sum(f.(_get_local_storage(x))) |> localize
Base.prod(x::TArray; dims=:) = prod(_get_local_storage(x); dims=dims) |> localize
Base.prod(f::Union{Function, Type}, x::TArray) = prod(f.(_get_local_storage(x))) |> localize

Base.findfirst(x::TArray, args...) = findfirst(_get_local_storage(x), args...) |> localize
Base.maximum(x::TArray; dims = :) = maximum(_get_local_storage(x), dims = dims) |> localize
Base.minimum(x::TArray; dims = :) = minimum(_get_local_storage(x), dims = dims) |> localize

Base.:/(x::TArray, y::TArray) = _get_local_storage(x) / _get_local_storage(y) |> localize
Base.:/(x::AbstractArray, y::TArray) = x / _get_local_storage(y) |> localize
Base.:/(x::TArray, y::AbstractArray) = _get_local_storage(x) / y |> localize
Base.:\(x::TArray, y::TArray) = _get_local_storage(x) \ _get_local_storage(y) |> localize
Base.:\(x::AbstractArray, y::TArray) = x \ _get_local_storage(y) |> localize
Base.:\(x::TArray, y::AbstractArray) = _get_local_storage(x) \ y |> localize
Base.:*(x::TArray, y::TArray) = _get_local_storage(x) * _get_local_storage(y) |> localize
Base.:*(x::AbstractArray, y::TArray) = x * _get_local_storage(y) |> localize
Base.:*(x::TArray, y::AbstractArray) = _get_local_storage(x) * y |> localize

# broadcast
Base.BroadcastStyle(::Type{<:TArray}) = Broadcast.ArrayStyle{TArray}()
Broadcast.broadcasted(::Broadcast.ArrayStyle{TArray}, f, args...) = f.(_get_local_storage.(args)...) |> localize

import LinearAlgebra
import LinearAlgebra:  \, /, inv, det, logdet, logabsdet, norm

LinearAlgebra.inv(x::TArray) = inv(_get_local_storage(x)) |> localize
LinearAlgebra.det(x::TArray) = det(_get_local_storage(x)) |> localize
LinearAlgebra.logdet(x::TArray) = logdet(_get_local_storage(x)) |> localize
LinearAlgebra.logabsdet(x::TArray) = logabsdet(_get_local_storage(x)) |> localize
LinearAlgebra.norm(x::TArray, p::Real = 2) =
    LinearAlgebra.norm(_get_local_storage(x), p) |> localize

import LinearAlgebra: dot
dot(x::TArray, ys::TArray) = dot(_get_local_storage(x), _get_local_storage(ys)) |> localize
dot(x::AbstractArray, ys::TArray) = dot(x, _get_local_storage(ys)) |> localize
dot(x::TArray, ys::AbstractArray) = dot(_get_local_storage(x), ys) |> localize

using Statistics
Statistics.mean(x::TArray; dims = :) = mean(_get_local_storage(x), dims = dims) |> localize
Statistics.std(x::TArray; kw...) = std(_get_local_storage(x), kw...) |> localize

# TODO
# * NNlib
