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

TArray{T,1}(d::Integer) where T = TArray(T,  d)
TArray{T}(d::Integer...) where T = TArray(T, d)
TArray{T}(UndefInitializer, d::Integer...) where T = TArray(T, d)
TArray{T,N}(d::Integer...) where {T,N} = length(d)==N ? TArray(T,d) : error("Malformed dims")
TArray{T,N}(UndefInitializer, d::Integer...) where {T,N} = length(d)==N ? TArray(T,d) : error("Malformed dims")
TArray{T,N}(dim::NTuple{N,Int}) where {T,N} = TArray(T, dim)

function TArray(T::Type, dim)
    res = TArray{T,length(dim)}();
    n = n_copies()
    d = Array{T}(undef, dim)
    task_local_storage(res.ref, (n,d))
    res
end

#
# Indexing Interface Implementation
#

function Base.getindex(S::TArray{T, N}, I::Vararg{Int,N}) where {T, N}
    t, d = task_local_storage(S.ref)
    return d[I...]
end

function Base.setindex!(S::TArray{T, N}, x, I::Vararg{Int,N}) where {T, N}
    n, d = task_local_storage(S.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        # println("[setindex!]: $(S.ref) copying data")
        newd = deepcopy(d)
        task_local_storage(S.ref, (cn, newd))
    end
    newd[I...] = x
end

function Base.push!(S::TArray, x)
    n, d = task_local_storage(S.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(S.ref, (cn, newd))
    end
    push!(newd, x)
end

function Base.pop!(S::TArray)
    n, d = task_local_storage(S.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        newd = deepcopy(d)
        task_local_storage(S.ref, (cn, newd))
    end
    pop!(d)
end

function Base.convert(::Type{TArray}, x::Array)
    res = TArray{typeof(x[1]),ndims(x)}();
    n   = n_copies()
    task_local_storage(res.ref, (n,x))
    return res
end

function Base.convert(::Array, x::Type{TArray})
    n,d = task_local_storage(S.ref)
    c = deepcopy(d)
    return c
end

function Base.display(S::TArray)
    arr = S.orig_task.storage[S.ref][2]
    @warn "display(::TArray) prints the originating task's storage, not the current task's storage. Please use show(::TArray) to display the current task's version of a TArray."
    display(arr)
end

Base.show(io::IO, S::TArray) = Base.show(io::IO, task_local_storage(S.ref)[2])
Base.size(S::TArray) = Base.size(task_local_storage(S.ref)[2])
Base.ndims(S::TArray) = Base.ndims(task_local_storage(S.ref)[2])

# Base.get(t::Task, S) = S
# Base.get(t::Task, S::TArray) = (t.storage[S.ref][2])
Base.get(S::TArray) = (current_task().storage[S.ref][2])

# Implements eltype, firstindex, lastindex, and iterate
# functions.
for F in (:eltype, :firstindex, :lastindex, :iterate)
    @eval Base.$F(a::TArray, args...) = $F(get(a), args...)
end

#
# Similarity implementation
#

Base.similar(S::TArray) = tzeros(eltype(S), size(S))
Base.similar(S::TArray, ::Type{T}) where {T} = tzeros(T, size(S))
Base.similar(S::TArray, dims::Dims) = tzeros(eltype(S), dims)

##########
# tzeros #
##########

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
