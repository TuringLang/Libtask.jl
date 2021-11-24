##########
#  TRef  #
##########

"""
    TRef(x)

Implementation of an abstract data structure that
automatically performs deepcopy during task copying.

Atomic (single-valued) TRef objects must be set or updated
by indexing. For example, to access `val = TRef(1)`, you
must use `val[]`.

Usage:

```julia
TRef(x)
```

Example:

```julia
# Initialize an atomic value
z = TRef(19.2)
z[] += 31

# Initialize a multi-index object
x = TRef([1 2 3; 4 5 6])
x[1, 3] = 999

# Initialize a TRef holding a dictionary.
d = TRef(Dict("A" => 1, 5 => "B"))
d["A"] = 10
```
"""
mutable struct TRef{T}
    val::T
end

Base.get(r::TRef) = r.val
Base.show(io::IO, r::TRef) = Base.show(io::IO, get(r))
Base.size(r::TRef) = Base.size(get(r))
Base.ndims(r::TRef) = Base.ndims(get(r))
tape_copy(r::TRef) = TRef(deepcopy(get(r)))

function Base.getindex(r::TRef, I::Vararg{Any,N}) where {N}
    return get(r)[I...]
end

function Base.setindex!(r::TRef, x, I::Vararg{Any,N}) where {N}
    d = get(r)
    if isa(d, Real)
        r.val = x
    else
        setindex!(d, x, I...)
    end
    return d
end


# Implements eltype, firstindex, lastindex, and iterate
# functions.
for F in (:eltype, :firstindex, :lastindex, :iterate)
    @eval Base.$F(a::TRef, args...) = $F(get(a), args...)
end
