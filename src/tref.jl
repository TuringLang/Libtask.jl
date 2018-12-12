##########
# TRef #
##########

"""
    TRef(x)

Implementation of an abstract data structure that
automatically performs copy-on-write after task copying.

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
struct TRef
    ref :: Symbol  # object_id
    orig_task :: Task
    TRef() = new(gensym(), current_task())
end

function TRef(x)
    res = TRef();
    n = n_copies()
    task_local_storage(res.ref, (n,Ref(x)))
    return res
end

function Base.getindex(S::TRef, I::Vararg{Any,N}) where {N}
    _, d = task_local_storage(S.ref)
    return d[][I...]
end

function Base.setindex!(S::TRef, x, I::Vararg{Any,N}) where {N}
    n, d = task_local_storage(S.ref)
    cn   = n_copies()
    newd = d
    if cn > n
        # println("[setindex!]: $(S.ref) copying data")
        newd = deepcopy(d)
        task_local_storage(S.ref, (cn, newd))
    end

    if isa(newd[], Real)
        newd[] = x
    else
        setindex!(newd[], x, I...)
    end
    return newd[]
end

function Base.display(S::TRef)
    display("Please use show(::TRef) instead.")
end

Base.show(io::IO, S::TRef) = Base.show(io::IO, task_local_storage(S.ref)[2][])
Base.size(S::TRef) = Base.size(task_local_storage(S.ref)[2][])
Base.ndims(S::TRef) = Base.ndims(task_local_storage(S.ref)[2][])

Base.get(S::TRef) = (current_task().storage[S.ref][2][])

# Implements eltype, firstindex, lastindex, and iterate
# functions.
for F in (:eltype, :firstindex, :lastindex, :iterate)
    @eval Base.$F(a::TRef, args...) = $F(get(a), args...)
end
