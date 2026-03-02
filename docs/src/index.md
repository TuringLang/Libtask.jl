# Libtask

Libtask is best explained by the docstring for [`TapedTask`](@ref):
```@docs; canonical=true
Libtask.TapedTask
```

The functions discussed in the above docstring (in addition to [`TapedTask`](@ref) itself) form the
public interface of Libtask.jl.
They divide neatly into two kinds of functions: those which are used to manipulate
[`TapedTask`](@ref)s, and those which are intended to be used _inside_ a
[`TapedTask`](@ref).

## Manipulation of [`TapedTask`](@ref)s
```@docs; canonical=true
Libtask.consume
Base.copy(::Libtask.TapedTask)
Libtask.get_taped_globals(::Libtask.TapedTask)
Libtask.set_taped_globals!
```

## Functions for use inside a [`TapedTask`](@ref)s
```@docs; canonical=true
Libtask.produce
Libtask.get_taped_globals(::Type{T}) where {T}
Libtask.NotInTapedTaskError
```

An opt-in mechanism marks functions that might contain `Libtask.produce` statements.

```@docs; canonical=true
Libtask.might_produce(::Type{<:Tuple})
Libtask.@might_produce
Libtask.might_produce_if_sig_contains
```
