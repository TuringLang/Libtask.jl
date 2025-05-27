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

## Manipulation of [`TapedTask`](@ref)s:
```@docs; canonical=true
Libtask.consume
Base.copy(::Libtask.TapedTask)
Libtask.set_taped_globals!
```

An opt-in mechanism marks functions that might contain `Libtask.produce` statements. 

```@docs; canonical=true
Libtask.might_produce(::Type{<:Tuple})
```

## Functions for use inside a [`TapedTask`](@ref)s:
```@docs; canonical=true
Libtask.produce
Libtask.get_taped_globals
```
