# Libtask

Libtask is best explained by the docstring for [`TapedTask`](@ref):
```@docs; canonical=true
Libtask.TapedTask
```

The functions discussed the above docstring (in addition to [`TapedTask`](@ref) itself) form the
public interface of Libtask.jl.
They divide neatly into two kinds of functions: those which are used to manipulate
[`TapedTask`](@ref)s, and those which are intended to be used _inside_ a
[`TapedTask`](@ref).
First, manipulation of [`TapedTask`](@ref)s:
```@docs; canonical=true
Libtask.consume
Base.copy(::Libtask.TapedTask)
Libtask.set_taped_globals!
```

Functions for use inside a [`TapedTask`](@ref)s are:
```@docs; canonical=true
Libtask.produce
Libtask.get_taped_globals
```
