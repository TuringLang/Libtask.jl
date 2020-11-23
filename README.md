# Libtask

[![Libtask Testing](https://github.com/TuringLang/Libtask.jl/workflows/Libtask%20Testing/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions?branch=master)

C shim for [task copying](https://github.com/JuliaLang/julia/issues/4085) in Turing

## Getting Started

Stack allocated objects are deep copied:

```julia
using Libtask

function f()
  t = 0
  while true
    produce(t)
    t = 1 + t
  end
end

ctask = CTask(f)

@show consume(ctask) # 0
@show consume(ctask) # 1

a = copy(ctask)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ctask) # 2
@show consume(ctask) # 3
```

Heap allocated objects are shallow copied:

```julia
using Libtask

function f()
  t = [0 1 2]
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

ctask = CTask(f)

@show consume(ctask) # 0
@show consume(ctask) # 1

a = copy(t)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ctask) # 4
@show consume(ctask) # 5
```

`TArray` implements a copy-on-write array. This is useful for task copying.
In constrast to standard arrays, which are only shallow copied during task copying,
`TArray` are deep copied after task copying:

```julia
using Libtask

function f()
  t = TArray(Int, 1)
  t[1] = 0
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

ctask = CTask(f)

@show consume(ctask) # 0
@show consume(ctask) # 1

a = copy(ctask)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ctask) # 2
@show consume(ctask) # 3
```

Note: The [Turing](https://github.com/TuringLang/Turing.jl) probabilistic programming language uses this task copying feature in an efficient implementation of the [particle filtering](https://en.wikipedia.org/wiki/Particle_filter) sampling algorithm for arbitary order [Markov processes](https://en.wikipedia.org/wiki/Markov_model#Hidden_Markov_model).

## Disclaimer

This feature is still experimental and should only be used with caution. Some discussions on its potential caveats can be found [here](https://github.com/JuliaLang/julia/pull/15078).

## Julia nightly

Libtask uses the `libtask_julia` library which is pre-built for Julia versions 1.3, 1.4, and 1.5 and
distributed via the [Libtask_jll](https://github.com/JuliaBinaryWrappers/Libtask_jll.jl/) package.

Julia nightly might not be compatible with the latest version of the `libtask_julia` library and is
not officially supported. If you want to use Julia nightly, you have to add the Libtask_jll package
manually:
```julia
julia> ] add https://github.com/JuliaBinaryWrappers/Libtask_jll.jl.git
```
