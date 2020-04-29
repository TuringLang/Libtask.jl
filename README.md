# Libtask
[![Libtask Testing](https://github.com/TuringLang/Libtask.jl/workflows/Libtask%20Testing/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions)
[![Dylib Build](https://github.com/TuringLang/Libtask.jl/workflows/Build%20Dylib/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions)


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

Disclaimer: This feature is still experimental and should only be used with caution. Some discussions on its potential caveats can be found [here](https://github.com/JuliaLang/julia/pull/15078).

## For Developer

### Release a new version
    - If you have updates of the binary dylib (files under the `deps`
      directory), you should:
      - Update the version/commit information in [this
        file](https://github.com/JuliaPackaging/Yggdrasil/blob/master/L/Libtask/build_tarballs.jl)
        by a pull request
      - Wait for that PR being merged, then you will find a new
        release of the dylib
        [here](https://github.com/JuliaBinaryWrappers/Libtask_jll.jl/releases)
      - Then, in the root directory of the Yggdrasil project, run
        `https://github.com/JuliaBinaryWrappers/Libtask_jll.jl/releases`,
        you will get a build file like
        `build/build_Libtask.v0.3.1.jl`. Copy the binary download
        information (the variables `bin_prefix` and `download_info`)
        from that generated file to `deps/build.jl` in our repo and
        commit the changes.
    - If you don't make any changes about the dylib, just ignore the
      first step and go to next one.
    - Update the new version number in `Project.toml`, ping
      @JuliaRegistrator with `@JuliaRegistrator register` in a
      comment.
