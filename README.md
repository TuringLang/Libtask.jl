# Libtask
[![Libtask Testing](https://github.com/TuringLang/Libtask.jl/workflows/Libtask%20Testing/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions)
[![Dylib Build](https://github.com/TuringLang/Libtask.jl/workflows/Build%20Dylib/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions)


C shim for [task copying](https://github.com/JuliaLang/julia/issues/4085) in Turing

## Getting Started

```julia
using Libtask

# Stack allocated objects are deep copied.
function f_ct()
  t = 0;
  while true
    produce(t)
    t = 1 + t
  end
end

t = CTask(f_ct)

consume(t) == 0
consume(t) == 1
a = copy(t);
consume(a) == 2
consume(a) == 3
consume(t) == 2
consume(t) == 3

# Heap allocated objects are shallow copied.

function f_ct2()
  t = [0 1 2];
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

t = CTask(f_ct2)

consume(t) == 0
consume(t) == 1
a = copy(t);
consume(a) == 2
consume(a) == 3
consume(t) == 4
consume(t) == 5

# `TArray` implements a copy-on-write array. This is useful for task copying.
#  In constrast to standard arrays, which are only shallow copied during task copying,
#  `TArray` are deep copied after task copying.

function f_cta()
  t = TArray(Int, 1);
  t[1] = 0;
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

t = CTask(f_cta)

consume(t) == 0
consume(t) == 1
a = copy(t);
consume(a) == 2
consume(a) == 3
consume(t) == 2
consume(t) == 3
```

Note: The [Turing](https://github.com/TuringLang/Turing.jl) probabilistic programming language uses this task copying feature in an efficient implementation of the [particle filtering](https://en.wikipedia.org/wiki/Particle_filter) sampling algorithm for arbitary order [Markov processes](https://en.wikipedia.org/wiki/Markov_model#Hidden_Markov_model).

Disclaimer: This feature is still experimental and should only be used with caution. Some discussions on its potential caveats can be found [here](https://github.com/JuliaLang/julia/pull/15078).

## For Developer

### Release a new version
    Update the new version number in `Project.toml`, a tag to that
    version will be made by the GitHub Actions after it is pushed.
