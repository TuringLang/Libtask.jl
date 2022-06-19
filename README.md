# Libtask

[![Libtask Testing](https://github.com/TuringLang/Libtask.jl/workflows/Libtask%20Testing/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions?branch=master)

Tape based task copying in Turing

## Getting Started

Stack allocated objects are deep copied:

```julia
using Libtask

function f()
  t = 0
  for _ in 1:10
    produce(t)
    t = 1 + t
  end
end

ttask = TapedTask(f)

@show consume(ttask) # 0
@show consume(ttask) # 1

a = copy(ttask)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ttask) # 2
@show consume(ttask) # 3
```

Heap allocated objects are shallow copied:

```julia
using Libtask

function f()
  t = [0 1 2]
  for _ in 1:10
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

ttask = TapedTask(f)

@show consume(ttask) # 0
@show consume(ttask) # 1

a = copy(t)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ttask) # 4
@show consume(ttask) # 5
```

In constrast to standard arrays, which are only shallow copied during
task copying, `TArray`, an array data structure provided by Libtask,
is deep copied during the copying process of a task:

```julia
using Libtask

function f()
  t = TArray(Int, 1)
  t[1] = 0
  for _ in 1:10
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

ttask = TapedTask(f)

@show consume(ttask) # 0
@show consume(ttask) # 1

a = copy(ttask)
@show consume(a) # 2
@show consume(a) # 3

@show consume(ttask) # 2
@show consume(ttask) # 3
```

Notes: 

- The [Turing](https://github.com/TuringLang/Turing.jl) probabilistic programming 
language uses this task copying feature in
an efficient implementation of the [particle
filtering](https://en.wikipedia.org/wiki/Particle_filter) sampling
algorithm for arbitary order [Markov
processes](https://en.wikipedia.org/wiki/Markov_model#Hidden_Markov_model).

- From v0.6.0, Libtask is implemented by recording all the computing
  to a tape and copying that tape. Before that version, it is based on
  a tricky hack on the Julia internals. You can check the commit
  history of this repo to see the details.
