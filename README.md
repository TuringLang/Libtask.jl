# Libtask
[![Build Status](https://travis-ci.org/TuringLang/Libtask.jl.svg?branch=master)](https://travis-ci.org/TuringLang/Libtask.jl)
[![Build status](https://ci.appveyor.com/api/projects/status/9oj4lh1bdya2ofjm/branch/master?svg=true)](https://ci.appveyor.com/project/TuringLang/libtask-jl/branch/master)

C shim for [task copying](https://github.com/JuliaLang/julia/issues/4085) in Turing

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

t = Task(f_ct)

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

t = Task(f_ct2)

consume(t) == 0
consume(t) == 1
a = copy(t);
consume(a) == 2
consume(a) == 3
consume(t) == 4
consume(t) == 5

# TArray objects are deep copied. 

function f_cta()
  t = TArray(Int, 1);
  t[1] = 0;
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

t = Task(f_cta)

consume(t) == 0
consume(t) == 1
a = copy(t);
consume(a) == 2
consume(a) == 3
consume(t) == 2
consume(t) == 3
```

Disclaimer: Task copying is experimental and should only be used with caution. Some discussions on this topic can be found [here](https://github.com/JuliaLang/julia/pull/15078).
