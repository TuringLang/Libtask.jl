# Libtask
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

# Heap allocated objects are shallowly copied.

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
