using Libtask
using Test

# Test atomic values.
function f_cta()
  t = TRef(1);
  t[] = 0;
  while true
    produce(t[])
    t[]
    t[] += 1
  end
end

t = CTask(f_cta)

consume(t); consume(t)
a = copy(t);
consume(a); consume(a)

Base.@assert consume(t) == 2
Base.@assert consume(a) == 4

# Test dictionary functionality.
function dict_test()
  t = TRef(Dict("A" => 1, 5 => "B"));
  t["A"] = 0;
  while true
    produce(t["A"])
    t["A"]
    t["A"] += 1
  end
end

t = CTask(dict_test)

consume(t); consume(t)
a = copy(t);
consume(a); consume(a)

Base.@assert consume(t) == 2
Base.@assert consume(a) == 4

# Create a TRef storing a matrix.
x = TRef([1 2 3; 4 5 6])
x[1, 3] = 900
Base.@assert x[1,3] == 900

# TRef holding an array.
y = TRef([1,2,3])
y[2] = 19
Base.@assert y[2] == 19
