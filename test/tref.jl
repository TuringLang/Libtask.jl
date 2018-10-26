using Libtask
using Test

function f_cta()
  t = TRef(1);
  t[] = 0;
  while true
    produce(t[])
    t[]
    t[] += 1
  end
end

t = Task(f_cta)

consume(t); consume(t)
a = copy(t);
consume(a); consume(a)

Base.@assert consume(t) == 2
Base.@assert consume(a) == 4

x = TRef([1 2 3; 4 5 6])
x[1, 3] = 900
# display(x)

y = TRef([1,2,3])
y[2] = 19
# display(y)

z = TRef(19.2)
z[] += 31
# display(z)

m = get(x)
display(m)
