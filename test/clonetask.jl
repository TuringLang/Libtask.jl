# Test task copying

using Libtask
using Test

# Test case 1: stack allocated objects are deep copied.
function f_ct()
  t = 0;
  while true
    produce(t)
    t = 1 + t
  end
end

t = CTask(f_ct)

@test consume(t) == 0
@test consume(t) == 1
a = copy(t);
@test consume(a) == 2
@test consume(a) == 3
@test consume(t) == 2
@test consume(t) == 3

# Test case 2: heap allocated objects are shallowly copied.

function f_ct2()
  t = [0 1 2];
  while true
    produce(t[1])
    t[1] = 1 + t[1]
  end
end

t = CTask(f_ct2)

@test consume(t) == 0
@test consume(t) == 1
a = copy(t);
@test consume(a) == 2
@test consume(a) == 3
@test consume(t) == 4
@test consume(t) == 5
@test consume(a) == 6
@test consume(a) == 7


# Breaking test
function g_break()
    t = 0
    while true
        t[3] = 1
        produce(t)
        t = t + 1
    end
end

t = CTask(g_break)
@test_throws MethodError consume(t)
