module TestResources

# Old test case without any produce statements used to test TapedFunction. Since this
# doesn't exist as a distinct entity anymore, not clear that this test case is useful.
mutable struct S
    i::Int
    S(x, y) = new(x + y)
end

# Old test case without any produce statements. Might make sense to ensure that something
# vaguely like this is included in the test suite, but isn't directly relevant.
function g(x, y)
    if x>y
        r = string(sin(x))
    else
        r = sin(x) * cos(y)
    end
    return r
end

# Old test case -- github.com/TuringLang/Libtask.jl/issues/148, unused argument
function f(x)
    produce(1)
end

# Old test case. Probably redundant, but makes sense to check. Might want to replace the
# final statement with a produce statement to make the test case meaningful.
function g(x, y)
    c = x + y
    return (; c, x, y)
end

# Make sure I provide a test case in which a function contains consts.
function f()
    # this line generates: %1 = 1::Core.Const(1)
    r = (a = 1)
    return nothing
end

end
