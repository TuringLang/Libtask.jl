module TestUtils

using ..Libtask
using Test
using ..Libtask: CopyableTask

struct Testcase
    name::String
    fargs::Tuple
    expected_iteration_results::Vector
end

function (case::Testcase)()
    testset = @testset "$(case.name)" begin

        # Construct the task.
        t = CopyableTask(case.fargs...)

        # Iterate through t. Record the results, and take a copy after each iteration.
        iteration_results = []
        t_copies = [deepcopy(t)]
        for val in t
            push!(iteration_results, val)
            push!(t_copies, deepcopy(t))
        end

        # Check that iterating the original task gives the expected results.
        @test iteration_results == case.expected_iteration_results

        # Check that iterating the copies yields the correct results.
        for (n, t_copy) in enumerate(t_copies)
            @test iteration_results[n:end] == collect(t_copy)
        end
    end
    return testset
end

function test_cases()
    return Testcase[Testcase(
        "single block",
        (single_block, 5.0),
        [sin(5.0), sin(sin(5.0)), sin(sin(sin(5.0))), sin(sin(sin(sin(5.0))))],
    ),
    Testcase("no produce", (no_produce_test, 5.0, 4.0), []),
    Testcase("new object", (new_object_test, 5, 4), [C(5, 4)]),
    Testcase("branching test l", (branching_test, 5.0, 4.0), [string(sin(5.0))]),
    Testcase("branching test r", (branching_test, 4.0, 5.0), [sin(4.0) * cos(5.0)]),
    Testcase("unused argument test", (unused_argument_test, 3), [1]),
    Testcase("test with const", (test_with_const, ), [1]),
    Testcase("nested", (nested_outer, ), [true, false]),
]
end

function single_block(x::Float64)
    x1 = sin(x)
    produce(x1)
    x2 = sin(x1)
    produce(x2)
    x3 = sin(x2)
    produce(x3)
    x4 = sin(x3)
    produce(x4)
    return cos(x4)
end

function no_produce_test(x, y)
    c = x + y
    return (; c, x, y)
end

# Old test case without any produce statements used to test TapedFunction. Since this
# doesn't exist as a distinct entity anymore, not clear that this test case is useful.
mutable struct C
    i::Int
    C(x, y) = new(x + y)
end

Base.:(==)(c::C, d::C) = c.i == d.i

function new_object_test(x, y)
    produce(C(x, y))
    return nothing
end

function branching_test(x, y)
    if x > y
        produce(string(sin(x)))
    else
        produce(sin(x) * cos(y))
    end
    return nothing
end

function unused_argument_test(x)
    produce(1)
    return nothing
end

function test_with_const()
    # this line generates: %1 = 1::Core.Const(1)
    r = (a = 1)
    produce(r)
    return nothing
end

@noinline function nested_inner()
    produce(true)
    return nothing
end

might_produce(::Type{Tuple{typeof(nested_inner)}}) = true

function nested_outer()
    nested_inner()
    produce(false)
    return nothing
end

end
