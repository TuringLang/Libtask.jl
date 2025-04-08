module TestUtils

using ..Libtask
using Test
using ..Libtask: TapedTask

struct Testcase
    name::String
    dynamic_scope::Any
    fargs::Tuple
    kwargs::Union{NamedTuple,Nothing}
    expected_iteration_results::Vector
end

function (case::Testcase)()
    testset = @testset "$(case.name)" begin

        # Construct the task.
        if case.kwargs === nothing
            t = TapedTask(case.dynamic_scope, case.fargs...)
        else
            t = TapedTask(case.dynamic_scope, case.fargs...; case.kwargs...)
        end

        # Iterate through t. Record the results, and take a copy after each iteration.
        iteration_results = []
        t_copies = [copy(t)]
        for val in t
            push!(iteration_results, val)
            push!(t_copies, copy(t))
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
    return Testcase[
        Testcase(
            "single block",
            nothing,
            (single_block, 5.0),
            nothing,
            [sin(5.0), sin(sin(5.0)), sin(sin(sin(5.0))), sin(sin(sin(sin(5.0))))],
        ),
        Testcase(
            "produce old", nothing, (produce_old_value, 5.0), nothing, [sin(5.0), sin(5.0)]
        ),
        Testcase(
            "branch on old value l",
            nothing,
            (branch_on_old_value, 2.0),
            nothing,
            [true, 2.0],
        ),
        Testcase(
            "branch on old value r",
            nothing,
            (branch_on_old_value, -1.0),
            nothing,
            [false, -2.0],
        ),
        Testcase("no produce", nothing, (no_produce_test, 5.0, 4.0), nothing, []),
        Testcase(
            "new object", nothing, (new_object_test, 5, 4), nothing, [C(5, 4), C(5, 4)]
        ),
        Testcase(
            "branching test l",
            nothing,
            (branching_test, 5.0, 4.0),
            nothing,
            [string(sin(5.0))],
        ),
        Testcase(
            "branching test r",
            nothing,
            (branching_test, 4.0, 5.0),
            nothing,
            [sin(4.0) * cos(5.0)],
        ),
        Testcase("unused argument test", nothing, (unused_argument_test, 3), nothing, [1]),
        Testcase("test with const", nothing, (test_with_const,), nothing, [1]),
        Testcase("while loop", nothing, (while_loop,), nothing, collect(1:9)),
        Testcase(
            "foreigncall tester",
            nothing,
            (foreigncall_tester, "hi"),
            nothing,
            [Ptr{UInt8}, Ptr{UInt8}],
        ),
        Testcase("dynamic scope 1", 5, (dynamic_scope_tester_1,), nothing, [5]),
        Testcase("dynamic scope 2", 6, (dynamic_scope_tester_1,), nothing, [6]),
        Testcase(
            "nested (static)", nothing, (static_nested_outer,), nothing, [true, false]
        ),
        Testcase(
            "nested (static + used)",
            nothing,
            (static_nested_outer_use_produced,),
            nothing,
            [true, 1],
        ),
        Testcase(
            "nested (dynamic)",
            nothing,
            (dynamic_nested_outer, Ref{Any}(nested_inner)),
            nothing,
            [true, false],
        ),
        Testcase(
            "nested (dynamic + used)",
            nothing,
            (dynamic_nested_outer_use_produced, Ref{Any}(nested_inner)),
            nothing,
            [true, 1],
        ),
        Testcase("callable struct", nothing, (CallableStruct(5), 4), nothing, [5, 4, 9]),
        Testcase(
            "kwarg tester 1",
            nothing,
            (Core.kwcall, (; y=5.0), kwarg_tester, 4.0),
            nothing,
            [],
        ),
        Testcase("kwargs tester 2", nothing, (kwarg_tester, 4.0), (; y=5.0), []),
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

function produce_old_value(x::Float64)
    v = sin(x)
    produce(v)
    produce(v)
    return nothing
end

function branch_on_old_value(x::Float64)
    b = x > 0
    produce(b)
    produce(b ? x : 2x)
    return nothing
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
    c = C(x, y)
    produce(c)
    produce(c)
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

function while_loop()
    t = 1
    while t < 10
        produce(t)
        t = 1 + t
    end
    return nothing
end

function foreigncall_tester(s::String)
    ptr = ccall(:jl_string_ptr, Ptr{UInt8}, (Any,), s)
    produce(typeof(ptr))
    produce(typeof(ptr))
    return nothing
end

function dynamic_scope_tester_1()
    produce(Libtask.get_dynamic_scope())
    return nothing
end

@noinline function nested_inner()
    produce(true)
    return 1
end

Libtask.might_produce(::Type{Tuple{typeof(nested_inner)}}) = true

function static_nested_outer()
    nested_inner()
    produce(false)
    return nothing
end

function static_nested_outer_use_produced()
    y = nested_inner()
    produce(y)
    return nothing
end

function dynamic_nested_outer(f::Ref{Any})
    f[]()
    produce(false)
    return nothing
end

function dynamic_nested_outer_use_produced(f::Ref{Any})
    y = f[]()
    produce(y)
    return nothing
end

struct CallableStruct{T}
    x::T
end

function (c::CallableStruct)(y)
    produce(c.x)
    produce(y)
    produce(c.x + y)
    return nothing
end

kwarg_tester(x; y) = x + y

end
