module TestUtils

using ..Libtask
using Test

# Function barrier to ensure inference in value types.
function count_allocs(f::F, x::Vararg{Any,N}) where {F,N}
    @allocations f(x...)
end

@enum PerfFlag none allocs

struct Testcase
    name::String
    taped_globals::Any
    fargs::Tuple
    kwargs::Union{NamedTuple,Nothing}
    expected_iteration_results::Vector
    perf::PerfFlag
end

function (case::Testcase)()
    testset = @testset "$(case.name)" begin

        # Display some information.
        @info "$(case.name)"

        # Construct the task.
        if case.kwargs === nothing
            t = TapedTask(case.taped_globals, case.fargs...)
        else
            t = TapedTask(case.taped_globals, case.fargs...; case.kwargs...)
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

        # Check no allocations if requested.
        if case.perf == allocs

            # Construct the task.
            if case.kwargs === nothing
                t = TapedTask(case.taped_globals, case.fargs...)
            else
                t = TapedTask(case.taped_globals, case.fargs...; case.kwargs...)
            end

            for _ in iteration_results
                @test count_allocs(consume, t) == 0
            end
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
            allocs,
        ),
        Testcase(
            "produce old",
            nothing,
            (produce_old_value, 5.0),
            nothing,
            [sin(5.0), sin(5.0)],
            allocs,
        ),
        Testcase(
            "branch on old value l",
            nothing,
            (branch_on_old_value, 2.0),
            nothing,
            [true, 2.0],
            allocs,
        ),
        Testcase(
            "branch on old value r",
            nothing,
            (branch_on_old_value, -1.0),
            nothing,
            [false, -2.0],
            allocs,
        ),
        Testcase("no produce", nothing, (no_produce_test, 5.0, 4.0), nothing, [], allocs),
        Testcase(
            "new object",
            nothing,
            (new_object_test, 5, 4),
            nothing,
            [C(5, 4), C(5, 4)],
            none,
        ),
        Testcase(
            "branching test l",
            nothing,
            (branching_test, 5.0, 4.0),
            nothing,
            [complex(sin(5.0))],
            allocs,
        ),
        Testcase(
            "branching test r",
            nothing,
            (branching_test, 4.0, 5.0),
            nothing,
            [sin(4.0) * cos(5.0)],
            allocs,
        ),
        Testcase(
            "unused argument test", nothing, (unused_argument_test, 3), nothing, [1], allocs
        ),
        Testcase("test with const", nothing, (test_with_const,), nothing, [1], allocs),
        Testcase("while loop", nothing, (while_loop,), nothing, collect(1:9), allocs),
        Testcase(
            "foreigncall tester",
            nothing,
            (foreigncall_tester, "hi"),
            nothing,
            [Ptr{UInt8}, Ptr{UInt8}],
            allocs,
        ),
        Testcase("globals tester 1", 5, (taped_globals_tester_1,), nothing, [5], allocs),
        Testcase("globals tester 2", 6, (taped_globals_tester_1,), nothing, [6], none),
        Testcase(
            "globals tester 3", 6, (while_loop_with_globals,), nothing, fill(6, 9), allocs
        ),
        Testcase(
            "nested (static)", nothing, (static_nested_outer,), nothing, [true, false], none
        ),
        Testcase(
            "nested (static + used)",
            nothing,
            (static_nested_outer_use_produced,),
            nothing,
            [true, 1],
            none,
        ),
        Testcase(
            "nested (dynamic)",
            nothing,
            (dynamic_nested_outer, Ref{Any}(nested_inner)),
            nothing,
            [true, false],
            none,
        ),
        Testcase(
            "nested (dynamic + used)",
            nothing,
            (dynamic_nested_outer_use_produced, Ref{Any}(nested_inner)),
            nothing,
            [true, 1],
            none,
        ),
        Testcase(
            "nested with args (static)",
            nothing,
            (static_nested_outer_args,),
            nothing,
            [:a, :b, false],
            none,
        ),
        Testcase(
            "nested with args (static + used)",
            nothing,
            (static_nested_outer_use_produced_args,),
            nothing,
            [:a, :b, 1],
            none,
        ),
        Testcase(
            "nested with args (dynamic)",
            nothing,
            (dynamic_nested_outer_args, Ref{Any}(nested_inner_args)),
            nothing,
            [:a, :b, false],
            none,
        ),
        Testcase(
            "nested with args (dynamic + used)",
            nothing,
            (dynamic_nested_outer_use_produced_args, Ref{Any}(nested_inner_args)),
            nothing,
            [:a, :b, 1],
            none,
        ),
        Testcase(
            "callable struct", nothing, (CallableStruct(5), 4), nothing, [5, 4, 9], allocs
        ),
        Testcase(
            "kwarg tester 1",
            nothing,
            (Core.kwcall, (; y=5.0), kwarg_tester, 4.0),
            nothing,
            [],
            allocs,
        ),
        Testcase("kwargs tester 2", nothing, (kwarg_tester, 4.0), (; y=5.0), [], allocs),
        Testcase(
            "default kwarg tester",
            nothing,
            (default_kwarg_tester, 4.0),
            nothing,
            [],
            allocs,
        ),
        Testcase(
            "default kwarg tester", nothing, (default_kwarg_tester, 4.0), (;), [], allocs
        ),
        Testcase(
            "final statment produce",
            nothing,
            (final_statement_produce,),
            nothing,
            [1, 2],
            allocs,
        ),
        Testcase(
            "rosenbrock", nothing, (rosenbrock, rand(100_000), nothing), nothing, [], none
        ),
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
        produce(complex(sin(x)))
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

function taped_globals_tester_1()
    produce(Libtask.get_taped_globals(Int))
    return nothing
end

function while_loop_with_globals()
    t = 1
    while t < 10
        produce(get_taped_globals(Int))
        t = 1 + t
    end
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

@noinline function nested_inner_args(xs...)
    for x in xs
        produce(x)
    end
    return 1
end

Libtask.might_produce(::Type{<:Tuple{typeof(nested_inner_args),Any}}) = true
Libtask.might_produce(::Type{<:Tuple{typeof(nested_inner_args),Any,Vararg}}) = true

function static_nested_outer_args()
    nested_inner_args(:a, :b)
    produce(false)
    return nothing
end

function static_nested_outer_use_produced_args()
    y = nested_inner_args(:a, :b)
    produce(y)
    return nothing
end

function dynamic_nested_outer_args(f::Ref{Any})
    f[](:a, :b)
    produce(false)
    return nothing
end

function dynamic_nested_outer_use_produced_args(f::Ref{Any})
    y = f[](:a, :b)
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

default_kwarg_tester(x; y=5.0) = x * y

function final_statement_produce()
    produce(1)
    return produce(2)
end

# Produces a `:loopinfo` expression.
function rosenbrock(x, callback=nothing)
    i = x[2:end]
    j = x[1:(end - 1)]
    ret = sum((1 .- j) .^ 2 + 100 * (i - j .^ 2) .^ 2)
    callback !== nothing && callback(ret)
    return ret
end

end
