module CopyableTaskTests

using Libtask
using Test

# Used later.
__global_a = 1.0

# Functions exercising `try` / `catch` support (#194). Defined at module scope (rather than
# inside the testset) so they are lowered like ordinary top-level functions -- a nested
# closure can capture variables in a way that changes the exception IR (e.g. introducing
# `UpsilonNode` / `PhiCNode`), which would not reflect normal usage.
@noinline _tc_mayfail() = error("boom")
@noinline _tc_maybe(i) = i == 999 ? error("x") : i
@noinline _tc_risky(y) = y > 0 ? y * 2 : error("neg")

# produce outside the handler extent -- supported on 1.12+.
function tc_safe()
    produce(0)
    try
        _tc_mayfail()
    catch
    end
    produce(3)
    return nothing
end
# the exception is genuinely caught (else `_tc_mayfail` would escape the task).
function tc_caught()
    ok = false
    try
        _tc_mayfail()
    catch
        ok = true
    end
    produce(ok)
    return nothing
end
# try/catch inside a producing loop: exercises a `:leave` (normal try exit) followed by a
# back-edge, which must keep `:leave` as the block terminator.
function tc_loop()
    for i in 1:3
        y = 0
        try
            y = _tc_maybe(i)
        catch
            y = -1
        end
        produce(y)
    end
    return nothing
end
# a value defined in the try/catch and consumed afterwards which lowers to a plain `PhiNode`
# (not `PhiCNode`): supported, unlike the `UpsilonNode` / `PhiCNode` case.
function tc_phi(y)
    local x
    try
        x = _tc_risky(y)
    catch
        x = -1.0
    end
    produce(x + 1)
    return nothing
end
# unsafe: produce inside the try body (the issue's MWE).
function tc_in_try()
    try
        produce(1)
        error("")
    catch
        produce(2)
    end
    return nothing
end
# unsafe: produce inside the catch.
function tc_in_catch()
    try
        _tc_mayfail()
    catch
        produce(2)
    end
    return nothing
end
# not yet supported: a value defined in the try/catch and used afterwards lowers to
# `UpsilonNode` / `PhiCNode`.
function tc_value(x)
    y = 0.0
    try
        x > 0 && error("b")
        y = x
    catch
        y = 2x
    end
    produce(y)
    return nothing
end

# A `try` / `catch` inside a `Base.ScopedValues.@with` body produces a scope-only `enter`
# (`Core.EnterNode` with `catch_dest == 0`); this must not crash IR construction (#194).
@static if isdefined(Base, :ScopedValues)
    const TC_SCOPED = Base.ScopedValues.ScopedValue(10)
    # produce AFTER the scope block -- supported on 1.12+.
    function tc_with_after()
        Base.ScopedValues.@with TC_SCOPED => 5 begin
            try
                _tc_mayfail()
            catch
            end
        end
        produce(TC_SCOPED[])
        return nothing
    end
    # produce INSIDE the scope -- unsafe (would suspend with the scope frame live).
    function tc_with_inside()
        Base.ScopedValues.@with TC_SCOPED => 5 begin
            produce(TC_SCOPED[])
        end
        return nothing
    end
end

@testset "copyable_task" begin
    @testset "get_taped_globals outside of a task" begin
        # This testset must come first because subsequent calls to get_taped_globals /
        # set_taped_globals! will affect the TLS of the task running the tests.
        @test_throws Libtask.NotInTapedTaskError Libtask.get_taped_globals(Any)
    end

    for case in Libtask.TestUtils.test_cases()
        case()
    end

    @testset "get_ and set_taped_globals!" begin
        # Note that this testset tests both methods of get_taped_globals, the one inside the
        # task itself, and the one outside.
        function f()
            produce(Libtask.get_taped_globals(Int))
            produce(Libtask.get_taped_globals(Int))
            return nothing
        end
        t = TapedTask(5, f)
        @test Libtask.get_taped_globals(t) == 5
        @test consume(t) == 5
        Libtask.set_taped_globals!(t, 6)
        @test Libtask.get_taped_globals(t) == 6
        @test consume(t) == 6
        @test consume(t) === nothing
    end

    @testset "iteration" begin
        function f()
            t = 1
            while true
                produce(t)
                t = 1 + t
            end
        end

        ttask = TapedTask(nothing, f)

        next = iterate(ttask)
        @test next === (1, nothing)

        val, state = next
        next = iterate(ttask, state)
        @test next === (2, nothing)

        val, state = next
        next = iterate(ttask, state)
        @test next === (3, nothing)

        a = collect(Iterators.take(ttask, 7))
        @test eltype(a) === Int
        @test a == 4:10
    end

    # Test of `Exception`.
    @testset "Exception" begin
        @testset "method error" begin
            function f()
                t = 0
                while true
                    t[3] = 1
                    produce(t)
                    t = t + 1
                end
            end

            ttask = TapedTask(nothing, f)
            try
                consume(ttask)
            catch ex
                @test ex isa MethodError
            end
        end

        @testset "error test" begin
            function f()
                x = 1
                while true
                    error("error test")
                    produce(x)
                    x += 1
                end
            end

            ttask = TapedTask(nothing, f)
            try
                consume(ttask)
            catch ex
                @test ex isa ErrorException
            end
        end

        @testset "OutOfBounds Test Before" begin
            function f()
                x = zeros(2)
                while true
                    x[1] = 1
                    x[2] = 2
                    x[3] = 3
                    produce(x[1])
                end
            end

            ttask = TapedTask(nothing, f)
            try
                consume(ttask)
            catch ex
                @test ex isa BoundsError
            end
        end

        @testset "OutOfBounds Test After `produce`" begin
            function f()
                x = zeros(2)
                while true
                    x[1] = 1
                    x[2] = 2
                    produce(x[2])
                    x[3] = 3
                end
            end

            ttask = TapedTask(nothing, f)
            @test consume(ttask) == 2
            try
                consume(ttask)
            catch ex
                @test ex isa BoundsError
            end
        end

        @testset "OutOfBounds Test After `copy`" begin
            function f()
                x = zeros(2)
                while true
                    x[1] = 1
                    x[2] = 2
                    produce(x[2])
                    x[3] = 3
                end
            end

            ttask = TapedTask(nothing, f)
            @test consume(ttask) == 2
            ttask2 = copy(ttask)
            try
                consume(ttask2)
            catch ex
                @test ex isa BoundsError
            end
        end

        @testset "Naked produce" begin
            @test_throws "wrap the call to `produce` in a function" Libtask.consume(
                Libtask.TapedTask(nothing, Libtask.produce, 0)
            )
        end

        @testset "No matching method (#183)" begin
            f_int_only(a::Int) = produce(a)
            # Wrong arity
            @test_throws ArgumentError TapedTask(nothing, f_int_only)
            @test_throws "Failed to generate IR" TapedTask(nothing, f_int_only)
            # Wrong argument type
            @test_throws ArgumentError TapedTask(nothing, f_int_only, 0.1)
            @test_throws "Failed to generate IR" TapedTask(nothing, f_int_only, 0.1)

            # try with a callable struct too
            struct NotActuallyCallable end
            @test_throws ArgumentError TapedTask(nothing, NotActuallyCallable())
            @test_throws "Failed to generate IR" TapedTask(nothing, NotActuallyCallable())

            struct IsCallable end
            (::IsCallable)(x::Int) = produce(x)
            @test_throws ArgumentError TapedTask(nothing, IsCallable())
            @test_throws "Failed to generate IR" TapedTask(nothing, IsCallable())
            @test_throws ArgumentError TapedTask(nothing, IsCallable(), 0.1)
            @test_throws "Failed to generate IR" TapedTask(nothing, IsCallable(), 0.1)
        end

        @testset "Ambiguous method (#199)" begin
            f_ambig(x::Int, y) = produce(x)
            f_ambig(x, y::Int) = produce(y)
            @test_throws ArgumentError TapedTask(nothing, f_ambig, 1, 1)
            @test_throws "Failed to generate IR" TapedTask(nothing, f_ambig, 1, 1)
        end
    end

    # try / catch / finally support (#194). The supported ("safe") subset is any try / catch
    # whose handler is set up and torn down within a single `consume` -- i.e. no `produce`
    # suspends while a handler is active. Cases where a `produce` could suspend inside a
    # handler, or where the block feeds a value into later code (UpsilonNode / PhiCNode), are
    # rejected with an informative error rather than miscompiled. Support requires the Julia
    # 1.12+ compiler to handle exception-handling IR in the derived task; on earlier versions
    # any `try` / `catch` that survives into the IR is rejected with a clear message.
    @testset "try/catch (#194)" begin
        @static if VERSION >= v"1.12-"
            # Safe cases run through the `Testcase` driver, which also copies the task after
            # every iteration and checks each copy resumes to the same remaining results --
            # exercising the copy/resume contract across the exception regions.
            Libtask.TestUtils.Testcase(
                "trycatch: produce outside handler extent",
                nothing,
                (tc_safe,),
                nothing,
                [0, 3],
                Libtask.TestUtils.none,
            )()
            Libtask.TestUtils.Testcase(
                "trycatch: exception is genuinely caught",
                nothing,
                (tc_caught,),
                nothing,
                [true],
                Libtask.TestUtils.none,
            )()
            Libtask.TestUtils.Testcase(
                "trycatch: inside a producing loop",
                nothing,
                (tc_loop,),
                nothing,
                [1, 2, 3],
                Libtask.TestUtils.none,
            )()
            Libtask.TestUtils.Testcase(
                "trycatch: PhiNode-carried catch value (caught)",
                nothing,
                (tc_phi, -1.0),
                nothing,
                [0.0],
                Libtask.TestUtils.none,
            )()
            Libtask.TestUtils.Testcase(
                "trycatch: PhiNode-carried catch value (not caught)",
                nothing,
                (tc_phi, 4.0),
                nothing,
                [9.0],
                Libtask.TestUtils.none,
            )()

            @testset "unsafe: produce inside try body (issue MWE)" begin
                @test_throws ArgumentError TapedTask(nothing, tc_in_try)
                @test_throws "does not support a `produce` inside" TapedTask(
                    nothing, tc_in_try
                )
            end

            @testset "unsafe: produce inside catch" begin
                @test_throws ArgumentError TapedTask(nothing, tc_in_catch)
                @test_throws "does not support a `produce` inside" TapedTask(
                    nothing, tc_in_catch
                )
            end

            @testset "not yet supported: value-carrying catch" begin
                @test_throws ArgumentError TapedTask(nothing, tc_value, 1.0)
                @test_throws "does not yet support" TapedTask(nothing, tc_value, 1.0)
            end

            @static if isdefined(Base, :ScopedValues)
                @testset "scope-only enter (@with): produce after scope" begin
                    # `TC_SCOPED[]` is read outside the scope, so it is the default value.
                    @test collect(TapedTask(nothing, tc_with_after)) == [10]
                end
                @testset "scope-only enter (@with): produce inside scope is unsafe" begin
                    @test_throws "does not support a `produce` inside" TapedTask(
                        nothing, tc_with_inside
                    )
                end
            end
        else
            @testset "try/catch rejected before Julia 1.12" begin
                for f in (tc_safe, tc_caught, tc_loop, tc_in_try, tc_in_catch)
                    @test_throws ArgumentError TapedTask(nothing, f)
                    @test_throws "only supports functions containing `try`" TapedTask(
                        nothing, f
                    )
                end
                @test_throws ArgumentError TapedTask(nothing, tc_value, 1.0)
                @test_throws "only supports functions containing `try`" TapedTask(
                    nothing, tc_value, 1.0
                )
                # A scope-only `enter` from `@with` must give the clean version error here,
                # not crash IR construction with a `KeyError` (#194).
                @static if isdefined(Base, :ScopedValues)
                    @test_throws "only supports functions containing `try`" TapedTask(
                        nothing, tc_with_after
                    )
                end
            end
        end
    end

    @testset "copying" begin
        # Test case 1: stack allocated objects copied by value.
        @testset "stack allocated objects shallow copy" begin
            function f()
                t = 0
                while true
                    produce(t)
                    t = 1 + t
                end
            end

            ttask = TapedTask(nothing, f)
            @test consume(ttask) == 0
            @test consume(ttask) == 1
            a = copy(ttask)
            @test consume(a) == 2
            @test consume(a) == 3
            @test consume(ttask) == 2
            @test consume(ttask) == 3
        end

        # Test case 2: Array objects are deeply copied.
        @testset "Array objects deep copy" begin
            function f()
                t = [0 1 2]
                while true
                    produce(t[1])
                    t[1] = 1 + t[1]
                end
            end

            ttask = TapedTask(nothing, f)
            @test consume(ttask) == 0
            @test consume(ttask) == 1
            a = copy(ttask)
            @test consume(a) == 2
            @test consume(a) == 3
            @test consume(ttask) == 2
            @test consume(ttask) == 3
            @test consume(ttask) == 4
            @test consume(ttask) == 5
        end
    end
    @testset "Issue: PR-86 (DynamicPPL.jl/pull/261)" begin
        function f()
            t = Array{Int}(undef, 1)
            t[1] = 0
            for _ in 1:4000
                produce(t[1])
                t[1]
                t[1] = 1 + t[1]
            end
        end

        ttask = TapedTask(nothing, f)

        ex = try
            for _ in 1:999
                consume(ttask)
                consume(ttask)
                a = copy(ttask)
                consume(a)
                consume(a)
            end
        catch ex
            ex
        end
        @test ex === nothing
    end

    @testset "Issue #185" begin
        g() = produce(rand() > -1.0 ? 2 : 0.1)
        @test Libtask.consume(Libtask.TapedTask(nothing, g)) == 2
    end

    @testset "Indirect produce in a loop" begin
        # Test that we can wrap a `produce` call in another function, and call that function
        # in a loop. This used to only produce some of the values, see
        # https://github.com/TuringLang/Libtask.jl/issues/190.
        produce_wrapper(x) = (Libtask.produce(x); return nothing)
        Libtask.might_produce(::Type{<:Tuple{typeof(produce_wrapper),Any}}) = true
        function f(obs)
            for o in obs
                produce_wrapper(o)
            end
            return nothing
        end

        # That the eltype of vals is Any is significant for reproducing the original bug.
        # Unclear why.
        vals = Any[:a, :b, :c]
        tt = Libtask.TapedTask(nothing, f, vals)
        @test Libtask.consume(tt) === :a
        @test Libtask.consume(tt) === :b
        @test Libtask.consume(tt) === :c
    end
    @testset "Return produce" begin
        # Test calling a function that does something with the return value of `produce`.
        # This used to error, see https://github.com/TuringLang/Libtask.jl/issues/190.
        function f(obs)
            x = produce(obs)
            # produce(x) returns x
            return if isnothing(x)
                produce(1.0)
            else
                produce(x)
            end
        end
        tt = Libtask.TapedTask(nothing, f, :a)
        @test Libtask.consume(tt) === :a
        @test Libtask.consume(tt) === :a
        @test Libtask.consume(tt) === nothing
    end

    @testset "@might_produce macro" begin
        # Positional arguments only
        @noinline g1(x) = produce(x)
        f1(x) = g1(x)
        # Without marking it as might_produce
        tt = Libtask.TapedTask(nothing, f1, 0)
        @test Libtask.consume(tt) === nothing
        # Now marking it
        Libtask.@might_produce(g1)
        tt = Libtask.TapedTask(nothing, f1, 0)
        @test Libtask.consume(tt) === 0
        @test Libtask.consume(tt) === nothing

        # Keyword arguments only
        @noinline g2(x; y=1, z=2) = produce(x + y + z)
        f2(x) = g2(x)
        # Without marking it as might_produce
        tt = Libtask.TapedTask(nothing, f2, 0)
        @test Libtask.consume(tt) === nothing
        # Now marking it
        Libtask.@might_produce(g2)
        tt = Libtask.TapedTask(nothing, f2, 0)
        @test Libtask.consume(tt) === 3
        @test Libtask.consume(tt) === nothing

        # A function with multiple methods.
        # The function reference is used to ensure that it really doesn't get inlined
        # (otherwise, for reasons that are yet unknown, these functions do get inlined when
        # inside a testset)
        @noinline g3(x) = produce(x)
        @noinline g3(x, y; z) = produce(x + y + z)
        @noinline g3(x, y, z; p, q) = produce(x + y + z + p + q)
        function f3(x, fref)
            fref[](x)
            fref[](x, 1; z=2)
            fref[](x, 1, 2; p=3, q=4)
            return nothing
        end
        tt = Libtask.TapedTask(nothing, f3, 0, Ref(g3))
        @test Libtask.consume(tt) === nothing
        # Now marking it
        Libtask.@might_produce(g3)
        tt = Libtask.TapedTask(nothing, f3, 0, Ref(g3))
        @test Libtask.consume(tt) === 0
        @test Libtask.consume(tt) === 3
        @test Libtask.consume(tt) === 10
        @test Libtask.consume(tt) === nothing
    end

    # Regression test against https://github.com/TuringLang/Libtask.jl/issues/207
    @testset "Union deferencing" begin
        function g(i::Int)
            produce(1)
            return i
        end
        function g(s::String)
            produce(2)
            return s
        end
        Libtask.@might_produce g

        h(i::Int)::Int = i
        h(s::String)::Int = length(s)

        function f(x, g)
            i_or_s = x > 0 ? 3 : "libtask"
            gres = g(i_or_s)
            return h(gres)
        end

        t = TapedTask(nothing, f, 1, g)
        consume(t)
        # This used to error
        @test (consume(t); true)
    end

    # Regression test for https://github.com/TuringLang/Libtask.jl/issues/204
    @testset "throw_undef_if_not handling" begin
        function tuin_g(y)
            return produce(y + 1)
        end
        Libtask.might_produce(::Type{Tuple{typeof(tuin_g),Int}}) = true

        function tuin_f(x)
            if x == 1
                y = 2
            end
            # `g` must produce.
            # Also, there must be at least two calls to `produce` in the function.
            tuin_g(y)
            tuin_g(y)
            return 1
        end

        # TapedTask construction used to error.
        t = TapedTask(nothing, tuin_f, 1)
        # Check that it returns the right things.
        @test consume(t) == 3
        @test consume(t) == 3
        @test consume(t) === nothing

        # This is bogus, but we should check that it has the right behaviour.
        t = TapedTask(nothing, tuin_f, 2)
        @test_throws UndefVarError consume(t)
    end

    @testset "might_produce_if_sig_contains" begin
        @testset "concrete type" begin
            struct MyType end
            @noinline function mp_g(x::Int, ::MyType)
                produce(x)
                return nothing
            end
            @noinline function mp_g(::MyType; x::Int=3)
                produce(x)
                return nothing
            end
            @noinline function mp_g(x::Int)
                produce(x)
                return nothing
            end

            # Use function reference to ensure that mp_g doesn't get inlined.
            function mp_f(x, gref)
                gref[](x, MyType())    # Should produce
                gref[](MyType(); x=4)  # Should produce
                gref[](x)              # Should not produce
                return nothing
            end
            # Before marking it as might_produce, nothing should produce.
            t = TapedTask(nothing, mp_f, 1, Ref(mp_g))
            @test consume(t) === nothing

            # Now marking `MyType` as causing produces should make the first two calls
            # produce, but not the third.
            Libtask.might_produce_if_sig_contains(::Type{MyType}) = true
            t = TapedTask(nothing, mp_f, 1, Ref(mp_g))
            @test consume(t) == 1
            @test consume(t) == 4
            @test consume(t) === nothing
        end

        @testset "abstract type" begin
            abstract type AbstractTT end
            struct MyType2 <: AbstractTT end
            @noinline function abs_mp_g(x::Int, ::MyType2)
                produce(x)
                return nothing
            end
            @noinline function abs_mp_g(::MyType2; x::Int=3)
                produce(x)
                return nothing
            end
            @noinline function abs_mp_g(x::Int)
                produce(x)
                return nothing
            end

            # Use function reference to ensure that mp_g doesn't get inlined.
            function abs_mp_f(x, gref)
                gref[](x, MyType2())    # Should produce
                gref[](MyType2(); x=4)  # Should produce
                gref[](x)              # Should not produce
                return nothing
            end
            # Before marking it as might_produce, nothing should produce.
            t = TapedTask(nothing, abs_mp_f, 1, Ref(abs_mp_g))
            @test consume(t) === nothing

            # Now marking `AbstractTT` as causing produces should make the first two calls
            # produce, but not the third.
            Libtask.might_produce_if_sig_contains(::Type{<:AbstractTT}) = true
            t = TapedTask(nothing, abs_mp_f, 1, Ref(abs_mp_g))
            @test consume(t) == 1
            @test consume(t) == 4
            @test consume(t) === nothing
        end
    end

    @testset "(non-const) global variables in TapedTasks" begin
        function global_f()
            produce(__global_a + 1.0)
            produce(__global_a + 1.0)
            return nothing
        end
        # TapedTask construction used to error:
        # https://github.com/TuringLang/Libtask.jl/issues/211
        t = TapedTask(nothing, global_f)
        @test consume(t) == 2.0
        # Check that you can mutate the variable between `produce`s.
        global __global_a
        __global_a = 10.0
        @test consume(t) == 11.0
        @test consume(t) === nothing
    end
end

end # module
