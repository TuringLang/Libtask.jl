@testset "copyable_task" begin
    for case in Libtask.TestUtils.test_cases()
        case()
    end
    @testset "set_taped_globals!" begin
        function f()
            produce(Libtask.get_taped_globals(Int))
            produce(Libtask.get_taped_globals(Int))
            return nothing
        end
        t = TapedTask(5, f)
        @test consume(t) == 5
        Libtask.set_taped_globals!(t, 6)
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
        # In this case it just returns it. This used to error, see
        # https://github.com/TuringLang/Libtask.jl/issues/190.
        f(obs) = produce(obs)
        tt = Libtask.TapedTask(nothing, f, :a)
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
end
