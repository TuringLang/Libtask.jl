@testset "tapedtask" begin
    # Test case 1: stack allocated objects are deep copied.
    @testset "stack allocated objects" begin
        function f()
            t = 0
            while true
                produce(t)
                t = 1 + t
            end
        end

        ttask = TapedTask(f)
        @test consume(ttask) == 0
        @test consume(ttask) == 1
        a = copy(ttask)
        @test consume(a) == 2
        @test consume(a) == 3
        @test consume(ttask) == 2
        @test consume(ttask) == 3

        @inferred Libtask.TapedFunction(f)
    end

    # Test case 2: Array objects are deeply copied.
    @testset "Array objects" begin
        function f()
            t = [0 1 2]
            while true
                produce(t[1])
                t[1] = 1 + t[1]
            end
        end

        ttask = TapedTask(f)
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

    # Test case 3: Dict objects are shallowly copied.
    @testset "Dict objects" begin
        function f()
            t = Dict(1=>10, 2=>20)
            while true
                produce(t[1])
                t[1] = 1 + t[1]
            end
        end

        ttask = TapedTask(f)

        @test consume(ttask) == 10
        @test consume(ttask) == 11

        a = copy(ttask)
        @test consume(a) == 12
        @test consume(a) == 13

        @test consume(ttask) == 14
        @test consume(ttask) == 15
    end

    @testset "iteration" begin
        function f()
            t = 1
            while true
                produce(t)
                t = 1 + t
            end
        end

        ttask = TapedTask(f)

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

            ttask = TapedTask(f)
            try
                consume(ttask)
            catch ex
                @test ex isa MethodError
            end
            if VERSION >= v"1.5"
                @test ttask.task.exception isa MethodError
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

            ttask = TapedTask(f)
            try
                consume(ttask)
            catch ex
                @test ex isa ErrorException
            end
            if VERSION >= v"1.5"
                @test ttask.task.exception isa ErrorException
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

            ttask = TapedTask(f)
            try
                consume(ttask)
            catch ex
                @test ex isa BoundsError
            end
            if VERSION >= v"1.5"
                @test ttask.task.exception isa BoundsError
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

            ttask = TapedTask(f)
            @test consume(ttask) == 2
            try
                consume(ttask)
            catch ex
                @test ex isa BoundsError
            end
            if VERSION >= v"1.5"
                @test ttask.task.exception isa BoundsError
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

            ttask = TapedTask(f)
            @test consume(ttask) == 2
            ttask2 = copy(ttask)
            try
                consume(ttask2)
            catch ex
                @test ex isa BoundsError
            end
            @test ttask.task.exception === nothing
            if VERSION >= v"1.5"
                @test ttask2.task.exception isa BoundsError
            end
        end
    end

    @testset "Issues" begin
        @testset "Issue-140, copy unstarted task" begin
            function f(x)
                for i in 1:3
                    produce(i + x)
                end
            end

            ttask = TapedTask(f, 3)
            ttask2 = copy(ttask)
            @test consume(ttask2) == 4
            ttask3 = copy(ttask; args=(4,))
            @test consume(ttask3) == 5
        end

        @testset "Issue-148, unused argument" begin
            function f(x)
                produce(1)
            end

            ttask = TapedTask(f, 2)
            @test consume(ttask) == 1
        end
    end
end
