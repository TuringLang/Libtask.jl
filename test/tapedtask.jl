@testset "tapedtask" begin
    @testset "construction" begin
        function f()
            t = 1
            while true
                produce(t)
                t = 1 + t
            end
        end

        ttask = TapedTask(f)
        @test consume(ttask) == 1

        ttask = TapedTask((f, Union{}))
        @test consume(ttask) == 1
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

        @testset "Too many producers" begin
            function f()
                produce(1)
                produce(2)
            end

            function g()
                f()
            end

            ttask = TapedTask(g)
            @test_throws Exception consume(ttask)
        end

        @testset "Multiple producers for non-primitive" begin
            function f2()
                produce(1)
                produce(2)
            end
            Libtask.is_primitive(::typeof(f2), args...) = false

            function g2()
                f2()
            end

            ttask = TapedTask(g2)
            @test consume(ttask) == 1
            @test consume(ttask) == 2
            @test consume(ttask) === nothing
        end

        @testset "Run two times" begin
            function f4()
                produce(2)
            end

            function g4()
                produce(1)
                f4()
            end

            Libtask.is_primitive(::typeof(f4), args...) = false

            ttask = TapedTask(g4)
            @test consume(ttask) == 1
            @test consume(ttask) == 2
            @test consume(ttask) === nothing

            ttask = TapedTask(g4)
            @test consume(ttask) == 1
            @test consume(ttask) == 2
            @test consume(ttask) === nothing
        end
    end
end