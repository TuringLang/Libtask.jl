@testset "ctask" begin
    # Test case 1: stack allocated objects are deep copied.
    @testset "stack allocated objects" begin
        function f()
            t = 0
            while true
                produce(t)
                t = 1 + t
            end
        end

        ctask = CTask(f)
        @test consume(ctask) == 0
        @test consume(ctask) == 1
        a = copy(ctask)
        @test consume(a) == 2
        @test consume(a) == 3
        @test consume(ctask) == 2
        @test consume(ctask) == 3

        @inferred Libtask.TapedFunction(f)
    end

    # Test case 2: heap allocated objects are shallowly copied.
    @testset "heap allocated objects" begin
        function f()
            t = [0 1 2]
            while true
                produce(t[1])
                t[1] = 1 + t[1]
            end
        end

        ctask = CTask(f)
        @test consume(ctask) == 0
        @test consume(ctask) == 1
        a = copy(ctask)
        @test consume(a) == 2
        @test consume(a) == 3
        @test consume(ctask) == 4
        @test consume(ctask) == 5
        @test consume(ctask) == 6
        @test consume(ctask) == 7
    end

    @testset "iteration" begin
        function f()
            t = 1
            while true
                produce(t)
                t = 1 + t
            end
        end

        ctask = CTask(f)

        next = iterate(ctask)
        @test next === (1, nothing)

        val, state = next
        next = iterate(ctask, state)
        @test next === (2, nothing)

        val, state = next
        next = iterate(ctask, state)
        @test next === (3, nothing)

        a = collect(Iterators.take(ctask, 7))
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

            ctask = CTask(f)
            try
                consume(ctask)
            catch ex
                @test ex isa MethodError
            end
            if VERSION >= v"1.5"
                @test ctask.task.exception isa MethodError
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

            ctask = CTask(f)
            try
                consume(ctask)
            catch ex
                @test ex isa ErrorException
            end
            if VERSION >= v"1.5"
                @test ctask.task.exception isa ErrorException
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

            ctask = CTask(f)
            try
                consume(ctask)
            catch ex
                @test ex isa BoundsError
            end
            if VERSION >= v"1.5"
                @test ctask.task.exception isa BoundsError
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

            ctask = CTask(f)
            @test consume(ctask) == 2
            try
                consume(ctask)
            catch ex
                @test ex isa BoundsError
            end
            if VERSION >= v"1.5"
                @test ctask.task.exception isa BoundsError
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

            ctask = CTask(f)
            @test consume(ctask) == 2
            ctask2 = copy(ctask)
            try
                consume(ctask2)
            catch ex
                @test ex isa BoundsError
            end
            @test ctask.task.exception === nothing
            if VERSION >= v"1.5"
                @test ctask2.task.exception isa BoundsError
            end
        end
    end
end
