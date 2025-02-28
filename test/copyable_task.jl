@testset "copyable_task" begin
    for case in Libtask.TestUtils.test_cases()
        case()
    end
    # @testset "construction" begin
    #     function f()
    #         t = 1
    #         while true
    #             produce(t)
    #             t = 1 + t
    #         end
    #     end

    #     ttask = TapedTask(f)
    #     @test consume(ttask) == 1

    #     ttask = TapedTask((f, Union{}))
    #     @test consume(ttask) == 1
    # end

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
        end
    end

    @testset "copying" begin
        # Test case 1: stack allocated objects are deep copied.
        @testset "stack allocated objects shallow copy" begin
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
        @testset "Dict objects shallow copy" begin
            function f()
                t = Dict(1 => 10, 2 => 20)
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

        @testset "Array deep copy 2" begin
            function f()
                t = Array{Int}(undef, 1)
                t[1] = 0
                while true
                    produce(t[1])
                    t[1]
                    t[1] = 1 + t[1]
                end
            end

            ttask = TapedTask(f)

            consume(ttask)
            consume(ttask)
            a = copy(ttask)
            consume(a)
            consume(a)

            @test consume(ttask) == 2
            @test consume(a) == 4

            DATA = Dict{Task,Array}()
            function g()
                ta = zeros(UInt64, 4)
                for i in 1:4
                    ta[i] = hash(current_task())
                    DATA[current_task()] = ta
                    produce(ta[i])
                end
            end

            # ttask = TapedTask(g)
            # @test consume(ttask) == hash(ttask.task) # index = 1
            # @test consume(ttask) == hash(ttask.task) # index = 2

            # a = copy(ttask)
            # @test consume(a) == hash(a.task) # index = 3
            # @test consume(a) == hash(a.task) # index = 4

            # @test consume(ttask) == hash(ttask.task) # index = 3

            # @test DATA[ttask.task] ==
            #     [hash(ttask.task), hash(ttask.task), hash(ttask.task), 0]
            # @test DATA[a.task] ==
            #     [hash(ttask.task), hash(ttask.task), hash(a.task), hash(a.task)]
        end

        # Test atomic values.
        @testset "ref atomic" begin
            function f()
                t = Ref(1)
                t[] = 0
                for _ in 1:6
                    produce(t[])
                    t[]
                    t[] += 1
                end
            end

            ctask = TapedTask(f)

            consume(ctask)
            consume(ctask)

            a = copy(ctask)
            consume(a)
            consume(a)

            @test consume(ctask) == 2
            @test consume(a) == 4
        end

        @testset "ref of dictionary deep copy" begin
            function f()
                t = Ref(Dict("A" => 1, 5 => "B"))
                t[]["A"] = 0
                for _ in 1:6
                    produce(t[]["A"])
                    t[]["A"] += 1
                end
            end

            ctask = TapedTask(f)

            consume(ctask)
            consume(ctask)

            a = copy(ctask)
            consume(a)
            consume(a)

            @test consume(ctask) == 2
            @test consume(a) == 4
        end

        @testset "override deepcopy_types #57" begin
            struct DummyType end

            function f(start::Int)
                t = [start]
                while true
                    produce(t[1])
                    t[1] = 1 + t[1]
                end
            end

            ttask = TapedTask(f, 0; deepcopy_types=DummyType)
            consume(ttask)

            ttask2 = copy(ttask)
            consume(ttask2)

            @test consume(ttask) == 1
            @test consume(ttask2) == 2
        end
    end
end
