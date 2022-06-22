@testset "tape copy" begin
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

        @inferred Libtask.TapedFunction(f)
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

        DATA = Dict{Task, Array}()
        function g()
            ta = zeros(UInt64, 4)
            for i in 1:4
                ta[i] = hash(current_task())
                DATA[current_task()] = ta
                produce(ta[i])
            end
        end

        ttask = TapedTask(g)
        @test consume(ttask) == hash(ttask.task) # index = 1
        @test consume(ttask) == hash(ttask.task) # index = 2

        a = copy(ttask)
        @test consume(a) == hash(a.task) # index = 3
        @test consume(a) == hash(a.task) # index = 4

        @test consume(ttask) == hash(ttask.task) # index = 3

        @test DATA[ttask.task] == [hash(ttask.task), hash(ttask.task), hash(ttask.task), 0]
        @test DATA[a.task] == [hash(ttask.task), hash(ttask.task), hash(a.task), hash(a.task)]
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

    @testset "ref of array deep copy" begin
        # Create a TRef storing a matrix.
        x = TRef([1 2 3; 4 5 6])
        x[][1, 3] = 900
        @test x[][1,3] == 900

        # TRef holding an array.
        y = TRef([1,2,3])
        y[][2] = 19
        @test y[][2] == 19
    end
end
