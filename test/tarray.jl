@testset "tarray" begin
    @testset "tzeros and tfill" begin
        a = tzeros(Float64, 10)
        @test eltype(a) === Float64
        @test all(iszero, a)
        @test size(a) == (10,)

        a = tzeros(10)
        @test eltype(a) === Float64
        @test all(iszero, a)
        @test size(a) == (10,)

        a = tfill(5, 10)
        @test eltype(a) === Int
        @test all(==(5), a)
        @test size(a) == (10,)

        a = tfill(19, (5, 5, 5, 5))
        @test eltype(a) === Int
        @test all(==(19), a)
        @test size(a) == (5, 5, 5, 5)
    end

    @testset "push! and pop!" begin
        ta1 = TArray(Int, 4)
        push!(ta1, 1)
        push!(ta1, 2)
        @test pop!(ta1) == 2

        # another constructor
        ta1_2 = TArray(Int, 4)
        push!(ta1_2, 1)
        push!(ta1_2, 2)
        @test pop!(ta1_2) == 2
    end

    @testset "task copy" begin
        function f()
            t = TArray(Int, 1)
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
            ta = tzeros(UInt64, 4)
            for i in 1:4
                ta[i] = hash(current_task())
                DATA[current_task()] = convert(Array, ta)
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

    @testset "Issue: PR-86 (DynamicPPL.jl/pull/261)" begin
        function f()
            t = TArray(Int, 1)
            t[1] = 0
            for _ in 1:4000
                produce(t[1])
                t[1]
                t[1] = 1 + t[1]
            end
        end


        ttask = TapedTask(f)

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

end
