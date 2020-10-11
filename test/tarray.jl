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
        ta1 = TArray{Int}(4)
        push!(ta1, 1)
        push!(ta1, 2)
        @test pop!(ta1) == 2

        # another constructor
        ta1_2 = TArray{Int, 1}(4)
        push!(ta1_2, 1)
        push!(ta1_2, 2)
        @test pop!(ta1_2) == 2
    end

    @testset "other methods" begin
        ta2 = TArray{Int}(4, 4)
        @test ta2 isa TArray{Int,2}
        @test size(ta2) == (4, 4)

        ta2 = TArray{Int}(undef, 4, 4)
        @test ta2 isa TArray{Int,2}
        @test size(ta2) == (4, 4)

        ta2 = TArray{Int,2}(4, 4)
        @test ta2 isa TArray{Int,2}
        @test size(ta2) == (4, 4)

        ta2 = TArray{Int,2}(undef, 4, 4)
        @test ta2 isa TArray{Int,2}
        @test size(ta2) == (4, 4)

        @test_throws MethodError TArray{Int,2}(4)
        @test_throws MethodError TArray{Int,2}(undef, 4)

        ta3 = TArray{Int, 4}(4, 3, 2, 1)
        ta4 = get(ta3)
        @test ta3[3] == ta4[3]

        ta5 = TArray{Int}(4)
        @test ta5 isa TArray{Int,1}
        @test size(ta5) == (4,)

        ta5 = TArray{Int}(undef, 4)
        @test ta5 isa TArray{Int,1}
        @test size(ta5) == (4,)

        ta5 = TArray{Int,1}(4)
        @test ta5 isa TArray{Int,1}
        @test size(ta5) == (4,)

        ta5 = TArray{Int,1}(undef, 4)
        @test ta5 isa TArray{Int,1}
        @test size(ta5) == (4,)

        @test_throws MethodError TArray{Int,1}(4, 4)
        @test_throws MethodError TArray{Int,1}(undef, 4, 4)

        for i in 1:4
            ta5[i] = i
        end
        @test Array(ta5) == [1, 2, 3, 4]
        @test convert(Array, ta5) == [1, 2, 3, 4]
        @test convert(Array{Int, 1}, ta5) == [1, 2, 3, 4]
        @test ta5 == convert(TArray, [1, 2, 3, 4])
        @test ta5 == convert(TArray{Int, 1}, [1, 2, 3, 4])
        @test_throws MethodError convert(TArray{Int, 2}, [1, 2, 3, 4])
        @test_throws MethodError convert(Array{Int, 2}, ta5)

        @test Array(tzeros(4)) == zeros(4)

        ta6 = TArray{Float64}(4)
        for i in 1:4
            ta6[i] = i / 10
        end
        @test ta6[1] == 0.1
        @test Array(ta6) == [0.1, 0.2, 0.3, 0.4]

        # TODO: add test for use this multi-dim array
        ta7 = TArray{Int, 2}((2, 2))
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

        ctask = CTask(f)

        consume(ctask)
        consume(ctask)
        a = copy(ctask)
        consume(a)
        consume(a)

        @test consume(ctask) == 2
        @task consume(a) == 4

        DATA = Dict{Task, Array}()
        function g()
            ta = tzeros(UInt64, 4)
            for i in 1:4
                ta[i] = hash(Libtask._current_task())
                DATA[Libtask._current_task()] = convert(Array, ta)
                produce(ta[i])
            end
        end

        ctask = CTask(g)
        @test consume(ctask) == hash(ctask.task) # index = 1
        @test consume(ctask) == hash(ctask.task) # index = 2

        a = copy(ctask)
        @test consume(a) == hash(a.task) # index = 3
        @test consume(a) == hash(a.task) # index = 4

        @test consume(ctask) == hash(ctask.task) # index = 3

        @test DATA[ctask.task] == [hash(ctask.task), hash(ctask.task), hash(ctask.task), 0]
        @test DATA[a.task] == [hash(ctask.task), hash(ctask.task), hash(a.task),
                               hash(a.task)]
    end
end
