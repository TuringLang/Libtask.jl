using Libtask
using IRTools
using Test

@testset "COW Test" begin

    @testset "Test on Array (1)" begin
        function f_ct()
            data = [0 1 2];
            while true
                produce(data[1])
                data[1] = 1 + data[1]
            end
        end

        t = CTask(f_ct, cow=true)
        @test consume(t) == 0
        @test consume(t) == 1

        a = copy(t);
        @test consume(a) == 2
        @test consume(a) == 3

        @test consume(t) == 2
        @test consume(t) == 3

        @test consume(a) == 4
        @test consume(a) == 5
    end

    @testset "Test on Array (2)" begin

        DATA = Dict{Task, Array}()

        function f()
            ta = zeros(UInt64, 4); # NOT a TArray!
            for i in 1:4
                ta[i] = hash(Libtask._current_task())
                @nevercopy DATA[Libtask._current_task()] = get(ta) # use get
                produce(ta[i])
            end
        end

        t = CTask(f, cow=true)

        @test consume(t) == hash(t.task) # index = 1
        @test consume(t) == hash(t.task) # index = 2

        a = copy(t);

        @test consume(a) == hash(a.task) # index = 3
        @test consume(a) == hash(a.task) # index = 4

        @test consume(t) == hash(t.task) # index = 3

        @test DATA[t.task] == [hash(t.task), hash(t.task), hash(t.task), 0]
        @test DATA[a.task] == [hash(t.task), hash(t.task), hash(a.task), hash(a.task)]
    end

    @testset "Test on Array (3)" begin

        DATA = Int[]

        function g()
            for i in 1:4
                push!(DATA, i)
                produce(DATA[end])
            end
        end

        function f()
            @nevercopy g()
        end

        t = CTask(f, cow=true)

        @test consume(t) == 1
        @test consume(t) == 2

        a = copy(t);

        @test consume(a) == 3
        @test consume(a) == 4

        @test consume(t) == 3

        @test DATA == [1, 2, 3, 4, 3]
    end
end
