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

        t = CTask(f_ct)
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

        function f_ct()
            ta = zeros(UInt64, 4); # NOT a TArray!
            for i in 1:4
                ta[i] = hash(Libtask._current_task())
                DATA[Libtask._current_task()] = get(ta) # use get
                produce(ta[i])
            end
        end

        t = CTask(f_ct)

        @test consume(t) == hash(t) # index = 1
        @test consume(t) == hash(t) # index = 2

        a = copy(t);

        @test consume(a) == hash(a) # index = 3
        @test consume(a) == hash(a) # index = 4

        @test consume(t) == hash(t) # index = 3

        @test DATA[t] == [hash(t), hash(t), hash(t), 0]
        @test DATA[a] == [hash(t), hash(t), hash(a), hash(a)]
    end
end
