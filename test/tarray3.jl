using Libtask
using Test

@testset "TArray Copying Test" begin
    DATA = Dict{Task, Array}()

    function f_ct()
        ta = tzeros(UInt64, 4);
        for i in 1:4
            ta[i] = hash(Libtask._current_task())
            DATA[Libtask._current_task()] = convert(Array, ta)
            produce(ta[i])
        end
    end

    t = CTask(f_ct, cow=false)

    @test consume(t) == hash(t) # index = 1
    @test consume(t) == hash(t) # index = 2

    a = copy(t);

    @test consume(a) == hash(a) # index = 3
    @test consume(a) == hash(a) # index = 4

    @test consume(t) == hash(t) # index = 3


    @test DATA[t] == [hash(t), hash(t), hash(t), 0]
    @test DATA[a] == [hash(t), hash(t), hash(a), hash(a)]

end
