using Libtask
using Test

@testset "TArray Copying Test" begin
    DATA = Dict{Task, Array}()

    function f_ct()
        ta = tzeros(Int, 4);
        for i in 1:4
            ta[i] = i
            DATA[current_task()] = convert(Array, ta)
            produce(ta[i])
        end
    end

    t = CTask(f_ct)

    @test consume(t) == 1
    @test consume(t) == 2

    a = copy(t);

    @test consume(a) == 3
    @test consume(a) == 4

    @test DATA[t] == [1, 2, 0, 0]
    @test DATA[a] == [1, 2, 3, 4]

end
