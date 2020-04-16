using Libtask
using IRTools
using Test

@testset "COW Test" begin

    @testset "Code Generation" begin

        function f_ct()
            ta = zeros(UInt64, 4);
            for i in 1:4
                ta[i] = hash(Libtask._current_task())
                DATA[Libtask._current_task()] = ta
            end
            ta
        end

        ir = IRTools.@code_ir f_ct()
        @show ir
        ir = Libtask.insert_copy(ir)
        print("--------\n")
        @show ir
    end


    @testset "Test on Array" begin

        DATA = Dict{Task, Array}()

        function f_ct()
            ta = zeros(UInt64, 4); # NOT a TArray!
            for i in 1:4
                ta[i] = hash(Libtask._current_task())
                DATA[Libtask._current_task()] = convert(Array, ta)
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
