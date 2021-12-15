@testset "Special Instructions" begin

    @testset "TapeInstruction" begin
        i1(x) = i2(x)
        i2(x) = produce(x)

        Libtask.trace_into(::typeof(i1)) = true
        Libtask.trace_into(::typeof(i2)) = true

        function f()
            t = 0
            while t < 4
                i1(t)
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
    end

end
