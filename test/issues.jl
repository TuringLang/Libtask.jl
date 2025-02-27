@testset "Issues" begin
    @testset "Issue: PR-86 (DynamicPPL.jl/pull/261)" begin
        function f()
            t = Array{Int}(undef, 1)
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

    # TODO: this test will need to change because I'm going to modify the interface _very_
    # slightly.
    @testset "Issue-140, copy unstarted task" begin
        function f(x)
            for i in 1:3
                produce(i + x)
            end
        end

        ttask = TapedTask(f, 3)
        ttask2 = copy(ttask)
        @test consume(ttask2) == 4
        ttask3 = copy(ttask; args=(4,))
        @test consume(ttask3) == 5
    end

end
