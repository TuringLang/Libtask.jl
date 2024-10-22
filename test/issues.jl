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

    @testset "Issue-148, unused argument" begin
        function f(x)
            produce(1)
        end

        ttask = TapedTask(f, 2)
        @test consume(ttask) == 1
    end

    @testset "Issue-Turing-1873, NamedTuple syntax" begin
        function g(x, y)
            c = x + y
            return (; c, x, y)
        end

        tf = Libtask.TapedFunction(g, 1, 2)
        r = tf(1, 2)
        @test r == (c=3, x=1, y=2)
    end

    @testset "Issue-Libtask-174, SSAValue=Int and static parameter" begin
        # SSAValue = Int
        function f()
            # this line generates: %1 = 1::Core.Const(1)
            r = (a = 1)
            return nothing
        end
        tf = Libtask.TapedFunction(f)
        r = tf()
        @test r == nothing

        # static parameter
        function g(::Type{T}) where {T}
            a = zeros(T, 10)
        end
        tf = Libtask.TapedFunction(g, Float64)
        r = tf(Float64)
        @test r == zeros(Float64, 10)
    end

end
