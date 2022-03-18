using Libtask

@testset "tapedfunction" begin
    # Test case 1: stack allocated objects are deep copied.
    @testset "Instruction{typeof(__new__)}" begin
        mutable struct S
            i::Int
            S(x, y) = new(x + y)
        end

        tf = Libtask.TapedFunction(S, 1, 2)
        s1 = tf(1, 2)
        @test s1.i == 3
        newins = findall(x -> isa(x, Libtask.Instruction{typeof(Libtask.__new__)}), tf.tape)
        @test length(newins) == 1
    end

    @testset "Compiled Tape" begin
        function g(x, y)
            if x>y
                r= string(sin(x))
            else
                r= sin(x) * cos(y)
            end
            return r
        end

        tf = Libtask.TapedFunction(g, 1., 2.)
        ctf = Libtask.compile(tf)
        r = ctf(1., 2.)

        @test typeof(r) === Float64
    end
end
