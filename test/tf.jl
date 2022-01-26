using Libtask

@testset "tapedfunction" begin
    # Test case 1: stack allocated objects are deep copied.
    @testset "Instruction{typeof(_new)}" begin
        mutable struct S
            i::Int
            S(x, y) = new(x + y)
        end

        tf = Libtask.TapedFunction(S, 1, 2)
        s1 = tf(1, 2)
        @test s1.i == 3
        newins = findall(x -> isa(x, Libtask.Instruction{typeof(Libtask._new)}), tf.tape)
        @test length(newins) == 1
    end
end
