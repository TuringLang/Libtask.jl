using Libtask: @might_produce
using Turing: @model, sample, SMC, PG, Normal, mean
using StableRNGs: StableRNG
using Test: @test, @testset

@model function f()
    x ~ Normal()
    y ~ Normal(x)
    return 2.0 ~ Normal(y)
end

model = f()

@testset "Turing integration" begin
    @testset "SMC" begin
        chain = sample(StableRNG(468), model, SMC(), 100; progress=false)
        @test size(chain, 1) == 100
        @test size(chain, 3) == 1
    end

    @testset "PG" begin
        chain = sample(StableRNG(468), model, PG(10), 500; progress=false)
        @test size(chain, 1) == 500
        @test mean(chain[:x]) ≈ 2 / 3 atol = 0.2
        @test mean(chain[:y]) ≈ 4 / 3 atol = 0.2
    end

    @testset "PG with keyword arguments" begin
        @model function kwarg_demo(y; n=0.0)
            x ~ Normal(n)
            return y ~ Normal(x)
        end

        # Check that enabling `might_produce` does allow sampling
        @might_produce kwarg_demo
        chain = sample(StableRNG(468), kwarg_demo(5.0), PG(20), 1000; progress=false)
        @test mean(chain[:x]) ≈ 2.5 atol = 0.2

        # Check that the keyword argument's value is respected
        chain2 = sample(
            StableRNG(468), kwarg_demo(5.0; n=10.0), PG(20), 1000; progress=false
        )
        @test mean(chain2[:x]) ≈ 7.5 atol = 0.2
    end
end
