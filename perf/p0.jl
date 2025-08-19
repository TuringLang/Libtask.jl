using Random
using Libtask
using Turing, DynamicPPL, AdvancedPS
using BenchmarkTools

@model gdemo(x, y) = begin
    # Assumptions
    σ ~ InverseGamma(2, 3)
    μ ~ Normal(0, sqrt(σ))
    # Observations
    x ~ Normal(μ, sqrt(σ))
    y ~ Normal(μ, sqrt(σ))
end

# Case 1: Sample from the prior.
rng = MersenneTwister()
m = Turing.Inference.TracedModel(gdemo(1.5, 2.0), SampleFromPrior(), VarInfo(), rng)
f = m.evaluator[1];
args = m.evaluator[2:end];

println("Directly call...")
@btime f(args...)
# (2.0, VarInfo (2 variables (μ, σ), dimension 2; logp: -6.162))
println("TapedTask construction...")
t = @btime TapedTask(f, args...)
println("Run a tape...")
@btime t.tf(args...)

# Case 2: SMC sampler
m = Turing.Inference.TracedModel(gdemo(1.5, 2.0), Sampler(SMC(50)), VarInfo(), rng)
f = m.evaluator[1];
args = m.evaluator[2:end];

println("Directly call...")
@btime f(args...)
println("TapedTask construction...")
t = @btime TapedTask(f, args...)
println("Run a tape...")
@btime t.tf(args...)
