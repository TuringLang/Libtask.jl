using Libtask
using Turing, DynamicPPL, AdvancedPS, Random
using BenchmarkTools

@model gdemo(x, y) = begin
    # Assumptions
    σ ~ InverseGamma(2,3)
    μ ~ Normal(0,sqrt(σ))
    # Observations
    x ~ Normal(μ, sqrt(σ))
    y ~ Normal(μ, sqrt(σ))
end


# Case 1: Sample from the prior.
m = Turing.Core.TracedModel(
    gdemo(1.5, 2.), SampleFromPrior(), VarInfo(), MersenneTwister(123456)
);
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
m = Turing.Core.TracedModel(
    gdemo(1.5, 2.), Sampler(SMC(50)), VarInfo(), MersenneTwister(123456)
);
f = m.evaluator[1];
args = m.evaluator[2:end];

println("Directly call...")
@btime f(args...)
println("TapedTask construction...")
t = @btime TapedTask(f, args...)
println("Run a tape...")
@btime t.tf(args...)
