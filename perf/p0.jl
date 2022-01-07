# ]add  Turing#hg/new-libtask2

using Libtask
using Turing, DynamicPPL, AdvancedPS
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

m = Turing.Core.TracedModel(gdemo(1.5, 2.), SampleFromPrior(), VarInfo())

f = m.evaluator[1];

args = m.evaluator[2:end];

@btime f(args...)
# (2.0, VarInfo (2 variables (μ, σ), dimension 2; logp: -6.162))

t = Libtask.CTask(f, args...)
# schedule(t.task) # work fine!
# @show Libtask.result(t.tf.tape)
@btime Libtask.step_in(t.tf.tape, args)

# Case 2: SMC sampler

m = Turing.Core.TracedModel(gdemo(1.5, 2.), Sampler(SMC(50)), VarInfo());
t = Libtask.CTask(m.evaluator[1], m.evaluator[2:end]...);
# schedule(t.task)
# @show Libtask.result(t.tf.tape)
@btime m.evaluator[1](m.evaluator[2:end]...)
@btime Libtask.step_in(t.tf.tape, m.evaluator[2:end])
