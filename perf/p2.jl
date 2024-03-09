using Turing, Test, AbstractMCMC, DynamicPPL, Random, Turing.RandomMeasures, Libtask

@model infiniteGMM(x) = begin
    # Hyper-parameters, i.e. concentration parameter and parameters of H.
    α = 1.0
    μ0 = 0.0
    σ0 = 1.0

    # Define random measure, e.g. Dirichlet process.
    rpm = DirichletProcess(α)

    # Define the base distribution, i.e. expected value of the Dirichlet process.
    H = Normal(μ0, σ0)

    # Latent assignment.
    z = tzeros(Int, length(x))

    # Locations of the infinitely many clusters.
    μ = tzeros(Float64, 0)

    for i in 1:length(x)

        # Number of clusters.
        K = maximum(z)
        nk = Vector{Int}(map(k -> sum(z .== k), 1:K))

        # Draw the latent assignment.
        z[i] ~ ChineseRestaurantProcess(rpm, nk)

        # Create a new cluster?
        if z[i] > K
            push!(μ, 0.0)

            # Draw location of new cluster.
            μ[z[i]] ~ H
        end

        # Draw observation.
        x[i] ~ Normal(μ[z[i]], 1.0)
    end
end

# Generate some test data.
rng = Random.seed!(1)

data = vcat(randn(rng, 10), randn(rng, 10) .- 5, randn(rng, 10) .+ 10)
data .-= mean(data)
data /= std(data)

# MCMC sampling
Random.seed!(rng, 2)
iterations = 500
model_fun = infiniteGMM(data)

m = Turing.Core.TracedModel(model_fun, Sampler(SMC(50)), VarInfo(), rng)
f = m.evaluator[1]
args = m.evaluator[2:end]

t = TapedTask(f, args...)

t.tf(args...)

@show Libtask.result(t.tf)
