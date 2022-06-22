using Libtask
using Test

include("tf.jl")
include("tapedtask.jl")
include("tape_copy.jl")
include("issues.jl")

if haskey(ENV, "BENCHMARK")
    include("benchmarks.jl")
end
