using Libtask
using Test

include("tf.jl")
include("tapedtask.jl")
include("tarray.jl")
include("tref.jl")

if haskey(ENV, "BENCHMARK")
    include("benchmarks.jl")
end
