using Libtask
using Test

include("tf.jl")
include("ctask.jl")
include("tarray.jl")
include("tref.jl")

if get(ENV, "BENCHMARK", nothing) != nothing
    include("benchmarks.jl")
end
