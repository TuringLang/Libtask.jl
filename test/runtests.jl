using Libtask
using Test

include("ctask.jl")
include("special-instuctions.jl")
include("tarray.jl")
include("tref.jl")

if get(ENV, "BENCHMARK", nothing) != nothing
    include("benchmarks.jl")
end
