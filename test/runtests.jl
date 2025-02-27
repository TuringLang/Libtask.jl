using Libtask
using Test

include("copyable_task.jl")
include("issues.jl")

if haskey(ENV, "BENCHMARK")
    include("benchmarks.jl")
end
