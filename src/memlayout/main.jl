const ALL_TASK_OFFSETS = Dict{Tuple{String, VersionNumber}, Dict{Symbol, Int}}()

include("linux-x64-v1_3_1.jl")
include("linux-x64-v1_4_2.jl")
include("linux-x64-v1_5_3.jl")
include("linux-x64-v1_5_4.jl")
include("linux-x64-v1_6_1.jl")
include("linux-x64-v1_8_0.jl")

const TASK_OFFSETS = ALL_TASK_OFFSETS[("linux-x64", v"1.6.1")]
