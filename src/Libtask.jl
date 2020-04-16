module Libtask

export CTask, consume, produce, TArray, get, tzeros, tfill, TRef

# Try to load the binary dependency
const depsjl_path = joinpath(@__DIR__, "..", "deps", "deps.jl")
if !isfile(depsjl_path)
    error("Libtask is not properly installed. Please run `import Pkg; Pkg.build(\"Libtask\")`")
end
include(depsjl_path)

function __init__()
    check_deps()
end

@static if VERSION < v"1.0.9999" # (-, v1.1)
    const libtask = libtask_v1_0
elseif VERSION < v"1.1.9999" # [v1.1, v1.2)
    const libtask = libtask_v1_1
elseif VERSION < v"1.2.9999" # [v1.2, v1.3)
    const libtask = libtask_v1_2
else # [v1.3, +)
    const libtask = libtask_v1_3
end

include("taskcopy.jl")
include("cow.jl")
include("tarray.jl")

end
