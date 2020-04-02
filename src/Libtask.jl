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

include("taskcopy.jl")
include("tarray.jl")

end
