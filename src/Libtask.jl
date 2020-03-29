module Libtask

export CTask, consume, produce, TArray, get, tzeros, tfill, TRef

# Try to load the binary dependency
if isfile(joinpath(@__DIR__, ".." , "deps", "deps.jl"))
    include("../deps/deps.jl")
    check_deps()
else
    error("Libtask is not properly installed. Please run `import Pkg; Pkg.build(\"Libtask\")`")
end

include("taskcopy.jl")
include("tarray.jl")

end
