module Libtask

export CTask, consume, produce, TArray, get, tzeros, tfill, TRef

# Try to load the binary dependency
if isfile(joinpath(@__DIR__, ".." , "deps", "deps.jl"))
    include("../deps/deps.jl")
    # check_deps()
    @static if VERSION < v"1.0.9999" # (-, v1.1)
        const libtask = libtask_v1_0
    elseif VERSION < v"1.1.9999" # [v1.1, v1.2)
        const libtask = libtask_v1_1
    elseif VERSION < v"1.2.9999" # [v1.2, v1.3)
        const libtask = libtask_v1_2
    else # [v1.3, +)
        const libtask = libtask_v1_3
    end

    if !isfile(libtask)
        error("$(libtask) does not exist, Please re-run Pkg.build(\"Libtask.jl\"), and restart Julia.")
    end

    if Libdl.dlopen_e(libtask) in (C_NULL, nothing)
        error("$(libtask) cannot be opened, Please re-run Pkg.build(\"Libtask.jl\"), and restart Julia.")
    end
else
    error("Libtask is not properly installed. Please run `import Pkg; Pkg.build(\"Libtask\")`")
end

include("taskcopy.jl")
include("tarray.jl")

end
