module Libtask

using Libtask_jll

export CTask, consume, produce, TArray, tzeros, tfill, TRef

@static if VERSION < v"1.0.9999" # (-, v1.1)
    const libtask = libtask_v1_0
elseif VERSION < v"1.1.9999" # [v1.1, v1.2)
    const libtask = libtask_v1_1
elseif VERSION < v"1.2.9999" # [v1.2, v1.3)
    const libtask = libtask_v1_2
else # [v1.3, +)
    const libtask = libtask_v1_3
end

include("ctask.jl")
include("tarray.jl")
include("tref.jl")

end
