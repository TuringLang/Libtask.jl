module Libtask

using Libdl
using Libtask_jll

export CTask, consume, produce, TArray, tzeros, tfill, TRef

function __init__()
    @static if VERSION < v"1.6.0"
        push!(Libdl.DL_LOAD_PATH,
              Libtask_jll.get_libtask_julia_path() |> dirname)
        Libdl.dlopen(Libtask_jll.libtask_julia_path)
    end
end

include("memlayout/main.jl")
include("ctask.jl")
include("tarray.jl")
include("tref.jl")

end
