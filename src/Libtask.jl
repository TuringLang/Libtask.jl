module Libtask

export CTask, consume, produce, TArray, get, tzeros, tfill, TRef

if !isfile(joinpath((@__DIR__) |> dirname, "deps/desp.jl"))
    import Pkg
    Pkg.build("Libtask")
end

include("../deps/deps.jl"); check_deps();
include("taskcopy.jl")
include("tarray.jl")

end
