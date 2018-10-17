module Libtask

export consume, produce, TArray, get, tzeros, tfill

include("../deps/deps.jl"); check_deps();
include("taskcopy.jl")
include("tarray.jl")

end
