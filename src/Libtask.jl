module Libtask

export CTask, consume, produce, TArray, get, tzeros, tfill, TRef

include("../deps/deps.jl"); check_deps();
include("taskcopy.jl")
include("tarray.jl")

end
