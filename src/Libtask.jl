module Libtask

export consume, produce, TArray, get, tzeros, tfill, TRef

include("../deps/deps.jl"); check_deps();
include("taskcopy.jl")
include("tarray.jl")
include("tref.jl")

end
