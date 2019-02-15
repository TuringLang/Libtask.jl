module Libtask

export enable_stack_copying, consume, produce, TArray, get, tzeros, tfill, TRef

include("../deps/deps.jl"); check_deps();
include("taskcopy.jl")
include("tarray.jl")

end
