module Libtask

using FunctionWrappers: FunctionWrapper
using LRUCache

export TapedTask, consume, produce

export TArray, tzeros, tfill, TRef # legacy types back compat

include("tapedfunction.jl")
include("tapedtask.jl")

end
