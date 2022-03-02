module Libtask

using CodeInfoTools
using MacroTools

using LRUCache

export TapedTask, consume, produce
export TArray, tzeros, tfill, TRef

include("tapedfunction.jl")
include("tapedtask.jl")

include("tarray.jl")
include("tref.jl")

end
