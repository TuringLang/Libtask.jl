module Libtask

using IRTools
using MacroTools

export CTask, consume, produce
export TArray, tzeros, tfill, TRef

export TapedTask

include("tapedfunction.jl")
include("tapedtask.jl")

include("tarray.jl")
include("tref.jl")

CTask = TapedTask

end
