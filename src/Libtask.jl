module Libtask

using FunctionWrappers: FunctionWrapper
using LRUCache

export TapedTask, consume, produce

export TArray, tzeros, tfill, TRef # legacy types back compat


const TypedSlot = @static if isdefined(Core, :TypedSlot) # Julia v1.10 removed Core.TypedSlot
    Core.TypedSlot
else
    Core.Compiler.TypedSlot
end

include("tapedfunction.jl")
include("tapedtask.jl")

end
