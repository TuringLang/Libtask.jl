module Libtask

using LRUCache

export TapedTask, consume, produce

export TArray, tzeros, tfill, TRef # legacy types back compat


@static if isdefined(Core, :TypedSlot) || isdefined(Core.Compiler, :TypedSlot)
    # Julia v1.10 removed Core.TypedSlot
    const TypedSlot = @static if isdefined(Core, :TypedSlot)
        Core.TypedSlot
    else
        Core.Compiler.TypedSlot
    end
else
    struct TypedSlot end # Dummy
end

include("tapedfunction.jl")
include("tapedtask.jl")

end
