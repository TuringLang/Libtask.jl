module Libtask

# Need this for BBCode.
using Mooncake
using Mooncake: BBCode, BBlock, ID, new_inst, stmt, seed_id!
using Mooncake: IDGotoIfNot, IDGotoNode, IDPhiNode, Switch

# We'll emit `MistyClosure`s rather than `OpaqueClosure`s.
using MistyClosures

# ScopedValues only became available as part of `Base` in v1.11. Therefore, on v1.10 we
# need to use the `ScopedValues` package.
@static if VERSION < v"1.11"
    using ScopedValues: ScopedValue, with
else
    using Base.ScopedValues: ScopedValue, with
end

# Import some names from the compiler.
const CC = Core.Compiler
using Core.Compiler: Argument, IRCode, ReturnNode

include("copyable_task.jl")
include("test_utils.jl")

export TapedTask, consume, produce, get_dynamic_scope, set_dynamic_scope!

end
