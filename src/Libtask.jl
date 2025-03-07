module Libtask

# Need this for BBCode.
using Mooncake
using Mooncake: BBCode, BBlock, ID, new_inst, stmt, seed_id!
using Mooncake: IDGotoIfNot, IDGotoNode, IDPhiNode, Switch

# We'll emit `MistyClosure`s rather than `OpaqueClosure`s.
using MistyClosures

# Import some names from the compiler.
const CC = Core.Compiler
using Core.Compiler: Argument, IRCode, ReturnNode

include("copyable_task.jl")
include("test_utils.jl")

export TapedTask, consume, produce, get_dynamic_scope, set_dynamic_scope!

end
