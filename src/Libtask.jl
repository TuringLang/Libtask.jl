module Libtask

# We'll emit `MistyClosure`s rather than `OpaqueClosure`s.
using MistyClosures

# Import some names from the compiler.
const CC = Core.Compiler
using Core: OpaqueClosure
using Core.Compiler: Argument, IRCode, ReturnNode

# IR-related functionality from Mooncake.
include("utils.jl")
include("bbcode.jl")
using .BasicBlockCode

include("copyable_task.jl")
include("test_utils.jl")

export TapedTask, consume, produce, get_taped_globals, set_taped_globals!

end
