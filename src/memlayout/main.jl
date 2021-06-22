const ALL_TASK_OFFSETS =
    Dict{Tuple{String, VersionNumber}, Dict{Symbol, Int}}()


include("linux-x86-v1_3_1.jl")
include("linux-x86-v1_4_2.jl")
include("linux-x86-v1_5_3.jl")
include("linux-x86-v1_5_4.jl")
include("linux-x86-v1_6_1.jl")
include("linux-x86-v1_8_0.jl")

include("linux-x86_64-v1_3_1.jl")
include("linux-x86_64-v1_4_2.jl")
include("linux-x86_64-v1_5_3.jl")
include("linux-x86_64-v1_5_4.jl")
include("linux-x86_64-v1_6_1.jl")
include("linux-x86_64-v1_8_0.jl")


const PLATFORM = @static if Sys.islinux()
    "linux-" * string(Sys.ARCH)
elseif Sys.iswindows()
    "windows-" * string(Sys.ARCH)
elseif Sys.isapple()
    "darwin-" * string(Sys.ARCH)
end

function find_offsets()
    candi = [v for (p, v) in keys(ALL_TASK_OFFSETS) if p == PLATFORM]
    sort!(candi)
    candi = [v for v in candi if v <= VERSION]
    isempty(candi) && error("No suitable offsets")
    ALL_TASK_OFFSETS[(PLATFORM, candi[end])]
end

const TASK_OFFSETS = find_offsets()

## utilities

# getter
function internal_getfield(task, field, ::Type{Ptr})
    ccall((:jl_getfield_ptr, libtask_julia),
          Ptr{Cvoid}, (Any, Csize_t),
          task, TASK_OFFSETS[field])
end

function internal_getfield(task, field, ::Type{Int32})
    ccall((:jl_getfield_int32_t, libtask_julia),
          Int32, (Any, Csize_t), task, TASK_OFFSETS[field])
end

# setter
function internal_setfield(task, field, ::Nothing)
    haskey(TASK_OFFSETS, field) && ccall(
        (:jl_setfield_nothing, libtask_julia),
        Any, (Any, Csize_t), task, TASK_OFFSETS[field])
end

function internal_setfield(task, field, p::Ptr{Nothing})
    p == C_NULL || error("this function is only for setting NULL value")
    haskey(TASK_OFFSETS, field) && ccall(
        (:jl_setfield_null, libtask_julia),
        Any, (Any, Csize_t), task, TASK_OFFSETS[field])
end

function internal_setfield(task, field, val::Csize_t)
    ccall((:jl_setfield_size_t, libtask_julia),
          Any, (Any, Csize_t, Csize_t),
          task, TASK_OFFSETS[field], val)
end

function internal_setfield(task, field, val::Int32)
    ccall((:jl_setfield_int32_t, libtask_julia),
          Any, (Any, Csize_t, Int32),
          task, TASK_OFFSETS[field], val)
end

function memset(task, val, offset_field; end_field=:END)
    haskey(TASK_OFFSETS, offset_field) && ccall(
        (:jl_memset, libtask_julia),
        Cvoid, (Any, Csize_t, Csize_t, Int),
        task, TASK_OFFSETS[offset_field], TASK_OFFSETS[end_field], val)
end
