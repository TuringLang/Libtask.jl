using Base: unsafe_convert
using Libdl

@show lib = Libdl.dlopen_e("./mempool.so")
@show inspector = dlsym(lib, :jl_array_inspect)
const global_buf = Vector{UInt8}(undef, 16)
for i in 1:16
    global_buf[i] = i
end

ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
a1 = unsafe_wrap(Array, ptr, (2, 2))
@show ptr, a1
ccall(inspector, Ref{Array}, (Any, Ptr{Cvoid}), a1, C_NULL)

ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 8
a2 = unsafe_wrap(Array, ptr, (2, 2))
@show ptr, a2
ccall(inspector, Ref{Array}, (Any, Ptr{Cvoid}), a2, C_NULL)


ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 8
ccall(inspector, Ref{Array}, (Any, Ptr{Cvoid}), a1, ptr)
ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
ccall(inspector, Ref{Array}, (Any, Ptr{Cvoid}), a2, ptr)

@show a1
@show a2
