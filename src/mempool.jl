using Base: unsafe_convert
using Libdl

@show lib = Libdl.dlopen_e("./mempool.so")
@show inspector = dlsym(lib, :jl_array_inspect)
@show obj_inspector = dlsym(lib, :jl_obj_inspect)
@show icaller = dlsym(lib, :jl_icaller)
@show a_copy = dlsym(lib, :jl_copy_array_to_stack)

struct S
    i::Int
end

function t1()
    a0 = [1 2; 3 4]
    ccall(obj_inspector, Cvoid, (Any,), a0)

    s0 = S(1)
    ccall(obj_inspector, Cvoid, (Any,), s0)
    s1 = S(1)
    ccall(obj_inspector, Cvoid, (Any,), s1)

    d = 1
    ccall(obj_inspector, Cvoid, (Any,), d)
    d = 2
    ccall(obj_inspector, Cvoid, (Any,), d)
end


const global_buf = Vector{UInt8}(undef, 16)
for i in 1:16
    global_buf[i] = i
end

function t2()

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
end

struct B
    i::Int
    j::Int
    k::Int
    x::Int
end

function t(v)
    ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
    a1 = unsafe_wrap(Array, ptr, (2, 2))

    println("Hi, I am from Julia")
    println(v)
    dest = Core.bitcast(Ptr{Cvoid}, v)
    @show dest

    a2 = ccall(a_copy, Any, (Any, Ptr{Cvoid}), a1, dest)
    return a2
end

x = ccall(icaller, Any, (Any,), t)
# print(x[1])
# t(1)
