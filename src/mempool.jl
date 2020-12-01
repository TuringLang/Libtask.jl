using Base: unsafe_convert
using Libdl

@show lib = Libdl.dlopen_e("./mempool.so")
@show inspector = dlsym(lib, :jl_array_inspect)
@show obj_inspector = dlsym(lib, :jl_obj_inspect)
@show stack_alloc = dlsym(lib, :lt_stack_alloc)
@show copy_to_stack_array = dlsym(lib, :lt_copy_to_stack_array)

# global memory buffer
const global_buf = Vector{UInt8}(undef, 16)
for i in 1:16
    global_buf[i] = i
end

# common test
println("============= common test =")

function entrance(stack_buffer)
    # my caller is `lt_stack_alloc`, when I return,
    # the stack-allocated buffer will be released.
    ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
    a1 = unsafe_wrap(Array{UInt8, 2}, ptr, (2, 2))
    println("Hi, I am from Julia")
    println(typeof(a1))
    println(stack_buffer)
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    @show dest

    a2 = ccall(copy_to_stack_array, Any, (Any, Ptr{Cvoid}), a1, dest)
    @show a2, a2[1], a2[2]
end

function test()
    ccall(stack_alloc, Any, (Any, UInt), entrance, 1024)
end

test()

# Task test
println("============= task test =")

tr = Ref{Task}()
t1 = @task for _ in 1:4
    @show "---"
    yieldto(tr[])
end

function entrance_t(stack_buffer)
    # my caller is `lt_stack_alloc`, when I return,
    # the stack-allocated buffer will be released.
    ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
    a1 = unsafe_wrap(Array{UInt8, 2}, ptr, (2, 2))
    println("Hi, I am from Julia")
    println(typeof(a1))
    println(stack_buffer)
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    @show dest
    a2 = ccall(copy_to_stack_array, Any, (Any, Ptr{Cvoid}), a1, dest)
    @show a2, a2[1], a2[2]
    istaskdone(t1) || yieldto(t1)
    @show a2, a2[1], a2[2]
    istaskdone(t1) || yieldto(t1)
    @show a2, a2[1], a2[2]
    istaskdone(t1) || yieldto(t1)
end

function test_t()
    ccall(stack_alloc, Any, (Any, UInt), entrance_t, 1024)
end

t = @task test_t()
tr[] = t
schedule(t)
wait(t)



# Libtask test
println("============= libtask test =")

using Libtask

function lt_e(stack_buffer)
    ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
    a1 = unsafe_wrap(Array{UInt8, 2}, ptr, (2, 2))
    println("Hi, I am from Julia")
    println(typeof(a1))
    println(stack_buffer)
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    @show dest

    a2 = ccall(copy_to_stack_array, Any, (Any, Ptr{Cvoid}), a1, dest)
    # @show "something..." # why we must have this line?
    # @show a2

    t = 1
    while true
        produce(a2[t])
        t = 1 + t
    end
end

function lt_f()
    ccall(stack_alloc, Any, (Any, UInt), lt_e, 1024)
end

ctask = CTask(lt_f)

@show consume(ctask) # 0
@show consume(ctask) # 1


@show "gonna copy!" # And why this line?
a = copy(ctask)
@show "after copy!"
@show consume(a) # 2
@show consume(a) # 3
