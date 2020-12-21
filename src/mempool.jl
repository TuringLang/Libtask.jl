using Base: unsafe_convert
using Libdl

@show lib = Libdl.dlopen_e("./mempool.so")
@show inspector = dlsym(lib, :jl_array_inspect)
@show obj_inspector = dlsym(lib, :jl_obj_inspect)
@show call_with_stackbuf = dlsym(lib, :lt_call_with_stackbuf)
@show copy_to_stack_array = dlsym(lib, :lt_copy_to_stack_array)


a = [1 3; 2 4]

# global memory buffer and const array
const global_buf = Vector{UInt8}(undef, 16)
for i in 1:16
    global_buf[i] = i
end

const global_array = [10 30; 20 40]

function get_array(mempool=false)
    if mempool
        ptr = unsafe_convert(Ptr{UInt8}, global_buf) + 0
        # a1 is allocated on heap, both of the box-object and the data buffer
        a1 = unsafe_wrap(Array{UInt8, 2}, ptr, (2, 2))
        return a1
    else
        return global_array
    end
end

function copy_to_stack(obj, dest; method=:noncopy) # :noncopy|:stkcopy|:weakref
    method == :noncopy && return obj
    o = ccall(copy_to_stack_array, Any, (Any, Ptr{Cvoid}), obj, dest)
    method == :weakref && return WeakRef(o)
    return o
end

stkobj(a) = a
stkobj(a::WeakRef) = a.value

# common test
println("============= common test =")
function entrance(stack_buffer)
    # my caller is `lt_call_with_stackbuf`, when I return,
    # the stack-allocated buffer will be released.
    a1 = get_array()
    println("Hi, I am from Julia")
    println(typeof(a1))
    println(stack_buffer)
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    @show dest

    # ccall(copy_to_stack_array, Any, (Any, Ptr{Cvoid}), a1, dest)
    a2 = copy_to_stack(a1, dest)
    @show a2, stkobj(a2)[1], stkobj(a2).value[2]
    GC.gc()
end

function test()
    ccall(call_with_stackbuf, Any, (Any, UInt), entrance, 1024)
end

test()

# Task test
println("============= task test =")

tr = Ref{Task}()
t1 = @task for _ in 1:4
    @show "---"
    yieldto(tr[])
    GC.gc()
end

function entrance_t(stack_buffer)
    # my caller is `lt_call_with_stackbuf`, when I return,
    # the stack-allocated buffer will be released.

    a1 = get_array()
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    # @show dest
    a2 = copy_to_stack(a1, dest)
    @show a2, stkobj(a2)[1], stkobj(a2)[2]
    istaskdone(t1) || yieldto(t1)
    @show a2, stkobj(a2)[1], stkobj(a2)[2]
    istaskdone(t1) || yieldto(t1)
    @show a2, stkobj(a2)[1], stkobj(a2)[2]
    istaskdone(t1) || yieldto(t1)
end

function test_t()
    ccall(call_with_stackbuf, Any, (Any, UInt), entrance_t, 1024)
end

tr[] = @task test_t()
schedule(tr[])
wait(tr[])
@show tr[].state
GC.gc()


# Libtask test
println("============= libtask test =")

using Libtask

function lt_e(stack_buffer)
    a1 = get_array()
    dest = Core.bitcast(Ptr{Cvoid}, stack_buffer)
    a2 = copy_to_stack(a1, dest)

    t = 1
    while true
        produce(stkobj(a2)[t])
        t = 1 + t
    end
end

function lt_f()
    ccall(call_with_stackbuf, Any, (Any, UInt), lt_e, 1024)
end

ctask = CTask(lt_f)

@show consume(ctask) # 0
@show consume(ctask) # 1


# @show "gonna copy!" # And why this line?
GC.enable(false)
a = copy(ctask)
GC.enable(true)
@show "after copy!"
GC.gc()
@show consume(a) # 2
@show consume(a) # 3
