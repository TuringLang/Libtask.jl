"""
    CTask

Wrapper of a [`Task`](@ref) for which deep copying of stack allocated objects is enabled.
"""
struct CTask
    task::Task

    function CTask(task::Task)
        ret = new(enable_stack_copying(task))
        task.storage === nothing && (task.storage = IdDict())
        task.storage[:ctask] = ret
        ret
    end
end

CTask(f) = CTask(Task(task_wrapper(f)))

# Iteration interface.
Base.iterate(ctask::CTask, state=nothing) = consume(ctask), nothing
Base.IteratorSize(::Type{CTask}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{CTask}) = Base.EltypeUnknown()

struct CTaskException <: Exception
    task::Task
end

function Base.showerror(io::IO, ex::CTaskException)
    println(io, "CTaskException:")
    ct = ex.task
    bt = @static if VERSION < v"1.6.0-DEV.1145"
        ct.backtrace
    else
        ct.storage[:_libtask_bt]
    end
    showerror(io, ct.exception, bt)
end

# Utility function for self-copying mechanism
_current_task() = ccall((:vanilla_get_current_task, libtask_julia), Ref{Task}, ())

n_copies() = n_copies(_current_task())
function n_copies(t::Task)
  t.storage === nothing && (t.storage = IdDict())
  return get!(t.storage, :n_copies, 0)
end

function enable_stack_copying(t::Task)
    if istaskfailed(t)
        error("only runnable or finished tasks' stack can be copied.")
    end
    # return ccall((:jl_enable_stack_copying, libtask_julia),
    #             Any, (Any,), t)::Task
    copy_stack = internal_getfield(t, :copy_stack, Int32)
    if copy_stack == 0
        internal_setfield(t, :copy_stack, Int32(1))
        internal_setfield(t, :bufsz, Csize_t(0))
        ccall((:jl_reset_task_ctx, libtask_julia),
              Cvoid, (Any, Csize_t, Csize_t, Csize_t),
              t, TASK_OFFSETS[:ctx], TASK_OFFSETS[:sizeof_ctx],
              TASK_OFFSETS[:tls_base_context])
    end
    return t
end

"""
    task_wrapper()

`task_wrapper` is a wordaround for set the result/exception to the
correct task which maybe copied/forked from another one(the original
one). Without this, the result/exception is always sent to the
original task. That is done in `JULIA_PROJECT/src/task.c`, the
function `start_task` and `finish_task`.

This workaround is not the proper way to do the work it does. The
proper way is refreshing the `current_task` (the variable `t`) in
`start_task` after the call to `jl_apply` returns.

"""
function task_wrapper(func)
    f = let func=func
        () -> begin
            try
                ct = _current_task()
                res = func()
                ct.result = res
                ct.storage === nothing && (ct.storage = IdDict())
                ct.storage[:_libtask_state] = :done
                wait()
            catch ex
                ct = _current_task()
                @static if VERSION < v"1.6.0-DEV.1145"
                    ct.exception = ex
                    ct.backtrace = catch_backtrace()
                else
                    ct._isexception = true
                    ct.storage[:_libtask_bt] = catch_backtrace()
                end
                ct.result = ex
                ct.storage === nothing && (ct.storage = IdDict())
                ct.storage[:_libtask_state] = :failed
                wait()
            end
        end
    end
    return f
end

function Base.copy(ctask::CTask)
    task = ctask.task
    if istaskfailed(task)
        error("only runnable or finished tasks can be copied.")
    end

    # memory copy
    # newtask = ccall((:jl_clone_task, libtask_julia), Any, (Any,), task)::Task
    newtask = ccall((:jl_clone_task_opaque, libtask_julia),
                    Any, (Any, Csize_t), task, TASK_OFFSETS[:END])::Task
    internal_setfield(newtask, :exception, nothing)
    internal_setfield(newtask, :backtrace, nothing)
    internal_setfield(newtask, :tls, nothing)
    internal_setfield(newtask, :result, nothing)
    internal_setfield(newtask, :donenotify, nothing)
    internal_setfield(newtask, :excstack, C_NULL)
    internal_setfield(newtask, :ptls, C_NULL)
    internal_setfield(newtask, :gcstack, C_NULL)

    if haskey(TASK_OFFSETS, :stkbuf) && haskey(TASK_OFFSETS, :bufsz)
        old_stkbuf = internal_getfield(task, :stkbuf, Ptr)
        if old_stkbuf != C_NULL
            internal_setfield(task, :bufsz, Csize_t(0))
        else
            internal_setfield(newtask, :stkbuf, C_NULL)
        end
        internal_setfield(newtask, :bufsz, Csize_t(0))
    end
    memset(newtask, 0, :locks)
    # memory copy done

    task.storage[:n_copies] = 1 + n_copies(task)
    newtask.storage = copy(task.storage)

    # copy fields not accessible in task.c
    newtask.code = task.code
    setstate!(newtask, getstate(task))
    newtask.result = task.result

    copy_tarrays(task, newtask)
    return CTask(newtask)
end

function produce(v)
    ct = _current_task()
    ct.storage === nothing && (ct.storage = IdDict())

    consumers = get!(ct.storage, :consumers, nothing)
    local empty, task
    while true
        if consumers isa Task
            task = consumers
            ct.storage[:consumers] = nothing
            empty = true
            break
        elseif consumers isa Condition && !isempty(consumers.waitq)
            task = popfirst!(consumers.waitq)
            empty = isempty(consumers.waitq)
            break
        end

        # Wait until there are more consumers.
        wait()

        # Update consumers.
        consumers = ct.storage[:consumers]
    end

    # Internal check to make sure that it is possible to switch to the consumer.
    @assert !istaskdone(task) && !istaskfailed(task)

    task.queue !== nothing && yield()

    if empty
        # Switch to the consumer.
        schedule(task, v)
        wait()

        ct = _current_task() # When a task is copied, ct should be updated to new task ID.
        while true
            q = ct.storage[:consumers]
            if isa(q,Task)
                return q.result
            elseif isa(q,Condition) && !isempty(q.waitq)
                return q.waitq[1].result
            end

            # Wait until there are more consumers.
            wait()
        end
    else
        schedule(task, v)
        # make sure `task` runs before us. otherwise, the producer might
        # finish before `t` runs again, causing it to see the producer
        # as done, causing done(::Task, _) to miss the value `v`.
        # see issue #7727
        yield()
        return consumers.waitq[1].result
    end
end


function consume(ctask::CTask, values...)
    # Check if the producer is done.
    producer = ctask.task
    istaskdone(producer) && return wait(producer)

    # Obtain the current task and set its result.
    ct = _current_task()
    ct.result = length(values) == 1 ? values[1] : values

    # Obtain the consumers listening for the producer.
    producer.storage === nothing && (producer.storage = IdDict())
    consumers = get!(producer.storage, :consumers, nothing)

    if consumers === nothing || (consumers isa Condition && isempty(consumers.waitq))
        # Set the current task as consumer if none exists.
        producer.storage[:consumers] = ct
    else
        # Otherwise add the current task to the waiting queue.
        if consumers isa Task
            # If there is no queue currently but only a single task, replace it with a
            # waiting queue with this task.
            producer.storage[:consumers] = Condition()
            push!(producer.storage[:consumers].waitq, consumers)
        end
        push!(producer.storage[:consumers].waitq, ct)
    end

    if !istaskdone(producer) && !istaskfailed(producer)
        # Switch to the producer.
        schedule(producer)
        yield()

        # Update the state if possible.
        if producer.storage isa IdDict && haskey(producer.storage, :_libtask_state)
            setstate!(producer, producer.storage[:_libtask_state])
        end

        # If the task failed, throw an exception.
        istaskfailed(producer) && throw(CTaskException(producer))

        # If the task is done return the result.
        istaskdone(producer) && return producer.result
    end

    wait()
end

function getstate(task::Task)
    @static if VERSION < v"1.6.0-DEV.618"
        return task.state
    else
        return task._state
    end
end

function setstate!(task::Task, state)
    @static if VERSION < v"1.6.0-DEV.618"
        task.state = state
    else
        if state === :runnable
            task._state = Base.task_state_runnable
        elseif state === :done
            task._state = Base.task_state_done
        elseif state === :failed
            task._state = Base.task_state_failed
        else
            task._state = state
        end
    end
end
