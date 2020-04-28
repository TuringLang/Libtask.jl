"""
    CTask

Wrapper of a [`Task`](@ref) for which deep copying of stack allocated objects is enabled.
"""
struct CTask
    task::Task

    function CTask(task::Task)
        new(enable_stack_copying(task))
    end
end

CTask(f) = CTask(Task(task_wrapper(f)))

struct CTaskException <: Exception
    task::Task
end

function Base.showerror(io::IO, ex::CTaskException)
    println(io, "CTaskException:")
    showerror(io, ex.task.exception, ex.task.backtrace)
end

# Utility function for self-copying mechanism
_current_task() = ccall((:vanilla_get_current_task, libtask), Ref{Task}, ())

n_copies() = n_copies(_current_task())
function n_copies(t::Task)
  t.storage === nothing && (t.storage = IdDict())
  return get!(t.storage, :n_copies, 0)
end

function enable_stack_copying(t::Task)
    t.state != :runnable && t.state != :done &&
        error("Only runnable or finished tasks' stack can be copied.")
    return ccall((:jl_enable_stack_copying, libtask), Any, (Any,), t)::Task
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
    () ->
    try
        ct = _current_task()
        res = func()
        ct.result = res
        ct.state = :done
        wait()
    catch ex
        ct = _current_task()
        ct.exception = ex
        ct.result = ex
        ct.state = :failed
        ct.backtrace = catch_backtrace()
        wait()
    end
end

function Base.copy(ctask::CTask)
    task = ctask.task
    task.state != :runnable && task.state != :done &&
        error("only runnable or finished tasks can be copied.")

    newtask = ccall((:jl_clone_task, libtask), Any, (Any,), task)::Task

    task.storage[:n_copies] = 1 + n_copies(task)
    newtask.storage = copy(task.storage)

    # copy fields not accessible in task.c
    newtask.code = task.code
    newtask.state = task.state
    newtask.result = task.result
    @static if VERSION < v"1.1"
        newtask.parent = task.parent
    end

    if isdefined(task, :last)
        newtask.last = nothing
    end

    return CTask(newtask)
end

# Forward methods to underlying `Task`.
for f in (:istaskdone, :istaskstarted, :istaskfailed)
    @eval begin
        Base.$f(ctask::CTask) = $f(ctask.task)
    end
end

function produce(v)
    ct = _current_task()

    if ct.storage == nothing
        ct.storage = IdDict()
    end

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
    @assert task.state in (:runnable, :queued)

    @static if VERSION < v"1.1.9999"
        task.state === :queued && yield()
    else
        task.queue !== nothing && yield()
    end

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

    if producer.state === :runnable
        # Switch to the producer.
        Base.schedule(producer)
        yield()

        # Throw an exception if the task failed.
        if istaskfailed(producer)
            throw(CTaskException(producer))
        end

        # Otherwise return the result.
        if istaskdone(producer)
            return Base.task_result(producer)
        end
    end

    wait()
end
