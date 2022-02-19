struct TapedTaskException
    exc::Exception
    backtrace::Vector{Any}
end

struct TapedTask
    task::Task
    tf::TapedFunction
    produce_ch::Channel{Any}
    consume_ch::Channel{Int}
    produced_val::Vector{Any}

    function TapedTask(
        t::Task, tf::TapedFunction, pch::Channel{Any}, cch::Channel{Int})
        new(t, tf, pch, cch, Any[])
    end
end

function producer()
    ttask = current_task().storage[:tapedtask]::TapedTask
    if length(ttask.produced_val) > 0
        val = pop!(ttask.produced_val)
        put!(ttask.produce_ch, val)
        take!(ttask.consume_ch) # wait for next consumer
    end
    return nothing
end

function wrap_task(tf, produce_ch, consume_ch, args...)
    try
        tf(args...; callback=producer)
    catch e
        bt = catch_backtrace()
        put!(produce_ch, TapedTaskException(e, bt))
        # @error "TapedTask Error: " exception=(e, bt)
        rethrow()
    finally
        @static if VERSION >= v"1.4"
            # we don't do this under Julia 1.3, because `isempty` always hangs on
            # an empty channel.
            while !isempty(produce_ch)
                yield()
            end
        end
        close(produce_ch)
        close(consume_ch)
    end
end

function TapedTask(tf::TapedFunction, args...)
    produce_ch = Channel()
    consume_ch = Channel{Int}()
    task = @task wrap_task(tf, produce_ch, consume_ch, args...)
    t = TapedTask(task, tf, produce_ch, consume_ch)
    task.storage === nothing && (task.storage = IdDict())
    task.storage[:tapedtask] = t
    tf.owner = t
    return t
end

# NOTE: evaluating model without a trace, see
# https://github.com/TuringLang/Turing.jl/pull/1757#diff-8d16dd13c316055e55f300cd24294bb2f73f46cbcb5a481f8936ff56939da7ceR329
function TapedTask(f, args...)
    tf = TapedFunction(f, args...; cache=true, init=true)
    TapedTask(tf, args...)
end

TapedTask(t::TapedTask, args...) = TapedTask(func(t), args...)
func(t::TapedTask) = t.tf.func

#=
# ** Approach (A) to implement `produce`:
# Make`produce` a standalone instturction. This approach does NOT
# support `produce` in a nested call
function internal_produce(instr::Instruction, val)
    tf = instr.tape
    ttask = tf.owner
    put!(ttask.produce_ch, val)
    take!(ttask.consume_ch) # wait for next consumer
end

function produce(val)
    error("Libtask.produce can only be directly called in a task!")
end

function (instr::Instruction{typeof(produce)})()
    args = val(instr.input[1])
    internal_produce(instr, args)
end
=#


# ** Approach (B) to implement `produce`:
# This way has its caveat:
# `produce` may deeply hide in an instruction, but not be an instruction
# itself, and when we copy a task, the newly copied task will resume from
# the instruction after the one which contains this `produce` call. If the
# call to `produce` is not the last expression in the instuction, that
# instruction will not be whole executed in the copied task.
@inline function is_in_tapedtask()
    ct = current_task()
    ct.storage === nothing && return false
    haskey(ct.storage, :tapedtask) || return false
    # check if we are recording a tape
    ttask = ct.storage[:tapedtask]::TapedTask
    isempty(ttask.tf.tape) && return false
    return true
end

function produce(val)
    is_in_tapedtask() || return nothing
    ttask = current_task().storage[:tapedtask]::TapedTask
    length(ttask.produced_val) > 1 &&
        error("There is a produced value which is not consumed.")
    push!(ttask.produced_val, val)
    return nothing
end

function consume(ttask::TapedTask)
    if istaskstarted(ttask.task)
        # tell producer that a consumer is coming
        put!(ttask.consume_ch, 0)
    else
        schedule(ttask.task)
    end

    val = try
        take!(ttask.produce_ch)
    catch e
        isa(e, InvalidStateException) || rethrow()
        istaskfailed(ttask.task) && throw(ttask.task.exception)
        # TODO: we return nothing to indicate the end of a task,
        #       remove this when AdvancedPS is udpated.
        istaskdone(ttask.task) && return nothing
    end

    # yield to let the task resume, this is necessary when there's
    # an exception is thrown in the task, it gives the task the chance
    # to rethow the exception and set its proper status:
    yield()
    isa(val, TapedTaskException) && throw(val.exc)
    return val
end

# Iteration interface.
function Base.iterate(t::TapedTask, state=nothing)
    try
        consume(t), nothing
    catch ex
        !isa(ex, InvalidStateException) && rethrow
        nothing
    end
end
Base.IteratorSize(::Type{TapedTask}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{TapedTask}) = Base.EltypeUnknown()


# copy the task

function Base.copy(t::TapedTask)
    tf = copy(t.tf)
    new_t = TapedTask(tf)
    storage = t.task.storage::IdDict{Any,Any}
    new_t.task.storage = copy(storage)
    new_t.task.storage[:tapedtask] = new_t
    return new_t
end
