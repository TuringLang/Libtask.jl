struct TapedTaskException
    exc
end

struct TapedTask
    task::Task
    tf::TapedFunction
    counter::Ref{Int}
    produce_ch::Channel{Any}
    consume_ch::Channel{Int}
end

function TapedTask(tf::TapedFunction, args...)
    tf.owner != nothing && error("TapedFunction is owned to another task.")
    # dry_run(tf)
    isempty(tf.tape) && tf(args...)
    counter = Ref{Int}(1)
    produce_ch = Channel()
    consume_ch = Channel{Int}()
    task = @task try
        step_in(tf, counter, args)
    catch e
        put!(produce_ch, TapedTaskException(e))
        # @error "TapedTask Error: " exception=(e, catch_backtrace())
        rethrow()
    finally
        @static if VERSION >= v"1.4"
            while !isempty(produce_ch)
                yield()
            end
        end
        close(produce_ch)
        close(consume_ch)
    end
    t = TapedTask(task, tf, counter, produce_ch, consume_ch)
    # task.storage === nothing && (task.storage = IdDict())
    # task.storage[:tapedtask] = t
    tf.owner = t
    return t
end

TapedTask(f, args...) = TapedTask(TapedFunction(f, arity=length(args)), args...)
TapedTask(t::TapedTask, args...) = TapedTask(func(t), args...)
func(t::TapedTask) = t.tf.func

function step_in(tf::TapedFunction, counter::Ref{Int}, args)
    len = length(tf.tape)
    if(counter[] <= 1)
        input = map(box, args)
        tf.tape[1].input = input
    end
    while counter[] <= len
        tf.tape[counter[]]()
        counter[] += 1
    end
end

# A way (the old way) to impl `produce`, which does NOT
# support `produce` in a nested call
function internal_produce(instr::Instruction, val)
    tape = gettape(instr)
    tf = tape.owner
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

#=
# Another way to support `produce` in nested call. This way has its caveat:
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
    ct.storage[:tapedtask].tf.tape === NULL_TAPE && return false
    return true
end

function produce(val)
    is_in_tapedtask() || return nothing
    ttask = current_task().storage[:tapedtask]
    put!(ttask.produce_ch, val)
    take!(ttask.consume_ch) # wait for next consumer
    return nothing
end
=#

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

"""
    tape_copy(x)

Function `tape_copy` is used to copy data while copying a TapedTask, the
default behavior is: 1. for `Array` and `Dict`, we do `deepcopy`; 2. for
other data types, we do not copy and share the data between tasks, i.e.,
`tape_copy(x) = x`. If one wants some kinds of data to be copied, or
deeply copied, one can add a method to this function.
"""
function tape_copy end
tape_copy(x) = x
# tape_copy(x::Array) = deepcopy(x)
# tape_copy(x::Dict) = deepcopy(x)

function copy_box(old_box::Box{T}, roster::Dict{UInt64, Any}) where T
    oid = objectid(old_box)
    haskey(roster, oid) && (return roster[oid])

    # We don't know the type of box.val now, so we use Box{Any}
    new_box = Box{T}(tape_copy(old_box.val))
    roster[oid] = new_box
    return new_box
end
copy_box(o, roster::Dict{UInt64, Any}) = o

function Base.copy(t::Tape)
    old_data = t.tape
    new_data = Vector{Instruction}()
    new_tape = Tape(new_data, t.owner)

    roster = Dict{UInt64, Any}()
    for x in old_data
        input = map(x.input) do ob
            copy_box(ob, roster)
        end
        output = copy_box(x.output, roster)
        new_ins = Instruction(x.fun, input, output, new_tape)
        push!(new_data, new_ins)
    end

    return new_tape
end

function Base.copy(tf::TapedFunction)
    new_tf = TapedFunction(tf.func; arity=tf.arity)
    new_tf.ir = tf.ir
    new_tape = copy(tf.tape)
    new_tape.owner = new_tf
    new_tf.tape = new_tape
    return new_tf
end

function Base.copy(t::TapedTask)
    # t.counter[] <= 1 && error("Can't copy a TapedTask which is not running.")
    tf = copy(t.tf)
    new_t = TapedTask(tf)
    new_t.counter[] = t.counter[] + 1
    return new_t
end
