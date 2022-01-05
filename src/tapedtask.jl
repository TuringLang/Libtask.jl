struct TapedTaskException
    exc
    backtrace
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

function TapedTask(tf::TapedFunction, args...)
    tf.owner != nothing && error("TapedFunction is owned to another task.")
    isempty(tf.tape) && tf(args...)
    produce_ch = Channel()
    consume_ch = Channel{Int}()
    task = @task try
        step_in(tf.tape, args)
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
    t = TapedTask(task, tf, produce_ch, consume_ch)
    task.storage === nothing && (task.storage = IdDict())
    task.storage[:tapedtask] = t
    tf.owner = t
    return t
end

# Issue: evaluating model without a trace, see
# https://github.com/TuringLang/Turing.jl/pull/1757#diff-8d16dd13c316055e55f300cd24294bb2f73f46cbcb5a481f8936ff56939da7ceR329
TapedTask(f, args...) = TapedTask(TapedFunction(f, arity=length(args)), args...)
TapedTask(t::TapedTask, args...) = TapedTask(func(t), args...)
func(t::TapedTask) = t.tf.func


function step_in(t::Tape, args)
    len = length(t)
    if(t.counter <= 1 && length(args) > 0)
        input = map(box, args)
        t[1].input = input
    end
    while t.counter <= len
        t[t.counter]()
        # produce and wait after an instruction is done
        ttask = t.owner.owner
        if length(ttask.produced_val) > 0
            val = pop!(ttask.produced_val)
            put!(ttask.produce_ch, val)
            take!(ttask.consume_ch) # wait for next consumer
        end
        increase_counter(t)
    end
end

next_step(t::TapedTask) = increase_counter(t.tf.tape)

#=
# ** Approach (A) to implement `produce`:
# Make`produce` a standalone instturction. This approach does NOT
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
    ct.storage[:tapedtask].tf.tape === NULL_TAPE && return false
    return true
end

function produce(val)
    is_in_tapedtask() || return nothing
    ttask = current_task().storage[:tapedtask]
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

function Base.copy(x::Instruction, on_tape::Tape, roster::Dict{UInt64, Any})
    input = map(x.input) do ob
        copy_box(ob, roster)
    end
    output = copy_box(x.output, roster)
    Instruction(x.fun, input, output, on_tape)
end

function Base.copy(t::Tape, roster::Dict{UInt64, Any})
    old_data = t.tape
    new_data = Vector{AbstractInstruction}()
    new_tape = Tape(new_data, t.counter, t.owner)

    for x in old_data
        new_ins = copy(x, new_tape, roster)
        push!(new_data, new_ins)
    end

    return new_tape
end

function Base.copy(tf::TapedFunction)
    new_tf = TapedFunction(tf.func; arity=tf.arity)
    new_tf.ir = tf.ir
    roster = Dict{UInt64, Any}()
    new_tape = copy(tf.tape, roster)
    setowner!(new_tape, new_tf)
    new_tf.tape = new_tape
    return new_tf
end

function Base.copy(t::TapedTask)
    # t.counter[] <= 1 && error("Can't copy a TapedTask which is not running.")
    tf = copy(t.tf)
    new_t = TapedTask(tf)
    new_t.task.storage = copy(t.task.storage)
    new_t.task.storage[:tapedtask] = new_t
    next_step(new_t)
    return new_t
end
