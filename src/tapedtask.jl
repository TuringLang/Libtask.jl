struct TapedTaskException
    exc::Exception
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

const TRCache = LRU{Any, Any}(maxsize=10)

function TapedTask(tf::TapedFunction, args...)
    produce_ch = Channel()
    consume_ch = Channel{Int}()
    task = @task try
        step_in(tf, args)
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

# NOTE: evaluating model without a trace, see
# https://github.com/TuringLang/Turing.jl/pull/1757#diff-8d16dd13c316055e55f300cd24294bb2f73f46cbcb5a481f8936ff56939da7ceR329
function TapedTask(f, args...)
    cache_key = (f, typeof.(args)...)
    if haskey(TRCache, cache_key)
        cached_tf = TRCache[cache_key]
        # Here we don't need change the initial arguments of the tape,
        # it will be set when we `step_in` to the tape.
        tf = copy(cached_tf)
        tf.counter = 1
    else
        tf = TapedFunction(f, args...)
        TRCache[cache_key] = tf
    end
    TapedTask(tf, args...)
end

TapedTask(t::TapedTask, args...) = TapedTask(func(t), args...)
func(t::TapedTask) = t.tf.func

function step_in(tf::TapedFunction, args)
    ttask = tf.owner

    if(tf.counter <= 1 && length(args) > 0)
        input = map(Box{Any}, args)
        tf[1].input = input
    end
    while true
        ins = tf[tf.counter]
        ins()

        # produce and wait after an instruction is done
        if length(ttask.produced_val) > 0
            val = pop!(ttask.produced_val)
            put!(ttask.produce_ch, val)
            take!(ttask.consume_ch) # wait for next consumer
        end

        isa(ins, ReturnInstruction) && break
    end
end

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
    isempty(ct.storage[:tapedtask].tf.tape) && return false
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

function Base.copy(x::Instruction, on_tape::Taped, roster::Dict{UInt64, Any})
    # func may also be a boxed variable
    func = copy_box(x.func, roster)
    input = map(x.input) do ob
        copy_box(ob, roster)
    end
    output = copy_box(x.output, roster)
    Instruction(func, input, output, on_tape)
end

function Base.copy(x::BlockInstruction, on_tape::Taped, roster::Dict{UInt64, Any})
    args = map(x.args) do ob
        copy_box(ob, roster)
    end
    BlockInstruction(x.block_id, args, on_tape)
end

function Base.copy(x::BranchInstruction, on_tape::Taped, roster::Dict{UInt64, Any})
    cond = copy_box(x.condition, roster)
    args = map(x.args) do ob
        copy_box(ob, roster)
    end

    BranchInstruction(cond, x.block_id, args, on_tape)
end

function Base.copy(x::ReturnInstruction, on_tape::Taped, roster::Dict{UInt64, Any})
    arg = copy_box(x.arg, roster)
    ReturnInstruction(arg, on_tape)
end

function Base.copy(old_tape::RawTape, on_tape::Taped, roster::Dict{UInt64, Any})
    new_tape = RawTape(undef, length(old_tape))
    for (i, x) in enumerate(old_tape)
        new_ins = copy(x, on_tape, roster)
        new_tape[i] = new_ins
    end

    init_ins = Instruction(args_initializer(new_tape[2]), tuple(new_tape[2].args[2:end]...),
                           Box{Any}(nothing), on_tape)
    new_tape[1] = init_ins
    return new_tape
end

function Base.copy(tf::TapedFunction)
    new_tf = TapedFunction(tf.func; init=false)
    new_tf.block_map = tf.block_map
    new_tf.ir = tf.ir
    roster = Dict{UInt64, Any}()
    new_tape = copy(tf.tape, new_tf, roster)
    new_tf.tape = new_tape
    new_tf.counter = tf.counter
    return new_tf
end

function Base.copy(t::TapedTask)
    # t.counter[] <= 1 && error("Can't copy a TapedTask which is not running.")
    tf = copy(t.tf)
    new_t = TapedTask(tf)
    new_t.task.storage = copy(t.task.storage)
    new_t.task.storage[:tapedtask] = new_t
    return new_t
end
