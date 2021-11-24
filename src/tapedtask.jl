struct TapedTaskException
    exc
end

struct TapedTask
    task::Task
    tf::TapedFunction
    counter::Ref{Int}
    channel::Channel{Any}
end

function TapedTask(tf::TapedFunction, args...)
    tf.owner != nothing && error("TapedFunction is owned to another task.")
    # dry_run(tf)
    isempty(tf.tape) && tf(args...)
    counter = Ref{Int}(1)
    ch = Channel()
    task = @task try
        step_in(tf, counter, args)
    catch e
        put!(ch, TapedTaskException(e))
        # @error "TapedTask Error: " exception=(e, catch_backtrace())
        rethrow()
    finally
        while !isempty(ch)
            yield()
        end
        close(ch)
    end
    t = TapedTask(task, tf, counter, ch)
    tf.owner = t
    return t
end

TapedTask(f::Function, args...) = TapedTask(TapedFunction(f, arity=length(args)), args...)

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

function produce(val) end
function (instr::Instruction{typeof(produce)})()
    args = val(instr.input[1])
    tape = gettape(instr)
    tf = tape.owner
    ttask = tf.owner
    ch = ttask.channel
    put!(ch, args)
end

function consume(ttask::TapedTask)
    schedule(ttask)
    val = take!(ttask.channel)
    yield()
    isa(val, TapedTaskException) && throw(val.exc)
    return val
end

function Base.schedule(t::TapedTask)
    istaskstarted(t.task) && return
    Base.schedule(t.task)
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
    t.counter[] <= 1 && error("Can't copy a TapedTask which is not running.")
    tf = copy(t.tf)
    new_t = TapedTask(tf)
    new_t.counter[] = t.counter[]
    return new_t
end
