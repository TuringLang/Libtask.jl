mutable struct Box{T}
    id::Symbol
    val::T
end

abstract type AbstractInstruction end
abstract type Taped end
const RawTape = Vector{AbstractInstruction}

"""
    Instruction

An `Instruction` stands for a function call
"""
mutable struct Instruction{F, TI<:Tuple, TO, T<:Taped} <: AbstractInstruction
    func::F
    input::TI
    output::Box{TO}
    tape::T
end

mutable struct GotoInstruction{T<:Taped} <: AbstractInstruction
    condition::Box{Any}
    dest::Int
    tape::T
end

mutable struct ReturnInstruction{TA, T<:Taped} <: AbstractInstruction
    arg::TA
    tape::T
end

mutable struct TapedFunction{F} <: Taped
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::Core.CodeInfo
    tape::RawTape
    counter::Int
    retval
    owner

    function TapedFunction(f::F, args...; cache=false, init=true) where {F}
        cache_key = (f, typeof.(args)...)

        if cache && haskey(TRCache, cache_key) # use cache
            cached_tf = TRCache[cache_key]::TapedFunction{F}
            tf = copy(cached_tf)
            tf.counter = 1
            return tf
        end

        tf = new{F}() # leave some fields to be undef
        tf.func = f
        tf.tape = RawTape()
        tf.counter = 1

        if init # init
            tf.arity = length(args)
            ir = code_inferred(tf.func, typeof.(args)...)
            tf.ir = ir
            translate!(tf, ir)
            # set cache
            TRCache[cache_key] = tf
        end
        return tf
    end
end

const TRCache = LRU{Tuple, TapedFunction}(maxsize=10)

## methods for Box
val(x) = x
val(x::Box) = x.val
val(x::GlobalRef) = getproperty(x.mod, x.name)
val(x::QuoteNode) = eval(x)
val(x::TapedFunction) = x.func
box(x::Box) = x
box(x) = Box(gensym(), x)
box(id::Symbol, x) = Box(id, x)
box_any(x) = Box{Any}(gensym(), x)
box_any(id::Symbol, x) = Box{Any}(id, x)
Base.show(io::IO, box::Box) = print(io, "Box(", box.id, ", ", box.val, ")")

## methods for RawTape and Taped
MacroTools.@forward TapedFunction.tape Base.iterate, Base.length
MacroTools.@forward TapedFunction.tape Base.push!, Base.getindex, Base.lastindex

result(t::TapedFunction) = t.retval

function (tf::TapedFunction)(args...; callback=nothing)
    # run the raw tape
    if(tf.counter <= 1 && length(args) > 0)
        input = map(box_any, args)
        tf.tape[1].input = input
    end

    while true
        ins = tf[tf.counter]
        ins()
        callback !== nothing && callback()
        isa(ins, ReturnInstruction) && break
    end
    return result(tf)
end

function Base.show(io::IO, tf::TapedFunction)
    buf = IOBuffer()
    println(buf, "TapedFunction:")
    println(buf, "* .func => $(tf.func)")
    println(buf, "* .ir   =>")
    println(buf, "------------------")
    println(buf, tf.ir)
    println(buf, "------------------")
    println(buf, "* .tape =>")
    println(buf, "------------------")
    println(buf, tf.tape)
    println(buf, "------------------")
    print(io, String(take!(buf)))
end

function Base.show(io::IO, tp::RawTape)
    # we use an extra IOBuffer to collect all the data and then
    # output it once to avoid output interrupt during task context
    # switching
    buf = IOBuffer()
    print(buf, "$(length(tp))-element RawTape")
    isempty(tp) || println(buf, ":")
    i = 1
    for instruction in tp
        print(buf, "\t$i => ")
        show(buf, instruction)
        i += 1
    end
    print(io, String(take!(buf)))
end

## methods for Instruction
Base.show(io::IO, instruction::AbstractInstruction) = println(io, "A ", typeof(instruction))

function Base.show(io::IO, instruction::Instruction)
    func = instruction.func
    tape = instruction.tape
    println(io, "Instruction($(func)$(map(val, instruction.input)), tape=$(objectid(tape)))")
end

function Base.show(io::IO, instruction::GotoInstruction)
    tape = instruction.tape
    println(io, "GotoInstruction($(val(instruction.condition)), tape=$(objectid(tape)))")
end

function (instr::Instruction{F})() where F
    # catch run-time exceptions / errors.
    try
        func = val(instr.func)
        output = func(map(val, instr.input)...)
        instr.output.val = output
        instr.tape.counter += 1
    catch e
        println(e, catch_backtrace());
        rethrow(e);
    end
end

"""
    __new__(T, args)

Return a new instance of `T` with `args` even when there is no inner constructor for these args.
Source: https://discourse.julialang.org/t/create-a-struct-with-uninitialized-fields/6967/5
"""
@generated function __new__(T, args)
    return Expr(:splatnew, :T, :args)
end

function (instr::Instruction{typeof(__new__)})()
    # catch run-time exceptions / errors.
    try
        input = map(val, instr.input)
        T = input[1]
        args = input[2:end]
        output = __new__(T, args)

        instr.output.val = output
        instr.tape.counter += 1
    catch e
        println(e, catch_backtrace());
        rethrow(e);
    end
end

function (instr::GotoInstruction)()
    if instr.condition === nothing || !val(instr.condition) # unless
        target = instr.dest
        instr.tape.counter = target
    else
        instr.tape.counter += 1
    end
end

function (instr::ReturnInstruction)()
    instr.tape.retval = val(instr.arg)
end


## internal functions

arg_boxer(var, boxes::Dict{Symbol, Box{Any}}) = var
arg_boxer(var::Core.SSAValue, boxes::Dict{Symbol, Box{Any}}) = arg_boxer(Symbol(var.id), boxes)
arg_boxer(var::Core.SlotNumber, boxes::Dict{Symbol, Box{Any}}) =
    arg_boxer(Symbol("_$(var.id)"), boxes)
arg_boxer(var::Symbol, boxes::Dict{Symbol, Box{Any}}) =
    get!(boxes, var, box_any(var, nothing))

function _find_box(tape::RawTape, slot::Int)
    box_id = Symbol("_$(slot)")
    for ins in tape
        isa(ins, Instruction) || continue
        for b in ins.input
            isa(b, Box) && b.id === box_id && return b
        end
        ins.output.id === box_id && return ins.output
    end
    slot == 1 && return box_any(nothing) # func is not used
    @error "None box found for slot" slot
end
function args_initializer(taped::Taped)
    funcbox = _find_box(taped.tape, 1)
    argsbox = [_find_box(taped.tape, i + 1) for i in 1:taped.arity]
    return (args...) -> begin
        funcbox.val = taped.func
        @assert length(args) == taped.arity
        for i in 1:length(args)
            argsbox[i].val = val(args[i]) # fill the boxes
        end
    end
end

function translate!(taped::Taped, ir::Core.CodeInfo)
    tape = taped.tape
    boxes = Dict{Symbol, Box{Any}}()
    _box = (x) -> arg_boxer(x, boxes)

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const

        if isa(line, Core.NewvarNode)
            # _box(line.slot) # we should deal its type later
        elseif isa(line, Core.TypedSlot)
            # TODO: type
            ins = Instruction(identity, (_box(Core.SlotNumber(line.id)),),
                              _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif isa(line, Core.GotoIfNot)
            cond = _box(line.cond)
            isa(cond, Bool) && (cond = box_any(cond)) # unify the condiftion type
            ins = GotoInstruction(cond, line.dest + 1, taped)
            push!(tape, ins)
        elseif isa(line, Core.GotoNode)
            cond = box_any(false) # unify the condiftion type
            ins = GotoInstruction(cond, line.label + 1, taped)
            push!(tape, ins)
        elseif isa(line, Core.ReturnNode)
            ins = ReturnInstruction(_box(line.val), taped)
            push!(tape, ins)
        elseif Meta.isexpr(line, :new)
            args = map(_box, line.args)
            ins = Instruction(__new__, args |> Tuple, _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif isa(line, GlobalRef)
            ins = Instruction(val, (line,), _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif Meta.isexpr(line, :(=))
            output = _box(line.args[1]) # args[1] is a SlotNumber
            funcall = line.args[2]
            if Meta.isexpr(funcall, :call)
                args = map(_box, funcall.args)
                # args[1] is the function (as a GlobalRef)
                f = args[1]
                ins = Instruction(f, args[2:end] |> Tuple,
                                  output, taped)
                push!(tape, ins)
            else # a literal const?
                ins = Instruction(identity, line.args[2:end] |> Tuple,
                                  output, taped)
                push!(tape, ins)
                # @error "Unknown IR code in assignment: " line
            end
        elseif Meta.isexpr(line, :call)
            args = map(_box, line.args)
            # args[1] is the function
            f = args[1]
            ins = Instruction(f, args[2:end] |> Tuple,
                              _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        else
            @error "Unknown IR code: " typeof(line) line
        end
    end

    init_ins = Instruction(
        args_initializer(taped),
        tuple((box_any(nothing) for _ in 1:taped.arity)...),
        box_any(nothing), taped)
    insert!(tape, 1, init_ins)
end

## copy Box, RawTape, and TapedFunction

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
# Core.Box is used as closure captured variable container, so we should tape_copy its contents
tape_copy(x::Core.Box) = Core.Box(tape_copy(x.contents))
# ?? should we deepcopy Array and Dict by default?
# tape_copy(x::Array) = deepcopy(x)
# tape_copy(x::Dict) = deepcopy(x)

function copy_box(old_box::Box{T}, roster::Dict{UInt64, Any}) where T
    oid = objectid(old_box)
    haskey(roster, oid) && (return roster[oid])

    # We don't know the type of box.val now, so we use Box{Any}
    new_box = Box{T}(old_box.id, tape_copy(old_box.val))
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

function Base.copy(x::GotoInstruction, on_tape::Taped, roster::Dict{UInt64, Any})
    cond = copy_box(x.condition, roster)
    GotoInstruction(cond, x.dest, on_tape)
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

    init_ins = Instruction(
        args_initializer(on_tape),
        tuple((box_any(nothing) for _ in 1:on_tape.arity)...),
        box_any(nothing), on_tape)
    new_tape[1] = init_ins
    return new_tape
end

function Base.copy(tf::TapedFunction)
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf.func; cache=false, init=false)
    new_tf.ir = tf.ir
    new_tf.arity = tf.arity
    roster = Dict{UInt64, Any}()
    new_tape = copy(tf.tape, new_tf, roster)
    new_tf.tape = new_tape
    new_tf.counter = tf.counter
    return new_tf
end
