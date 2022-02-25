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
        args_type = _accurate_typeof.(args)
        cache_key = (f, args_type...)

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
            ir = code_inferred(tf.func, args_type...)
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
Box(x) = Box(gensym(), x)
Box{T}(x) where {T} = Box{T}(gensym(), x)
Base.show(io::IO, box::Box) = print(io, "Box(", box.id, ")")

## methods for RawTape and Taped
MacroTools.@forward TapedFunction.tape Base.iterate, Base.length
MacroTools.@forward TapedFunction.tape Base.push!, Base.getindex, Base.lastindex

result(t::TapedFunction) = t.retval

function (tf::TapedFunction)(args...; callback=nothing)
    # run the raw tape
    if(tf.counter <= 1 && length(args) > 0)
        input = map(Box{Any}, args)
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

function Base.show(io::IO, rtape::RawTape)
    # we use an extra IOBuffer to collect all the data and then
    # output it once to avoid output interrupt during task context
    # switching
    buf = IOBuffer()
    print(buf, "$(length(rtape))-element RawTape")
    isempty(rtape) || println(buf, ":")
    i = 1
    for instr in rtape
        print(buf, "\t$i => ")
        show(buf, instr)
        i += 1
    end
    print(io, String(take!(buf)))
end

## methods for Instruction
Base.show(io::IO, instr::AbstractInstruction) = println(io, "A ", typeof(instr))

function Base.show(io::IO, instr::Instruction)
    func = instr.func
    tape = instr.tape
    println(io, "Instruction($(instr.output)=$(func)$(instr.input), tape=$(objectid(tape)))")
end

function Base.show(io::IO, instr::GotoInstruction)
    tape = instr.tape
    println(io, "GotoInstruction($(instr.condition), dest=$(instr.dest), tape=$(objectid(tape)))")
end

function (instr::Instruction{F})() where F
    # catch run-time exceptions / errors.
    try
        func = val(instr.func)
        output = func(map(val, instr.input)...)
        instr.output.val = output
        instr.tape.counter += 1
    catch e
        println("counter=", instr.tape.counter)
        println("tape=", instr.tape)
        println(e, catch_backtrace());
        rethrow(e);
    end
end

function (instr::GotoInstruction)()
    if !val(instr.condition) # unless
        instr.tape.counter = instr.dest
    else
        instr.tape.counter += 1
    end
end

function (instr::ReturnInstruction)()
    instr.tape.retval = val(instr.arg)
end


## internal functions

_accurate_typeof(v) = typeof(v)
_accurate_typeof(v::Type) = Type{v}

"""
    __new__(T, args...)

Return a new instance of `T` with `args` even when there is no inner constructor for these args.
Source: https://discourse.julialang.org/t/create-a-struct-with-uninitialized-fields/6967/5
"""
@generated function __new__(T, args...)
    return Expr(:splatnew, :T, :args)
end


## Translation: CodeInfo -> Tape

arg_boxer(var, boxes::Dict{Symbol, Box{<:Any}}) = var
arg_boxer(var::Core.SSAValue, boxes::Dict{Symbol, Box{<:Any}}) = arg_boxer(Symbol(var.id), boxes)
arg_boxer(var::Core.TypedSlot, boxes::Dict{Symbol, Box{<:Any}}) =
    arg_boxer(Symbol("_$(var.id)"), boxes)
arg_boxer(var::Core.SlotNumber, boxes::Dict{Symbol, Box{<:Any}}) =
    arg_boxer(Symbol("_$(var.id)"), boxes)
arg_boxer(var::Symbol, boxes::Dict{Symbol, Box{<:Any}}) =
    get!(boxes, var, Box{Any}(var, nothing))

function _find_slot(all_boxes::Dict{Symbol, Box{<:Any}}, slot::Int)
    box_id = Symbol("_$(slot)")
    haskey(all_boxes, box_id) && return all_boxes[box_id]
    return Box{Any}(nothing) # func or argument is not used
end

function args_initializer(taped::Taped, all_boxes::Dict{Symbol, Box{<:Any}})
    funcbox = _find_slot(all_boxes, 1)
    argsbox = [_find_slot(all_boxes, i + 1) for i in 1:taped.arity]
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
    boxes = Dict{Symbol, Box{<:Any}}()
    _box = (x) -> arg_boxer(x, boxes)
    goto_offset = 1
    mi = ir.parent

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const

        if isa(line, Core.NewvarNode)
            ins = GotoInstruction(Box{Any}(true), 0, taped) # a noop
            push!(tape, ins)
        elseif isa(line, GlobalRef)
            ins = Instruction(val, (line,), _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif isa(line, Core.SlotNumber)
            ins = Instruction(identity, (_box(line),),
                              _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif isa(line, Core.TypedSlot)
            ins = Instruction(identity, (_box(Core.SlotNumber(line.id)),),
                              _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif isa(line, Core.GotoIfNot)
            cond = _box(line.cond)
            isa(cond, Bool) && (cond = Box{Any}(cond)) # unify the condiftion type
            ins = GotoInstruction(cond, line.dest + goto_offset, taped)
            push!(tape, ins)
        elseif isa(line, Core.GotoNode)
            cond = Box{Any}(false) # unify the condiftion type
            ins = GotoInstruction(cond, line.label + goto_offset, taped)
            push!(tape, ins)
        elseif isa(line, Core.ReturnNode)
            ins = ReturnInstruction(_box(line.val), taped)
            push!(tape, ins)
        elseif Meta.isexpr(line, :new)
            args = map(_box, line.args)
            ins = Instruction(__new__, args |> Tuple, _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif Meta.isexpr(line, :call)
            args = map(_box, line.args)
            # args[1] is the function
            func = args[1]
            if Meta.isexpr(func, :static_parameter) # func is a type parameter
                func = mi.sparam_vals[func.args[1]]
            end
            ins = Instruction(func, args[2:end] |> Tuple, _box(Core.SSAValue(idx)), taped)
            push!(tape, ins)
        elseif Meta.isexpr(line, :(=))
            output = _box(line.args[1]) # args[1] (the left hand) is a SlotNumber
            rh = line.args[2] # the right hand
            if Meta.isexpr(rh, :new)
                args = map(_box, rh.args)
                ins = Instruction(__new__, args |> Tuple, output, taped)
                push!(tape, ins)
            elseif Meta.isexpr(rh, :call)
                args = map(_box, rh.args)
                # args[1] is the function (as a GlobalRef)
                func = args[1]
                if Meta.isexpr(func, :static_parameter) # func is a type parameter
                    func = mi.sparam_vals[func.args[1]]
                end
                ins = Instruction(func, args[2:end] |> Tuple, output, taped)
                push!(tape, ins)
            else # rh is a single value
                ins = Instruction(identity, (_box(rh),), output, taped)
                push!(tape, ins)
            end
        else
            @error "Unknown IR code: " typeof(line) line
        end
    end

    init_ins = Instruction(
        args_initializer(taped, boxes),
        tuple((Box{Any}(nothing) for _ in 1:taped.arity)...),
        Box{Any}(nothing), taped)
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

function copy_box(old_box::Box{T}, roster::Dict{Symbol, Box{<:Any}}) where T
    oid = old_box.id
    haskey(roster, oid) && (return roster[oid])
    new_box = Box{T}(oid, tape_copy(old_box.val))
    roster[oid] = new_box
    return new_box
end
copy_box(o, roster::Dict{Symbol, Box{<:Any}}) = o

function Base.copy(x::Instruction, on_tape::Taped, roster::Dict{Symbol, Box{<:Any}})
    # func may also be a boxed variable
    func = copy_box(x.func, roster)
    input = map(x.input) do ob
        copy_box(ob, roster)
    end
    output = copy_box(x.output, roster)
    Instruction(func, input, output, on_tape)
end

function Base.copy(x::GotoInstruction, on_tape::Taped, roster::Dict{Symbol, Box{<:Any}})
    cond = copy_box(x.condition, roster)
    GotoInstruction(cond, x.dest, on_tape)
end

function Base.copy(x::ReturnInstruction, on_tape::Taped, roster::Dict{Symbol, Box{<:Any}})
    arg = copy_box(x.arg, roster)
    ReturnInstruction(arg, on_tape)
end

function Base.copy(old_tape::RawTape, on_tape::Taped, roster::Dict{Symbol, Box{<:Any}})
    new_tape = RawTape(undef, length(old_tape))
    for (i, x) in enumerate(old_tape)
        i == 1 && continue
        new_ins = copy(x, on_tape, roster)
        new_tape[i] = new_ins
    end

    init_ins = Instruction(
        args_initializer(on_tape, roster),
        tuple((Box{Any}(nothing) for _ in 1:on_tape.arity)...),
        Box{Any}(nothing), on_tape)
    new_tape[1] = init_ins

    return new_tape
end

function Base.copy(tf::TapedFunction)
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf.func; cache=false, init=false)
    new_tf.ir = tf.ir
    new_tf.arity = tf.arity
    roster = Dict{Symbol, Box{<:Any}}()
    new_tape = copy(tf.tape, new_tf, roster)
    new_tf.tape = new_tape
    new_tf.counter = tf.counter
    return new_tf
end
