mutable struct Box{T}
    id::Symbol
    val::T
end

## methods for Box
Box(x) = Box(gensym(), x)
Box{T}(x) where {T} = Box{T}(gensym(), x)
Base.show(io::IO, box::Box) = print(io, "Box(", box.id, ")")

## Instruction and TapedFunction

abstract type AbstractInstruction end
const RawTape = Vector{AbstractInstruction}

"""
    Instruction

An `Instruction` stands for a function call
"""
struct Instruction{F, N} <: AbstractInstruction
    func::F
    input::NTuple{N, Symbol}
    output::Symbol
end

struct GotoInstruction <: AbstractInstruction
    condition::Symbol
    # we enusre a 1-to-1 mapping between ir.code and instruction
    # so here we can use the index directly.
    dest::Int
end

struct ReturnInstruction <: AbstractInstruction
    arg::Symbol
end

mutable struct TapedFunction{F}
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::Core.CodeInfo
    tape::RawTape
    counter::Int
    bindings::Dict{Symbol, Box{<:Any}}
    retval::Symbol
    owner

    function TapedFunction(f::F, args...; cache=false) where {F}
        args_type = _accurate_typeof.(args)
        cache_key = (f, args_type...)

        if cache && haskey(TRCache, cache_key) # use cache
            cached_tf = TRCache[cache_key]::TapedFunction{F}
            tf = copy(cached_tf)
            tf.counter = 1
            return tf
        end

        ir = CodeInfoTools.code_inferred(f, args_type...)
        tf = new{F}() # leave some fields to be undef
        tf.func, tf.arity, tf.ir = f, length(args), ir
        tf.tape = RawTape()
        tf.counter = 1

        translate!(tf, ir)
        TRCache[cache_key] = tf # set cache
        return tf
    end

    function TapedFunction(tf::TapedFunction{F}) where {F}
        new{F}(tf.func, tf.arity, tf.ir, tf.tape,
               tf.counter, tf.bindings, :none, tf.owner)
    end
end

const TRCache = LRU{Tuple, TapedFunction}(maxsize=10)

val(x) = x
val(x::Box) = x.val
val(x::Box{GlobalRef}) = val(x.val)
val(x::Box{QuoteNode}) = val(x.val)
val(x::GlobalRef) = getproperty(x.mod, x.name)
val(x::QuoteNode) = eval(x)
val(x::TapedFunction) = x.func
result(t::TapedFunction) = val(t.bindings[t.retval])

function (tf::TapedFunction)(args...; callback=nothing)
    # set args
    if tf.counter <= 1
        haskey(tf.bindings, :_1) && (tf.bindings[:_1].val = tf.func)
        for i in 1:length(args)
            slot = Symbol("_", i + 1)
            haskey(tf.bindings, slot) && (tf.bindings[slot].val = args[i])
        end
    end

    # run the raw tape
    while true
        ins = tf.tape[tf.counter]
        ins(tf)
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
    print(buf, length(rtape), "-element RawTape")
    isempty(rtape) || println(buf, ":")
    i = 1
    for instr in rtape
        print(buf, "\t", i, " => ")
        show(buf, instr)
        i += 1
    end
    print(io, String(take!(buf)))
end

## methods for Instruction
Base.show(io::IO, instr::AbstractInstruction) = println(io, "A ", typeof(instr))

function Base.show(io::IO, instr::Instruction)
    println(io, "Instruction(", instr.output, "=", instr.func, instr.input)
end

function Base.show(io::IO, instr::GotoInstruction)
    println(io, "GotoInstruction(", instr.condition, ", dest=", instr.dest, ")")
end


_lookup(tf::TapedFunction, v) = v
_lookup(tf::TapedFunction, v::Symbol) = tf.bindings[v]

function (instr::Instruction{F})(tf::TapedFunction) where F
    # catch run-time exceptions / errors.
    try
        func = val(_lookup(tf, instr.func))
        inputs = map(x -> val(_lookup(tf, x)), instr.input)
        output = func(inputs...)
        output_box = _lookup(tf, instr.output)
        output_box.val = output
        tf.counter += 1
    catch e
        println("counter=", tf.counter)
        println("tf=", tf)
        println(e, catch_backtrace());
        rethrow(e);
    end
end

function (instr::GotoInstruction)(tf::TapedFunction)
    cond = instr.condition === :_true ? true :
        instr.condition === :_false ? false :
        val(_lookup(tf, instr.condition))

    if cond
        tf.counter += 1
    else # goto dest unless cond
        tf.counter = instr.dest
    end
end

function (instr::ReturnInstruction)(tf::TapedFunction)
    tf.retval = instr.arg
end


## internal functions

_accurate_typeof(v) = typeof(v)
_accurate_typeof(::Type{V}) where V = Type{V}

"""
    __new__(T, args...)

Return a new instance of `T` with `args` even when there is no inner constructor for these args.
Source: https://discourse.julialang.org/t/create-a-struct-with-uninitialized-fields/6967/5
"""
@generated function __new__(T, args...)
    return Expr(:splatnew, :T, :args)
end


## Translation: CodeInfo -> Tape

function var_boxer(var, boxes::Dict{Symbol, Box{<:Any}}) # for literal constants
    box = Box(var)
    boxes[box.id] = box
    return box.id
end
var_boxer(var::Core.SSAValue, boxes::Dict{Symbol, Box{<:Any}}) = var_boxer(Symbol(var.id), boxes)
var_boxer(var::Core.TypedSlot, boxes::Dict{Symbol, Box{<:Any}}) =
    var_boxer(Symbol(:_, var.id), boxes)
var_boxer(var::Core.SlotNumber, boxes::Dict{Symbol, Box{<:Any}}) =
    var_boxer(Symbol(:_, var.id), boxes)
var_boxer(var::Symbol, boxes::Dict{Symbol, Box{<:Any}}) =
    get!(boxes, var, Box{Any}(var, nothing)).id

function translate!(tf::TapedFunction, ir::Core.CodeInfo)
    tape = tf.tape
    boxes = Dict{Symbol, Box{<:Any}}()

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const
        ins = translate!!(Core.SSAValue(idx), line, boxes, ir)
        push!(tape, ins)
    end

    tf.bindings = boxes
end

const IRVar = Union{Core.SSAValue, Core.SlotNumber}

function translate!!(var::IRVar, line::Core.NewvarNode,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    # use a noop to ensure the 1-to-1 mapping from ir.code to instructions
    # on tape. see GotoInstruction.dest.
    return GotoInstruction(:_true, 0)
end

function translate!!(var::IRVar, line::GlobalRef,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    return Instruction(() -> val(line), (), var_boxer(var, boxes))
end

function translate!!(var::IRVar, line::Core.SlotNumber,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    return Instruction(identity, (var_boxer(line, boxes),), var_boxer(var, boxes))
end

function translate!!(var::IRVar, line::Core.TypedSlot,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    input_box = var_boxer(Core.SlotNumber(line.id), boxes)
    return Instruction(identity, (input_box,), var_boxer(var, boxes))
end

function translate!!(var::IRVar, line::Core.GotoIfNot,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    _cond = var_boxer(line.cond, boxes)
    cond = if isa(_cond, Bool)
        _cond ? :_true : :_false
    else
        _cond
    end
    return GotoInstruction(cond, line.dest)
end

function translate!!(var::IRVar, line::Core.GotoNode,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    return GotoInstruction(:_false, line.label)
end

function translate!!(var::IRVar, line::Core.ReturnNode,
                     boxes::Dict{Symbol, Box{<:Any}}, @nospecialize(ir))
    return ReturnInstruction(var_boxer(line.val, boxes))
end

function translate!!(var::IRVar, line::Expr,
                     boxes::Dict{Symbol, Box{<:Any}}, ir::Core.CodeInfo)
    head = line.head
    _box_fn = (x) -> var_boxer(x, boxes)
    if head === :new
        args = map(_box_fn, line.args)
        return Instruction(__new__, args |> Tuple, var_boxer(var, boxes))
    elseif head === :call
        args = map(_box_fn, line.args)
        # args[1] is the function
        func = line.args[1]
        if Meta.isexpr(func, :static_parameter) # func is a type parameter
            func = ir.parent.sparam_vals[func.args[1]]
        else # isa(func, GlobalRef) or a var?
            func = args[1] # a var(box)
        end
        return Instruction(func, args[2:end] |> Tuple, var_boxer(var, boxes))
    elseif head === :(=)
        # line.args[1] (the left hand side) is a SlotNumber, and it should be the output
        lhs = line.args[1]
        rhs = line.args[2] # the right hand side, maybe a Expr, or a var, or ...
        if Meta.isexpr(rhs, (:new, :call))
            return translate!!(lhs, rhs, boxes, ir)
        else # rhs is a single value
            return Instruction(identity, (_box_fn(rhs),), _box_fn(lhs))
        end
    else
        @error "Unknown Expression: " typeof(var) var typeof(line) line
        throw(ErrorException("Unknown Expression"))
    end
end

function translate!!(var, line, boxes, ir)
    @error "Unknown IR code: " typeof(var) var typeof(line) line
    throw(ErrorException("Unknown IR code"))
end

## copy Box, TapedFunction

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

Base.copy(box::Box{T}) where T =  Box{T}(box.id, tape_copy(box.val))

function copy_bindings(old::Dict{Symbol, Box{<:Any}})
    newb = Dict{Symbol, Box{<:Any}}()
    for (k, v) in old
        newb[k] = copy(v)
    end
    return newb
end

function Base.copy(tf::TapedFunction)
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf)
    new_tf.bindings = copy_bindings(tf.bindings)
    return new_tf
end
