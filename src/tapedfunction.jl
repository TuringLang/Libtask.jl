## Instruction and TapedFunction

abstract type AbstractInstruction end
const RawTape = Vector{AbstractInstruction}

mutable struct TapedFunction{F, TapeType}
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::Core.CodeInfo
    tape::TapeType
    counter::Int
    bindings::Dict{Symbol, Any}
    retval::Symbol

    function TapedFunction{F, T}(f::F, args...; cache=false) where {F, T}
        args_type = _accurate_typeof.(args)
        cache_key = (f, args_type...)

        if cache && haskey(TRCache, cache_key) # use cache
            cached_tf = TRCache[cache_key]::TapedFunction{F, T}
            tf = copy(cached_tf)
            tf.counter = 1
            return tf
        end

        ir = CodeInfoTools.code_inferred(f, args_type...)
        bindings, tape = translate!(RawTape(), ir)

        tf = new{F, T}(f, length(args), ir, tape, 1, bindings, :none)
        TRCache[cache_key] = tf # set cache
        return tf
    end

    TapedFunction(f, args...; cache=false) =
        TapedFunction{typeof(f), RawTape}(f, args...; cache=cache)

    function TapedFunction{F, T0}(tf::TapedFunction{F, T1}) where {F, T0, T1}
        new{F, T0}(tf.func, tf.arity, tf.ir, tf.tape,
               tf.counter, tf.bindings, :none)
    end

    TapedFunction(tf::TapedFunction{F, T}) where {F, T} = TapedFunction{F, T}(tf)
end

const TRCache = LRU{Tuple, TapedFunction}(maxsize=10)
const CompiledTape = Vector{FunctionWrapper{Nothing, Tuple{TapedFunction}}}

function Base.convert(::Type{CompiledTape}, tape::RawTape)
    ctape = CompiledTape(undef, length(tape))
    for idx in 1:length(tape)
        ctape[idx] = FunctionWrapper{Nothing, Tuple{TapedFunction}}(tape[idx])
    end
    return ctape
end

compile(tf::TapedFunction{F, RawTape}) where {F} = TapedFunction{F, CompiledTape}(tf)

# const TypedFunction = FunctionWrapper
struct TypedFunction{OT, IT<:Tuple}
    func::Function
    retval::Base.RefValue{OT}
    TypedFunction{OT, IT}(f::Function) where {OT, IT<:Tuple} = new{OT, IT}(f, Ref{OT}())
end

function (f::TypedFunction{OT, IT})(args...) where {OT, IT<:Tuple}
    output = f.func(args...)
    f.retval[] = OT === Nothing ? nothing : output
    return f.retval[]
end

@inline _lookup(tf::TapedFunction, v) = v
@inline _lookup(tf::TapedFunction, v::Symbol) = tf.bindings[v]
@inline _update_var!(tf::TapedFunction, v::Symbol, c) = tf.bindings[v] = c

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


@inline val(x) = x
@inline val(x::GlobalRef) = getproperty(x.mod, x.name)
@inline val(x::QuoteNode) = eval(x)
@inline val(x::TapedFunction) = x.func
@inline result(t::TapedFunction) = t.bindings[t.retval]

function (tf::TapedFunction)(args...; callback=nothing, continuation=false)
    if !continuation # reset counter and retval to run from the start
        tf.counter = 1;
        tf.retval = :none;
    end

    # set args
    if tf.counter <= 1
        haskey(tf.bindings, :_1) && _update_var!(tf, :_1, tf.func)
        for i in 1:length(args)
            slot = Symbol("_", i + 1)
            haskey(tf.bindings, slot) && _update_var!(tf, slot, args[i])
        end
    end

    # run the raw tape
    while true
        ins = tf.tape[tf.counter]
        ins(tf)
        callback !== nothing && callback()
        tf.retval !== :none && break
    end
    return result(tf)
end

function Base.show(io::IO, tf::TapedFunction)
    # we use an extra IOBuffer to collect all the data and then
    # output it once to avoid output interrupt during task context
    # switching
    buf = IOBuffer()
    println(buf, "TapedFunction:")
    println(buf, "* .func => $(tf.func)")
    println(buf, "* .ir   =>")
    println(buf, "------------------")
    println(buf, tf.ir)
    println(buf, "------------------")
    print(io, String(take!(buf)))
end

function Base.show(io::IO, rtape::RawTape)
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

function (instr::Instruction{F})(tf::TapedFunction) where F
    # catch run-time exceptions / errors.
    try
        func = val(_lookup(tf, instr.func))
        inputs = map(x -> val(_lookup(tf, x)), instr.input)
        output = func(inputs...)
        _update_var!(tf, instr.output, output)
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
_loose_type(t) = t
_loose_type(::Type{Type{T}}) where T = isa(T, DataType) ? Type{T} : typeof(T)

"""
    __new__(T, args...)

Return a new instance of `T` with `args` even when there is no inner constructor for these args.
Source: https://discourse.julialang.org/t/create-a-struct-with-uninitialized-fields/6967/5
"""
@generated function __new__(T, args...)
    return Expr(:splatnew, :T, :args)
end


## Translation: CodeInfo -> Tape

function bind_var!(var, bindings::Dict{Symbol, Any}, ir::Core.CodeInfo) # for literal constants
    id = gensym()
    bindings[id] = var
    return id
end
bind_var!(var::Core.SSAValue, bindings::Dict{Symbol, Any}, ir::Core.CodeInfo) =
    bind_var!(Symbol(var.id), bindings, ir.ssavaluetypes[var.id])
bind_var!(var::Core.TypedSlot, bindings::Dict{Symbol, Any}, ir::Core.CodeInfo) =
    bind_var!(Symbol(:_, var.id), bindings, ir.slottypes[var.id])
bind_var!(var::Core.SlotNumber, bindings::Dict{Symbol, Any}, ir::Core.CodeInfo) =
    bind_var!(Symbol(:_, var.id), bindings, ir.slottypes[var.id])
bind_var!(var::Symbol, boxes::Dict{Symbol, Any}, c::Core.Const) =
    bind_var!(var, boxes, _loose_type(Type{c.val}))
bind_var!(var::Symbol, boxes::Dict{Symbol, Any}, c::Core.PartialStruct) =
    bind_var!(var, boxes, _loose_type(c.typ))
function bind_var!(var::Symbol, bindings::Dict{Symbol, Any}, ::Type{T}) where T
    get!(bindings, var, nothing)
    return var
end

function translate!(tape::RawTape, ir::Core.CodeInfo)
    bindings = Dict{Symbol, Any}()

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const
        ins = translate!!(Core.SSAValue(idx), line, bindings, ir)
        push!(tape, ins)
    end
    return (bindings, tape)
end

const IRVar = Union{Core.SSAValue, Core.SlotNumber}

function translate!!(var::IRVar, line::Core.NewvarNode,
                     bindings::Dict{Symbol, Any}, @nospecialize(ir))
    # use a noop to ensure the 1-to-1 mapping from ir.code to instructions
    # on tape. see GotoInstruction.dest.
    return GotoInstruction(:_true, 0)
end

function translate!!(var::IRVar, line::GlobalRef,
                     bindings::Dict{Symbol, Any}, ir)
    return Instruction(() -> val(line), (), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.SlotNumber,
                     bindings::Dict{Symbol, Any}, ir)
    return Instruction(identity, (bind_var!(line, bindings, ir),), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.TypedSlot,
                     bindings::Dict{Symbol, Any}, ir)
    input = bind_var!(Core.SlotNumber(line.id), bindings, ir)
    return Instruction(identity, (input,), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.GotoIfNot,
                     bindings::Dict{Symbol, Any}, ir)
    _cond = bind_var!(line.cond, bindings, ir)
    cond = if isa(_cond, Bool)
        _cond ? :_true : :_false
    else
        _cond
    end
    return GotoInstruction(cond, line.dest)
end

function translate!!(var::IRVar, line::Core.GotoNode,
                     bindings::Dict{Symbol, Any}, @nospecialize(ir))
    return GotoInstruction(:_false, line.label)
end

function translate!!(var::IRVar, line::Core.ReturnNode,
                     bindings::Dict{Symbol, Any}, ir)
    return ReturnInstruction(bind_var!(line.val, bindings, ir))
end

function translate!!(var::IRVar, line::Expr,
                     bindings::Dict{Symbol, Any}, ir::Core.CodeInfo)
    head = line.head
    _bind_fn = (x) -> bind_var!(x, bindings, ir)
    if head === :new
        args = map(_bind_fn, line.args)
        return Instruction(__new__, args |> Tuple, _bind_fn(var))
    elseif head === :call
        args = map(_bind_fn, line.args)
        # args[1] is the function
        func = line.args[1]
        if Meta.isexpr(func, :static_parameter) # func is a type parameter
            func = ir.parent.sparam_vals[func.args[1]]
        else # isa(func, GlobalRef) or a var?
            func = args[1] # a var(box)
        end
        return Instruction(func, args[2:end] |> Tuple, _bind_fn(var))
    elseif head === :(=)
        # line.args[1] (the left hand side) is a SlotNumber, and it should be the output
        lhs = line.args[1]
        rhs = line.args[2] # the right hand side, maybe a Expr, or a var, or ...
        if Meta.isexpr(rhs, (:new, :call))
            return translate!!(lhs, rhs, bindings, ir)
        else # rhs is a single value
            return Instruction(identity, (_bind_fn(rhs),), _bind_fn(lhs))
        end
    else
        @error "Unknown Expression: " typeof(var) var typeof(line) line
        throw(ErrorException("Unknown Expression"))
    end
end

function translate!!(var, line, bindings, ir)
    @error "Unknown IR code: " typeof(var) var typeof(line) line
    throw(ErrorException("Unknown IR code"))
end

## copy Bindings, TapedFunction

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

function copy_bindings(old::Dict{Symbol, Any})
    newb = Dict{Symbol, Any}()
    for (k, v) in old
        newb[k] = tape_copy(v)
    end
    return newb
end

function Base.copy(tf::TapedFunction)
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf)
    new_tf.bindings = copy_bindings(tf.bindings)
    return new_tf
end
