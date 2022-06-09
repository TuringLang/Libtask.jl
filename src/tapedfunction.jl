#=

In this file, we convert a Julia function to a tape hence we can have
some power on controlling how to run the function, like caching data,
stopping or do some checks between expressions, etc.

To do so, we firstly use the Julia builtin compiler to get the IR code
of the function. Here we use the unoptimized typed code, which is in a
non-strict SSA form. Then we convert each IR instruction to a Julia
representation (an object of a subtype of AbstractInstruction). All
the operands (i.e., the varibales) these instructions use are stored
in a Vector{Any} called bindings. We called this conversion process
compile-time or to distinguish it from Julia's compile time,
tape-recording time.

There many kinds of instructions on tape:
- Instruction stands for an ordinary function call
- GotoInstruction and CondGotoInstruction are for control-flow, with these
  instructions, we can jump from an instruction to another one on the same
  tape.
- ReturnInstruction, as its name indicates, let us return from a function.

Once the tape is recorded, we can run the tape to gain the same effect
as calling the original function. We first fill the arguments into to
the bindings, then go through each instruction on the tape, stop after
we encounter a ReturnInstruction.

We provide a chance to add a callback after each time we execute an
instruction, with this facility we implemented the produce/consume
machanism in TapedTask. And based on the feautres of being copiable
and execution controlling, we made the fork machanism of TapedTask.

However, this implementation currently has some caveates:
1. GlobalRef is evaluated at tape-recording time (compile time), you will
   see an info log on each GloabalRef evaluation. At most time, we don't
   change the value/object which is associated to a GlobalRef at runtime,
   so this works well. But, if you do somthing like
   `module A v=1 end; make tapedfunction; A.eval(:(v=2)); run tf;`, The
   assignment won't work.
2. QuoteNode is also evaluated at tape-recording time (compile time) with
   an info log. Mostly the result of evaluating a QuoteNode is a Symbol, so
   this works well at most time.
3. There's one allocation in each Instruction execution, so write a function
   in a manner which has the less instruction executions will be more
   performant, for example, use broadcast instead of a loop.

=#

## Instruction and TapedFunction
abstract type AbstractInstruction end
const RawTape = Vector{AbstractInstruction}

function _infer(f, args_type)
    # `code_typed` returns a vector: [Pair{Core.CodeInfo, DataType}]
    ir0 = code_typed(f, Tuple{args_type...}, optimize=false)[1][1]
    return ir0
end

const Bindings = Vector{Any}

mutable struct TapedFunction{F, TapeType}
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::Core.CodeInfo
    tape::TapeType
    counter::Int
    bindings::Bindings
    slots::Dict{Int, Int} # slots indices in bindings
    retval::Int # 0 indicates the function has not returned

    function TapedFunction{F, T}(f::F, args...; cache=false) where {F, T}
        args_type = _accurate_typeof.(args)
        cache_key = (f, args_type...)

        if cache && haskey(TRCache, cache_key) # use cache
            cached_tf = TRCache[cache_key]::TapedFunction{F, T}
            tf = copy(cached_tf)
            tf.counter = 1
            return tf
        end
        ir = _infer(f, args_type)
        bindings, slots, tape = translate!(RawTape(), ir)

        tf = new{F, T}(f, length(args), ir, tape, 1, bindings, slots, 0)
        TRCache[cache_key] = tf # set cache
        return tf
    end

    TapedFunction(f, args...; cache=false) =
        TapedFunction{typeof(f), RawTape}(f, args...; cache=cache)

    function TapedFunction{F, T0}(tf::TapedFunction{F, T1}) where {F, T0, T1}
        new{F, T0}(tf.func, tf.arity, tf.ir, tf.tape,
                   tf.counter, tf.bindings, tf.slots, 0)
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

@inline _lookup(tf::TapedFunction, v::Int) = @inbounds tf.bindings[v]
@inline _update_var!(tf::TapedFunction, v::Int, c) = @inbounds tf.bindings[v] = c

"""
    Instruction

An `Instruction` stands for a function call
"""
struct Instruction{F, N} <: AbstractInstruction
    func::F
    input::NTuple{N, Int}
    output::Int
end

struct GotoInstruction <: AbstractInstruction
    # we enusre a 1-to-1 mapping between ir.code and instruction
    # so here we can use the index directly.
    dest::Int
end

struct CondGotoInstruction <: AbstractInstruction
    condition::Int
    dest::Int
end

struct ReturnInstruction <: AbstractInstruction
    arg::Int
end

struct NOOPInstruction <: AbstractInstruction end

@inline result(t::TapedFunction) = t.bindings[t.retval]

function (tf::TapedFunction)(args...; callback=nothing, continuation=false)
    if !continuation # reset counter and retval to run from the start
        tf.counter = 1
        tf.retval = 0
    end

    # set args
    if tf.counter <= 1
        # The first slot in `bindings` is assumed to be `tf.func`.
        haskey(tf.slots, 1) && _update_var!(tf, tf.slots[1], tf.func)
        for i in 1:length(args) # the subsequent slots are arguments
            slot = i + 1
            haskey(tf.slots, slot) && _update_var!(tf, tf.slots[slot], args[i])
        end
    end

    # run the raw tape
    while true
        ins = tf.tape[tf.counter]
        ins(tf)
        callback !== nothing && callback()
        tf.retval != 0 && break
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
    println(io, "GotoInstruction(dest=", instr.dest, ")")
end

function Base.show(io::IO, instr::CondGotoInstruction)
    println(io, "CondGotoInstruction(", instr.condition, ", dest=", instr.dest, ")")
end

function (instr::Instruction{F})(tf::TapedFunction) where F
    # catch run-time exceptions / errors.
    try
        func = F === Int ? _lookup(tf, instr.func) : instr.func
        inputs = map(x -> _lookup(tf, x), instr.input)
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
    tf.counter = instr.dest
end

function (instr::CondGotoInstruction)(tf::TapedFunction)
    cond = _lookup(tf, instr.condition)
    if cond
        tf.counter += 1
    else # goto dest unless cond
        tf.counter = instr.dest
    end
end

function (instr::ReturnInstruction)(tf::TapedFunction)
    tf.retval = instr.arg
end

function (instr::NOOPInstruction)(tf::TapedFunction)
    tf.counter += 1
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

const IRVar = Union{Core.SSAValue, Core.SlotNumber}

struct TempBindings
    data::Bindings
    book::Dict{IRVar, Int}
end

function bind_var!(var_literal, tbind::TempBindings, ir::Core.CodeInfo)
    # for literal constants
    push!(tbind.data, var_literal)
    idx = length(tbind.data)
    return idx
end
function bind_var!(var::GlobalRef, tbind::TempBindings, ir::Core.CodeInfo)
    in(var.mod, (Base, Core)) ||
        @info "evaluating GlobalRef $var at compile time"
    bind_var!(getproperty(var.mod, var.name), tbind, ir)
end
function bind_var!(var::QuoteNode, tbind::TempBindings, ir::Core.CodeInfo)
    @info "evaluating QuoteNode $var at compile time"
    bind_var!(eval(var), tbind, ir)
end
function bind_var!(var::Core.TypedSlot, tbind::TempBindings, ir::Core.CodeInfo)
    # turn TypedSlot to SlotNumber
    bind_var!(Core.SlotNumber(var.id), tbind, ir)
end
function bind_var!(var::Core.SlotNumber, tbind::TempBindings, ir::Core.CodeInfo)
    get!(tbind.book, var, allocate_binding!(var, tbind, ir.slottypes[var.id]))
end
function bind_var!(var::Core.SSAValue, tbind::TempBindings, ir::Core.CodeInfo)
    get!(tbind.book, var, allocate_binding!(var, tbind, ir.ssavaluetypes[var.id]))
end

allocate_binding!(var, tbind::TempBindings, c::Core.Const) =
    allocate_binding!(var, tbind, _loose_type(Type{c.val}))
allocate_binding!(var, tbind::TempBindings, c::Core.PartialStruct) =
    allocate_binding!(var, tbind, _loose_type(c.typ))
function allocate_binding!(var, tbind::TempBindings, ::Type{T}) where T
    # we may use the type info (T) here
    push!(tbind.data, nothing)
    idx = length(tbind.data)
    return idx
end

function translate!(tape::RawTape, ir::Core.CodeInfo)
    bindings = Bindings()
    sizehint!(bindings, 128)
    bcache = Dict{IRVar, Int}()
    tbind = TempBindings(bindings, bcache)
    slots = Dict{Int, Int}()

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const
        isconst = isa(ir.ssavaluetypes[idx], Core.Const)
        ins = translate!!(Core.SSAValue(idx), line, tbind, isconst, ir)
        push!(tape, ins)
    end
    for (k, v) in bcache
        isa(k, Core.SlotNumber) && (slots[k.id] = v)
    end
    return (bindings, slots, tape)
end

function _const_instruction(var::IRVar, v, tbind::TempBindings, ir)
    if isa(var, Core.SSAValue)
        box = bind_var!(var, tbind, ir)
        tbind.data[box] = v
        return NOOPInstruction()
    end
    return Instruction(identity, (bind_var!(v, tbind, ir),), bind_var!(var, tbind, ir))
end

function translate!!(var::IRVar, line::Core.NewvarNode,
                     tbind::TempBindings, isconst::Bool, @nospecialize(ir))
    # use a no-op to ensure the 1-to-1 mapping from ir.code to instructions on tape.
    return NOOPInstruction()
end

function translate!!(var::IRVar, line::GlobalRef,
                     tbind::TempBindings, isconst::Bool, ir)
    if isconst
        v = ir.ssavaluetypes[var.id].val
        return _const_instruction(var, v, tbind, ir)
    end
    func() = getproperty(line.mod, line.name)
    return Instruction(func, (), bind_var!(var, tbind, ir))
end

function translate!!(var::IRVar, line::Core.SlotNumber,
                     tbind::TempBindings, isconst::Bool, ir)
    if isconst
        v = ir.ssavaluetypes[var.id].val
        return _const_instruction(var, v, tbind, ir)
    end
    func = identity
    input = (bind_var!(line, tbind, ir),)
    output =  bind_var!(var, tbind, ir)
    return Instruction(func, input, output)
end

function translate!!(var::IRVar, line::Core.TypedSlot,
                     tbind::TempBindings, isconst::Bool, ir)
    input_box = bind_var!(Core.SlotNumber(line.id), tbind, ir)
    return Instruction(identity, (input_box,), bind_var!(var, tbind, ir))
end

function translate!!(var::IRVar, line::Core.GotoIfNot,
                     tbind::TempBindings, isconst::Bool, ir)
    cond = bind_var!(line.cond, tbind, ir)
    return CondGotoInstruction(cond, line.dest)
end

function translate!!(var::IRVar, line::Core.GotoNode,
                     tbind::TempBindings, isconst::Bool, @nospecialize(ir))
    return GotoInstruction(line.label)
end

function translate!!(var::IRVar, line::Core.ReturnNode,
                     tbind::TempBindings, isconst::Bool, ir)
    return ReturnInstruction(bind_var!(line.val, tbind, ir))
end

_canbeoptimized(v) = isa(v, DataType) || isprimitivetype(typeof(v))
function translate!!(var::IRVar, line::Expr,
                     tbind::TempBindings, isconst::Bool, ir::Core.CodeInfo)
    head = line.head
    _bind_fn = (x) -> bind_var!(x, tbind, ir)
    if head === :new
        args = map(_bind_fn, line.args)
        return Instruction(__new__, args |> Tuple, _bind_fn(var))
    elseif head === :call
        # Only some of the function calls can be optimized even though many of their results are
        # inferred as constants: we only optimize primitive and datatype constants for now. For
        # optimised function calls, we will evaluate the function at compile-time and cache results.
        if isconst
            v = ir.ssavaluetypes[var.id].val
            _canbeoptimized(v) && return _const_instruction(var, v, tbind, ir)
        end
        args = map(_bind_fn, line.args)
        # args[1] is the function
        func = line.args[1]
        if Meta.isexpr(func, :static_parameter) # func is a type parameter
            func = ir.parent.sparam_vals[func.args[1]]
        elseif isa(func, GlobalRef)
            func = getproperty(func.mod, func.name)  # Staging out global reference variable (constants).
        else # a var?
            func = args[1] # a var(box)
        end
        return Instruction(func, args[2:end] |> Tuple, _bind_fn(var))
    elseif head === :(=)
        # line.args[1] (the left hand side) is a SlotNumber, and it should be the output
        lhs = line.args[1]
        rhs = line.args[2] # the right hand side, maybe a Expr, or a var, or ...
        if Meta.isexpr(rhs, (:new, :call))
            return translate!!(lhs, rhs, tbind, false, ir)
        else # rhs is a single value
            if isconst
                v = ir.ssavaluetypes[var.id].val
                return Instruction(identity, (_bind_fn(v),), _bind_fn(lhs))
            end
            return Instruction(identity, (_bind_fn(rhs),), _bind_fn(lhs))
        end
    else
        @error "Unknown Expression: " typeof(var) var typeof(line) line
        throw(ErrorException("Unknown Expression"))
    end
end

function translate!!(var, line, tbind, ir)
    @error "Unknown IR code: " typeof(var) var typeof(line) line
    throw(ErrorException("Unknown IR code"))
end

## copy Bindings, TapedFunction

"""
    tape_copy(x)

Function `tape_copy` is used to copy data while copying a TapedTask, the
default behavior is: for all data types, we do not copy but share the data
between tasks, i.e., `tape_copy(x) = x`. If one wants some kinds of data'
to be copied, or deeply copied, one can add a method to this function.
"""
function tape_copy end
tape_copy(x) = x
# Core.Box is used as closure captured variable container, so we should tape_copy its contents
tape_copy(x::Core.Box) = Core.Box(tape_copy(x.contents))
# ?? should we deepcopy Array and Dict by default?
# tape_copy(x::Array) = deepcopy(x)
# tape_copy(x::Dict) = deepcopy(x)

function copy_bindings(old::Bindings)
    newb = copy(old)
    for k in 1:length(old)
        isassigned(old, k) && (newb[k] = tape_copy(old[k]))
    end
    return newb
end

function Base.copy(tf::TapedFunction)
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf)
    new_tf.bindings = copy_bindings(tf.bindings)
    return new_tf
end
