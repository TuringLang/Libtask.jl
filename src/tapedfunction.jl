## Instruction and TapedFunction

abstract type AbstractInstruction end
const RawTape = Vector{AbstractInstruction}

function _infer(f, args_type)
    # `code_typed` returns a vector: [Pair{Core.CodeInfo, DataType}]
    ir0 = code_typed(f, Tuple{args_type...}, optimize=false)[1][1]
    # ir1 = CodeInfoTools.code_inferred(f, args_type...)
    # ir1.ssavaluetypes = ir0.ssavaluetypes
    return ir0
end

# const Bindings = Dict{Symbol, Any}
const Bindings = Vector{Any}

mutable struct TapedFunction{F, TapeType}
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::Core.CodeInfo
    tape::TapeType
    counter::Int
    bindings::Bindings
    retval::Int

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
        bindings, tape = translate!(RawTape(), ir)

        tf = new{F, T}(f, length(args), ir, tape, 1, bindings, 0)
        TRCache[cache_key] = tf # set cache
        return tf
    end

    TapedFunction(f, args...; cache=false) =
        TapedFunction{typeof(f), RawTape}(f, args...; cache=cache)

    function TapedFunction{F, T0}(tf::TapedFunction{F, T1}) where {F, T0, T1}
        new{F, T0}(tf.func, tf.arity, tf.ir, tf.tape,
               tf.counter, tf.bindings, 0)
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

@inline _inner_getter(tf::TapedFunction, v::Int) = @inbounds tf.bindings[v]
@inline _lookup(tf::TapedFunction, v::Int) = @inbounds tf.bindings[v]
@inline _update_var!(tf::TapedFunction, v::Int, c::T) where T = @inbounds tf.bindings[v] = c

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
    condition::Int
    # we enusre a 1-to-1 mapping between ir.code and instruction
    # so here we can use the index directly.
    dest::Int
end

struct ReturnInstruction <: AbstractInstruction
    arg::Int
end

@inline result(t::TapedFunction) = t.bindings[t.retval]

function (tf::TapedFunction)(args...; callback=nothing, continuation=false)
    if !continuation # reset counter and retval to run from the start
        tf.counter = 1
        tf.retval = 0
    end

    # set args
    if tf.counter <= 1
        # _update_var!(tf, 1, tf.func)
        @inbounds tf.bindings[1] = tf.func
        for i in 1:length(args)
            slot = i + 1
            # _update_var!(tf, slot, args[i])
            @inbounds tf.bindings[slot] = args[i]
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
    println(io, "GotoInstruction(", instr.condition, ", dest=", instr.dest, ")")
end
#=
@generated function (instr::Instruction{F, N})(tf::TapedFunction) where {F, N}
    ftype = instr.parameters[1]
    arity = instr.parameters[2]
    _func = ftype == Int ? :(_lookup(tf, instr.func)) : :(instr.func)
    body = quote
        try
            func = $(_func)
            output = func() # will inject arguments later
            # _update_var!(tf, instr.output, output)
            @inbounds tf.bindings[instr.output] = output
            tf.counter += 1
        catch e
            # catch run-time exceptions / errors.
            println("counter=", tf.counter)
            println("tf=", tf)
            println(e, catch_backtrace());
            rethrow(e);
        end
    end
    Base.remove_linenums!(body)
    # inject arguments
    call_args = body.args[1].args[1].args[2].args[2].args
    for i in 1:arity
        push!(call_args, :(_lookup(tf, instr.input[$i])))
    end
    body
end
=#

function (instr::Instruction{F})(tf::TapedFunction) where F
    # catch run-time exceptions / errors.
    try
        func = F == Int ? _lookup(tf, instr.func) : instr.func
        inputs = map(x -> _lookup(tf, x), instr.input)
        output = func(inputs...)
        # _update_var!(tf, instr.output, output)
        tf.bindings[instr.output] = output
        tf.counter += 1
    catch e
        println("counter=", tf.counter)
        println("tf=", tf)
        println(e, catch_backtrace());
        rethrow(e);
    end
end

function (instr::GotoInstruction)(tf::TapedFunction)
    cond = instr.condition == -1 ? true :
        instr.condition == 0 ? false :
        _lookup(tf, instr.condition)

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
const CNT_SLOT = 200
const CNT_LITE = 1000
const OFFSET_VAR = CNT_SLOT + CNT_LITE

function bind_var!(var, bindings::Bindings, ir::Core.CodeInfo) # for literal constants
    last_idx = bindings[CNT_SLOT]
    idx = last_idx + 1
    @assert idx < OFFSET_VAR
    bindings[CNT_SLOT] = idx
    bindings[idx] = var
    idx
end
bind_var!(var::GlobalRef, bindings::Bindings, ir::Core.CodeInfo) =
    bind_var!(getproperty(var.mod, var.name), bindings, ir)
bind_var!(var::QuoteNode, bindings::Bindings, ir::Core.CodeInfo) =
    bind_var!(eval(var), bindings, ir)
bind_var!(var::Core.SSAValue, bindings::Bindings, ir::Core.CodeInfo) =
    bind_var!(var.id + OFFSET_VAR, bindings, ir.ssavaluetypes[var.id])
bind_var!(var::Core.TypedSlot, bindings::Bindings, ir::Core.CodeInfo) =
    (@assert var.id < CNT_SLOT; bind_var!(var.id, bindings, ir.slottypes[var.id]))
bind_var!(var::Core.SlotNumber, bindings::Bindings, ir::Core.CodeInfo) =
    (@assert var.id < CNT_SLOT; bind_var!(var.id, bindings, ir.slottypes[var.id]))
bind_var!(var::Int, boxes::Bindings, c::Core.Const) =
    bind_var!(var, boxes, _loose_type(Type{c.val}))
bind_var!(var::Int, boxes::Bindings, c::Core.PartialStruct) =
    bind_var!(var, boxes, _loose_type(c.typ))
function bind_var!(var::Int, bindings::Bindings, ::Type{T}) where T
    var > length(bindings) && resize!(bindings, var + 10)
    var
end

function translate!(tape::RawTape, ir::Core.CodeInfo)
    bindings = Bindings()
    resize!(bindings, OFFSET_VAR + 10)
    bindings[CNT_SLOT] = CNT_SLOT

    for (idx, line) in enumerate(ir.code)
        isa(line, Core.Const) && (line = line.val) # unbox Core.Const
        isconst = isa(ir.ssavaluetypes[idx], Core.Const)
        ins = translate!!(Core.SSAValue(idx), line, bindings, isconst, ir)
        push!(tape, ins)
    end
    return (bindings, tape)
end

const IRVar = Union{Core.SSAValue, Core.SlotNumber}

function _const_instruction(var::IRVar, v, bindings::Bindings, ir)
    if isa(var, Core.SSAValue)
        box = bind_var!(var, bindings, ir)
        bindings[box] = v
        return GotoInstruction(-1, 0) # NOOP
    end
    return Instruction(identity, (bind_var!(v, bindings, ir),), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.NewvarNode,
                     bindings::Bindings, isconst::Bool, @nospecialize(ir))
    # use a noop to ensure the 1-to-1 mapping from ir.code to instructions
    # on tape. see GotoInstruction.dest.
    return GotoInstruction(-1, 0)
end

function translate!!(var::IRVar, line::GlobalRef,
                     bindings::Bindings, isconst::Bool, ir)
    if isconst
        v = ir.ssavaluetypes[var.id].val
        return _const_instruction(var, v, bindings, ir)
    end
    return Instruction(() -> getproperty(line.mod, line.name), (), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.SlotNumber,
                     bindings::Bindings, isconst::Bool, ir)
    if isconst
        v = ir.ssavaluetypes[var.id].val
        return _const_instruction(var, v, bindings, ir)
    end
    return Instruction(identity, (bind_var!(line, bindings, ir),), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.TypedSlot,
                     bindings::Bindings, isconst::Bool, ir)
    input_box = bind_var!(Core.SlotNumber(line.id), bindings, ir)
    return Instruction(identity, (input_box,), bind_var!(var, bindings, ir))
end

function translate!!(var::IRVar, line::Core.GotoIfNot,
                     bindings::Bindings, isconst::Bool, ir)
    _cond = bind_var!(line.cond, bindings, ir)
    cond = if isa(_cond, Bool)
        _cond ? -1 : 0
    else
        _cond
    end
    return GotoInstruction(cond, line.dest)
end

function translate!!(var::IRVar, line::Core.GotoNode,
                     bindings::Bindings, isconst::Bool, @nospecialize(ir))
    return GotoInstruction(0, line.label)
end

function translate!!(var::IRVar, line::Core.ReturnNode,
                     bindings::Bindings, isconst::Bool, ir)
    return ReturnInstruction(bind_var!(line.val, bindings, ir))
end

_canbeoptimized(v) = isa(v, DataType) || isprimitivetype(typeof(v))
function translate!!(var::IRVar, line::Expr,
                     bindings::Bindings, isconst::Bool, ir::Core.CodeInfo)
    head = line.head
    _bind_fn = (x) -> bind_var!(x, bindings, ir)
    if head === :new
        args = map(_bind_fn, line.args)
        return Instruction(__new__, args |> Tuple, _bind_fn(var))
    elseif head === :call
        # Only some of function calls can be optimized even many of their results are
        # inferred as constants: the optimization tries to do the call at compile-time,
        # so we only optimize primitive adn datatype constants for now.
        if isconst
            v = ir.ssavaluetypes[var.id].val
            _canbeoptimized(v) && return _const_instruction(var, v, bindings, ir)
        end
        args = map(_bind_fn, line.args)
        # args[1] is the function
        func = line.args[1]
        if Meta.isexpr(func, :static_parameter) # func is a type parameter
            func = ir.parent.sparam_vals[func.args[1]]
        elseif isa(func, GlobalRef)
            func = getproperty(func.mod, func.name)
        else # a var?
            func = args[1] # a var(box)
        end
        return Instruction(func, args[2:end] |> Tuple, _bind_fn(var))
    elseif head === :(=)
        # line.args[1] (the left hand side) is a SlotNumber, and it should be the output
        lhs = line.args[1]
        rhs = line.args[2] # the right hand side, maybe a Expr, or a var, or ...
        if Meta.isexpr(rhs, (:new, :call))
            return translate!!(lhs, rhs, bindings, false, ir)
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
