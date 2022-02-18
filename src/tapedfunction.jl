mutable struct Box{T}
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

mutable struct BlockInstruction{TA, T<:Taped} <: AbstractInstruction
    block_id::Int
    args::Vector{TA}
    tape::T
end

mutable struct BranchInstruction{TA, T<:Taped} <: AbstractInstruction
    condition::Box{Any}
    block_id::Int
    args::Vector{TA}
    tape::T
end

mutable struct ReturnInstruction{TA, T<:Taped} <: AbstractInstruction
    arg::TA
    tape::T
end

mutable struct TapedFunction{F} <: Taped
    func::F # maybe a function, a constructor, or a callable object
    arity::Int
    ir::IRTools.IR
    tape::RawTape
    counter::Int
    # map from BlockInstruction.block_id to its index on tape
    block_map::Dict{Int, Int}
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
        tf.block_map = Dict{Int, Int}()

        if init # init
            tf.arity = length(args)
            ir = IRTools.@code_ir tf.func(args...)
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
box(x) = Box(x)
box(x::Box) = x
Base.show(io::IO, box::Box) = print(io, "Box(", box.val, ")")

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

function Base.show(io::IO, instruction::BlockInstruction)
    id = instruction.block_id
    tape = instruction.tape
    println(io, "BlockInstruction($(id)->$(map(val, instruction.args)), tape=$(objectid(tape)))")
end

function Base.show(io::IO, instruction::BranchInstruction)
    tape = instruction.tape
    println(io, "BranchInstruction($(val(instruction.condition)), tape=$(objectid(tape)))")
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

function _new end
function (instr::Instruction{typeof(_new)})()
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

function (instr::BlockInstruction)()
    instr.tape.counter += 1
end

function (instr::BranchInstruction)()
    if instr.condition === nothing || !val(instr.condition) # unless
        target = instr.block_id
        target_idx = instr.tape.block_map[target]
        blk_ins = instr.tape[target_idx]
        @assert isa(blk_ins, BlockInstruction)
        @assert length(instr.args) == length(blk_ins.args)
        for i in 1:length(instr.args)
            blk_ins.args[i].val = val(instr.args[i])
        end
        instr.tape.counter = target_idx
    else
        instr.tape.counter += 1
    end
end

function (instr::ReturnInstruction)()
    instr.tape.retval = val(instr.arg)
end


## internal functions

arg_boxer(var, boxes::Dict{IRTools.Variable, Box{Any}}) = var
arg_boxer(var::IRTools.Variable, boxes::Dict{IRTools.Variable, Box{Any}}) =
    get!(boxes, var, Box{Any}(nothing))
function args_initializer(ins::BlockInstruction)
    return (args...) -> begin
        @assert length(args) + 1 == length(ins.args)
        ins.args[1].val = ins.tape.func
        for i in 1:length(args)
            ins.args[i + 1].val = val(args[i]) # fill the boxes
        end
    end
end

function translate!(taped::Taped, ir::IRTools.IR)
    tape = taped.tape
    boxes = Dict{IRTools.Variable, Box{Any}}()
    _box = (x) -> arg_boxer(x, boxes)
    for (blk_id, blk) in enumerate(IRTools.blocks(ir))
        # blocks
        blk_args = IRTools.arguments(blk)
        push!(tape, BlockInstruction(blk_id, map(_box, blk_args), taped))
        # `+ 1` because we will have an extra ins at the beginning
        taped.block_map[blk_id] = length(tape) + 1
        # normal instructions
        for (x, st) in blk
            if Meta.isexpr(st.expr, :call)
                args = map(_box, st.expr.args)
                # args[1] is the function
                f = args[1]
                ins = Instruction(f, args[2:end] |> Tuple,
                                  _box(x), taped)
                push!(tape, ins)
            elseif Meta.isexpr(st.expr, :new)
                args = map(_box, st.expr.args)
                ins = Instruction(_new, args |> Tuple, _box(x), taped)
                push!(tape, ins)
            elseif isa(st.expr, GlobalRef)
                ins = Instruction(val, (st.expr,), _box(x), taped)
                push!(tape, ins)
            else
                @error "Unknown IR code: " st
            end
        end

        # branches (including `return`)
        for br in IRTools.branches(blk)
            if br.condition === nothing && br.block == 0 # a return
                ins = ReturnInstruction(_box(br.args[1]), taped)
                push!(tape, ins)
            else
                cond = br.condition === nothing ? false : _box(br.condition)
                isa(cond, Bool) && (cond = Box{Any}(cond)) # unify the condiftion type
                ins = BranchInstruction(
                    cond, br.block, map(_box, br.args), taped)
                push!(tape, ins)
            end
        end
    end
    init_ins = Instruction(args_initializer(tape[1]), tuple(tape[1].args[2:end]...),
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
    # create a new uninitialized TapedFunction
    new_tf = TapedFunction(tf.func; cache=false, init=false)
    new_tf.block_map = tf.block_map
    new_tf.ir = tf.ir
    roster = Dict{UInt64, Any}()
    new_tape = copy(tf.tape, new_tf, roster)
    new_tf.tape = new_tape
    new_tf.counter = tf.counter
    return new_tf
end
