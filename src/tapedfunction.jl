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
mutable struct Instruction{F, T<:Taped} <: AbstractInstruction
    func::F
    input::Tuple
    output::Box
    tape::T
end

mutable struct BlockInstruction{T<:Taped} <: AbstractInstruction
    id::Int
    args::Vector
    tape::T
end

mutable struct BranchInstruction{T<:Taped} <: AbstractInstruction
    condition::Union{Bool, Box{Any}, Nothing}
    block::Int
    args::Vector
    tape::T
end

mutable struct ReturnInstruction{T<:Taped} <: AbstractInstruction
    arg
    tape::T
end

mutable struct TapedFunction{F} <: Taped
    func::F # maybe a function or a callable obejct
    arity::Int
    ir::Union{Nothing, IRTools.IR}
    tape::RawTape
    counter::Int
    block_map::Dict{Int, Int}
    retval
    owner
    function TapedFunction(f::F; arity::Int=-1) where {F}
        new{F}(f, arity, nothing, RawTape(), 1,
               Dict{Int, Int}(), nothing, nothing)
    end
end

## methods for Box
val(x) = x
val(x::Box) = x.val
val(x::TapedFunction) = x.func
box(x) = Box(x)
box(x::Box) = x
Base.show(io::IO, box::Box) = print(io, "Box(", box.val, ")")

## methods for RawTape and Taped
MacroTools.@forward TapedFunction.tape Base.iterate, Base.length
MacroTools.@forward TapedFunction.tape Base.push!, Base.getindex, Base.lastindex

result(t::TapedFunction) = t.retval

function init!(tf::TapedFunction, args)
    if isempty(tf.tape)
        ir = IRTools.@code_ir tf.func(args...)
        tf.ir = ir
        translate!(tf, ir)
    end
    return tf
end

function reset!(tf::TapedFunction, ir::IRTools.IR, tape::RawTape)
    tf.ir = ir
    tf.tape = tape
    blk_map = Dict{Int, Int}()

    for (i, ins) in enumerate(tf.tape)
        isa(ins, BlockInstruction) || continue
        blk_map[ins.id] = i
    end

    tf.block_map = blk_map
    tf.counter = 1
    return tf
end

function (tf::TapedFunction)(args...)
    init!(tf, args)
    # run the raw tape
    if length(args) > 0
        input = map(box, args)
        tf.tape[1].input = input
    end
    tf.counter = 1
    while true
        ins = tf[tf.counter]
        ins()
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
    id = instruction.id
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

function _new end
function (instr::Instruction{typeof(_new)})()
    # catch run-time exceptions / errors.
    try
        expr = Expr(:new, map(val, instr.input)...)
        output = eval(expr)
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
        target = instr.block
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

arg_boxer(var, boxes) = var
arg_boxer(var::GlobalRef, boxes) = eval(var)
arg_boxer(var::QuoteNode, boxes) = eval(var)
arg_boxer(var::IRTools.Variable, boxes) = get!(boxes, var, Box{Any}(nothing))
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
            else
                if isa(st.expr, Symbol) || isa(st.expr, GlobalRef)
                    v = eval(st.expr)
                    ins = Instruction(identity, (v,), _box(x), taped)
                    push!(tape, ins)
                else
                    @warn "Unknown IR code: " st
                end
            end
        end

        # branches (including `return`)
        for br in IRTools.branches(blk)
            if br.condition === nothing && br.block == 0
                ins = ReturnInstruction(_box(br.args[1]), taped)
                push!(tape, ins)
            else
                ins = BranchInstruction(
                    _box(br.condition), br.block, map(_box, br.args), taped)
                push!(tape, ins)
            end
        end
    end
    init_ins = Instruction(args_initializer(tape[1]), (),
                           Box{Any}(nothing), taped)
    insert!(tape, 1, init_ins)
end
