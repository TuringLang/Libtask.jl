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
    output
    tape::T
end

mutable struct TapedFunction{F} <: Taped
    func::F # maybe a function or a callable obejct
    arity::Int
    ir::Union{Nothing, IRTools.IR}
    tape::RawTape
    counter::Int
    owner
    function TapedFunction(f::F; arity::Int=-1) where {F}
        new{F}(f, arity, nothing, NULL_TAPE, 1, nothing)
    end
end

mutable struct Box{T}
    val::T
end

## methods for Box
val(x) = x
val(x::Box) = x.val
val(x::TapedFunction) = x.func
box(x) = Box(x)
box(x::Box) = x
Base.show(io::IO, box::Box) = print(io, "Box(", box.val, ")")

## methods for RawTape and Taped
const NULL_TAPE = RawTape()
MacroTools.@forward TapedFunction.tape Base.iterate, Base.length
MacroTools.@forward TapedFunction.tape Base.push!, Base.getindex, Base.lastindex

result(t::RawTape) = isempty(t) ? nothing : val(t[end].output)
result(t::TapedFunction) = result(t.tape)

function increase_counter!(t::TapedFunction)
    t.counter > length(t) && return
    # instr = t[t.counter]
    t.counter += 1
    return t
end

function reset!(tf::TapedFunction, ir::IRTools.IR, tape::RawTape)
    tf.ir = ir
    tf.tape = tape
    return tf
end

function (tf::TapedFunction)(args...)
    if isempty(tf.tape)
        ir = IRTools.@code_ir tf.func(args...)
        ir = intercept(ir; recorder=:run_and_record!)
        tf.ir = ir
        tf.tape = RawTape()
        tf2 = IRTools.evalir(ir, tf, args...)
        @assert tf === tf2
        return result(tf)
    end
    run(tf.tape, args...)
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

function run(tape::RawTape, args...)
    if length(args) > 0
        input = map(box, args)
        tape[1].input = input
    end
    for instruction in tape
        instruction()
    end
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
Base.show(io::IO, instruction::AbstractInstruction) = print(io, "A ", typeof(instruction))

function Base.show(io::IO, instruction::Instruction)
    func = instruction.func
    tape = instruction.tape
    println(io, "Instruction($(func)$(map(val, instruction.input)), tape=$(objectid(tape)))")
end

function (instr::Instruction{F})() where F
    # catch run-time exceptions / errors.
    try
        output = instr.func(map(val, instr.input)...)
        instr.output.val = output
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
    catch e
        println(e, catch_backtrace());
        rethrow(e);
    end
end

## internal functions

function run_and_record!(tape::Taped, f, args...)
    f = val(f) # f maybe a Boxed closure
    output = try
        box(f(map(val, args)...))
    catch e
        @warn e
        Box{Any}(nothing)
    end
    ins = Instruction(f, args, output, tape)
    push!(tape, ins)
    return output
end

function run_and_record!(tape::Taped, ::typeof(_new), args...)
    output = try
        expr = Expr(:new, map(val, args)...)
        box(eval(expr))
    catch e
        @warn e
        Box{Any}(nothing)
    end
    ins = Instruction(_new, args, output, tape)
    push!(tape, ins)
    return output
end

function unbox_condition(ir)
    for blk in IRTools.blocks(ir)
        vars = keys(blk)
        brs = IRTools.branches(blk)
        for (i, br) in enumerate(brs)
            IRTools.isconditional(br) || continue
            cond = br.condition
            new_cond = IRTools.push!(
                blk,
                IRTools.xcall(@__MODULE__, :val, cond))
            brs[i] = IRTools.Branch(br; condition=new_cond)
        end
    end
end

box_args() = nothing
box_args(x) = x
box_args(args...) = args

function _replace_args(args, pairs::Dict)
    map(args) do x
        haskey(pairs, x) ? pairs[x] : x
    end
end

function intercept(ir; recorder=:run_and_record!)
    ir == nothing && return
    # we use tf instead of the original function as the first argument
    # get the TapedFunction
    tape = pushfirst!(ir, IRTools.xcall(Base, :identity, IRTools.arguments(ir)[1]))

    # box the args
    first_blk = IRTools.blocks(ir)[1]
    args = IRTools.arguments(first_blk)
    arity = length(args) - 1
    arg_pairs= Dict()

    args_var = args[1]
    if arity == 0
        args_var = IRTools.insertafter!(ir, tape, IRTools.xcall(@__MODULE__, :box_args))
    elseif arity == 1
        args_var = IRTools.insertafter!(ir, tape, IRTools.xcall(@__MODULE__, :box_args, args[2]))
        arg_pairs = Dict(args[2] => args_var)
    else # arity > 1
        args_var = IRTools.insertafter!(ir, tape, IRTools.xcall(@__MODULE__, :box_args, args[2:end]...))
        args_new, last_pos = [], args_var

        iter_state = []

        for i in 1:arity
            last_pos = IRTools.insertafter!(ir, last_pos, IRTools.xcall(Base, :indexed_iterate, args_var, i, iter_state...))
            args_iter = last_pos
            last_pos = IRTools.insertafter!(ir, last_pos, IRTools.xcall(Core, :getfield, args_iter, 1))
            push!(args_new, last_pos)
            if i != arity
                last_pos = IRTools.insertafter!(ir, last_pos, IRTools.xcall(Core, :getfield, args_iter, 2))
                iter_state = [last_pos]
            end
        end
        arg_pairs = Dict(zip(args[2:end], args_new))
    end

    # here we assumed the ir only has a return statement at its last block,
    # and we make sure the return value is from a function call (to `identity`)
    last_blk = IRTools.blocks(ir)[end]
    retv = IRTools.returnvalue(last_blk)
    IRTools.return!(last_blk, IRTools.xcall(Base, :identity, retv))

    for (x, st) in ir
        x == tape && continue
        if Meta.isexpr(st.expr, :call)
            new_args = (x == args_var) ? st.expr.args : _replace_args(st.expr.args, arg_pairs)
            ir[x] = IRTools.xcall(@__MODULE__, recorder, tape, new_args...)
        elseif Meta.isexpr(st.expr, :new)
            args = st.expr.args
            ir[x] = IRTools.xcall(@__MODULE__, recorder, tape, _new, args...)
        else
            @warn "Unknown IR code: " st
        end
    end
    # the real return value will be in the last instruction on the tape
    IRTools.return!(ir, tape)
    unbox_condition(ir)
    return ir
end
