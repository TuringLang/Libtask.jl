abstract type AbstractInstruction end

mutable struct Tape
    tape::Vector{<:AbstractInstruction}
    counter::Int
    owner
end

mutable struct Instruction{F} <: AbstractInstruction
    fun::F
    input::Tuple
    output
    tape::Tape
end

Tape() = Tape(Vector{AbstractInstruction}(), 1, nothing)
Tape(owner) = Tape(Vector{AbstractInstruction}(), 1, owner)
MacroTools.@forward Tape.tape Base.iterate, Base.length
MacroTools.@forward Tape.tape Base.push!, Base.getindex, Base.lastindex
const NULL_TAPE = Tape()

function setowner!(tape::Tape, owner)
    tape.owner = owner
end

mutable struct Box{T}
    val::T
end

val(x) = x
val(x::Box) = x.val
box(x) = Box(x)
box(x::Box) = x

gettape(x) = nothing
gettape(x::Instruction) = x.tape
function gettape(x::Tuple)
    for i in x
        gettape(i) != nothing && return gettape(i)
    end
end
result(t::Tape) = isempty(t) ? nothing : val(t[end].output)

function Base.show(io::IO, box::Box)
    println(io, "Box($(box.val))")
end

function Base.show(io::IO, instruction::Instruction)
    fun = instruction.fun
    tape = instruction.tape
    println(io, "Instruction($(fun)$(map(val, instruction.input)), tape=$(objectid(tape)))")
end

function Base.show(io::IO, tp::Tape)
    buf = IOBuffer()
    print(buf, "$(length(tp))-element Tape")
    isempty(tp) || println(buf, ":")
    i = 1
    for instruction in tp
        print(buf, "\t$i => ")
        show(buf, instruction)
        i += 1
    end
    print(io, String(take!(buf)))
end

function (instr::Instruction{F})() where F
    output = instr.fun(map(val, instr.input)...)
    instr.output.val = output
end

function run(tape::Tape, args...)
    if length(args) > 0
        input = map(box, args)
        tape[1].input = input
    end
    for instruction in tape
        instruction()
        tape.counter += 1
    end
end

function run_and_record!(tape::Tape, f, args...)
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
    tape = pushfirst!(ir, IRTools.xcall(@__MODULE__, :Tape))

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
        Meta.isexpr(st.expr, :call) || continue
        new_args = (x == args_var) ? st.expr.args : _replace_args(st.expr.args, arg_pairs)
        ir[x] = IRTools.xcall(@__MODULE__, recorder, tape, new_args...)
    end
    # the real return value will be in the last instruction on the tape
    IRTools.return!(ir, tape)
    unbox_condition(ir)
    return ir
end

mutable struct TapedFunction
    func # ::Function # maybe a callable obejct
    arity::Int
    ir::Union{Nothing, IRTools.IR}
    tape::Tape
    owner
    function TapedFunction(f; arity::Int=-1)
        new(f, arity, nothing, NULL_TAPE, nothing)
    end
end

function (tf::TapedFunction)(args...)
    if isempty(tf.tape)
        ir = IRTools.@code_ir tf.func(args...)
        ir = intercept(ir; recorder=:run_and_record!)
        tape = IRTools.evalir(ir, tf.func, args...)
        tf.ir = ir
        tf.tape = tape
        setowner!(tape, tf)
        return result(tape)
    end
    # TODO: use cache
    run(tf.tape, args...)
    return result(tf.tape)
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
