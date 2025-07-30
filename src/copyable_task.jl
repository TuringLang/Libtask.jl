"""
    get_taped_globals(T::Type)

When called from inside a call to a `TapedTask`, this will return whatever is contained in
its `taped_globals` field.

The type `T` is required for optimal performance. If you know that the result of this
operation must return a specific type, specify `T`. If you do not know what type it will
return, pass `Any` -- this will typically yield type instabilities, but will run correctly.

See also [`set_taped_globals!`](@ref).
"""
@noinline function get_taped_globals(::Type{T}) where {T}
    # This function is `@noinline`d to ensure that the type-unstable items in here do not
    # appear in a calling function, and cause allocations.
    #
    # The return type of `task_local_storage(:task_variable)` is `Any`. To ensure that this
    # type instability does not propagate through the rest of the code, we `typeassert` the
    # result to be `T`. By doing this, callers of this function will (hopefully) think
    # carefully about how they can figure out what type they have put in global storage.
    return typeassert(task_local_storage(:task_variable), T)
end

# A dummy global variable used inside `produce` to ensure that `produce` calls never get
# inlined away. This doesn't ever actually get run, provided that the `produce` call is made
# from a `consume` call.
__v::Int = 5

"""
    produce(x)

When run inside a [`TapedTask`](@ref), will immediately yield to the caller, returning value
`x`. Users will typically hit this function when calling `consume`.

See also: [`Libtask.consume`](@ref)
"""
@noinline function produce(x)
    # This function is basically just a placeholder -- it never gets run when a user
    # `consume`s a `TapedTask`. Rather, it is transformed into other statements during the
    # IR transformation pass below. Think of it more as being a reserved statement (e.g.
    # like `return`) rather than a function call.
    global __v = 4 # silly side-effect to prevent this call getting constant-folded away. Should really use the effects system.
    return x
end

"""
    callable_ret_type(sig, produce_types)

Computes the types which might possibly be returned from a `TapedTask`, where `sig` is the
signature (something of the form `Tuple{...}`) of the function from which the `TapedTask` is
constructed, and `produce_types` are the possible types which such a call might `produce`.

In general, computing `produce_types` requires analysing the `produce` type of any statment
in the IR associated to `sig` which might `produce`. See locations where this function is
called to see where this happens.
"""
function callable_ret_type(sig, produce_types)
    produce_type = Union{}
    for t in produce_types
        p = isconcretetype(t) ? ProducedValue{t} : ProducedValue{T} where {T<:t}
        produce_type = CC.tmerge(p, produce_type)
    end
    return Union{Base.code_ircode_by_type(sig)[1][2],produce_type}
end

"""
    build_callable(sig::Type{<:Tuple})

Returns a `MistyClosure` which is used by `TapedTask` to implement the
`produce`-`consume` interface. If this method has been called using `sig` in
the current world age, will make a copy of an existing `MistyClosure`. If not,
will derive it from scratch (derive the IR + compile it etc).
"""
function build_callable(sig::Type{<:Tuple})
    if sig <: Tuple{typeof(produce),Any}
        msg = """
            Can not construct a TapedTask for a 'naked' call to `produce`.
             Please wrap the call to `produce` in a function, and construct a
             TapedTask from that function."""
        throw(ArgumentError(msg))
    end
    key = CacheKey(Base.get_world_counter(), sig)
    if haskey(mc_cache, key)
        return fresh_copy(mc_cache[key])
    else
        ir = Base.code_ircode_by_type(sig)[1][1]
        # Check whether this is a varargs call.
        isva = which(sig).isva
        bb, refs, types = derive_copyable_task_ir(BBCode(ir))
        unoptimised_ir = IRCode(bb)
        optimised_ir = optimise_ir!(unoptimised_ir)
        mc_ret_type = callable_ret_type(sig, types)
        mc = misty_closure(mc_ret_type, optimised_ir, refs...; isva=isva, do_compile=true)
        mc_cache[key] = mc
        return mc, refs[end]
    end
end

# Note: `position` must be a const field of this mutable struct, rather than
# simply being a mutable field because it must alias a `Ref` in the captures
# field of `mc`. This correspondence is ensured upon construction.
mutable struct TapedTask{Ttaped_globals,Tfargs,Tmc<:MistyClosure}
    taped_globals::Ttaped_globals
    const fargs::Tfargs
    const mc::Tmc
    const position::Base.RefValue{Int32}
end

struct CacheKey
    world_age::UInt
    key::Any
end

const mc_cache = Dict{CacheKey,MistyClosure}()

"""
    TapedTask(taped_globals::Any, f, args...; kwargs...)

Construct a `TapedTask` with the specified `taped_globals`, for function `f`, positional
arguments `args`, and keyword argument `kwargs`.

# Extended Help

There are three central features of a `TapedTask`, which we demonstrate via three examples.

## Resumption

The function [`Libtask.produce`](@ref) has a special meaning in Libtask. You can insert it
into regular Julia functions anywhere that you like. For example
```jldoctest tt
julia> function f()
           for t in 1:2
               produce(t)
               t += 1
           end
           return nothing
       end
f (generic function with 1 method)
```

If you construct a `TapedTask` from `f`, and call [`Libtask.consume`](@ref) on it, you'll
see
```jldoctest tt
julia> t = TapedTask(nothing, f);

julia> consume(t)
1
```
The semantics of this are that [`Libtask.consume`](@ref) runs the function `f` until it
reaches the call to [`Libtask.produce`](@ref), at which point it will return the argument
to [`Libtask.produce`](@ref).

Subsequent calls to [`Libtask.produce`](@ref) will _resume_ execution of `f` immediately
after the last [`Libtask.produce`](@ref) statement that was hit.
```jldoctest tt
julia> consume(t)
2
```

When there are no more [`Libtask.produce`](@ref) statements to hit, calling
[`Libtask.consume`](@ref) will return `nothing`:
```jldoctest tt
julia> consume(t)

```

## Copying

[`TapedTask`](@ref)s can be copied. Doing so creates a completely independent object.
For example:
```jldoctest tt
julia> t2 = TapedTask(nothing, f);

julia> consume(t2)
1
```

If we make a copy and advance its state, it produces the same value that the original would
have produced:
```jldoctest tt
julia> t3 = copy(t2);

julia> consume(t3)
2
```

Moreover, advancing the state of the copy has not advanced the state of the original,
because they are completely independent copies:
```jldoctest tt
julia> consume(t2)
2
```

## TapedTask-Specific Globals

It is often desirable to permit a copy of a task and the original to differ in very specific
ways. For example, in the context of Sequential Monte Carlo, you might want the only
difference between two copies to be their random number generator.

A generic mechanism is available to achieve this. [`Libtask.get_taped_globals`](@ref) and
[`Libtask.set_taped_globals!`](@ref) let you set and retrieve a variable which is specific
to a given [`Libtask.TapedTask`](@ref). The former can be called inside a function:
```jldoctest sv
julia> function f()
           produce(get_taped_globals(Int))
           produce(get_taped_globals(Int))
           return nothing
       end
f (generic function with 1 method)
```

The first argument to [`Libtask.TapedTask`](@ref) is the value that
[`Libtask.get_taped_globals`](@ref) will return:
```jldoctest sv
julia> t = TapedTask(1, f);

julia> consume(t)
1
```

The value that it returns can be changed between [`Libtask.consume`](@ref) calls:
```jldoctest sv
julia> set_taped_globals!(t, 2)

julia> consume(t)
2
```

`Int`s have been used here, but it is permissible to set the value returned by
[`Libtask.get_taped_globals`](@ref) to anything you like.

# Implementation Notes

Under the hood, we implement a `TapedTask` by obtaining the `IRCode` associated to the
original function, transforming it so that it implements the semantics required by the
`produce` / `consume` interface, and placing it inside a `MistyClosure` to make it possible
to execute.

There are two main considerations when transforming the `IRCode`. The first is to ensure
that the "state" of a `TapedTask` can be copied, so that a `TapedTask` can be copied, and
resumed later. The complete state of a `TapedTask` is given by its arguments, and the value
associated to each ssa (these are initially undefined).
To make it possible to copy the state of the ssa values, we place `Base.RefValue{T}`s into
the captures of the `MistyClosure` which implements the `TapedTask`, one for each ssa in the
IR (`T` is the type inferred for that ssa). A call is replaced by reading in values of ssas
from these refs, applying the original operation, and writing the result to the ref
associated to the instruction. For example, if the original snippet of `IRCode` is something
like
```julia
%5 = f(%3, _1)
```
the transformed IR would be something like
```julia
%5 = ref_for_%3[]
%6 = f(%5, _1)
ref_for_%5[] = %6
```
Setting things up in this manner ensures that an independent copy is made by simply copying
all of the refs. A `deepcopy` is required for correctness as, while the refs do not alias
one another (by construction), their contents might. For example, two refs may contain the
same `Array`, and in general the behaviour of a function depends on this relationship.

The second component of the transformation is implementing the `produce` mechanism, and the
ability to resume computation from where we produced. Roughly speaking, the `IRCode` must be
modified to ensure that whenever a `produce` call in encountered, the `MistyClosure` returns
the argument to `produce`, and that subsequent calls resume computation immediately after
the `produce` statement. This resumption is achieved by setting the value of a counter
prior to returning following a `produce` statement -- a sequence of comparisons against this
counter, and [`GotoIfNot`](https://docs.julialang.org/en/v1/devdocs/ast/#Lowered-form)
statement are inserted at the top of the IR. These are used to jump to the point in the code
from which computation should resume. These are set up such that, when the `TapedTask` is
first run, computation start froms the first statement. Observe that this is also
facilitated by the ref mechanism discussed above, as it ensures that the state persists
between calls to a `MistyClosure`.

The above gives the broad outline of how `TapedTask`s are implemented. We refer interested
readers to the code, which is extensively commented to explain implementation details.
"""
function TapedTask(taped_globals::Any, fargs...; kwargs...)
    all_args = isempty(kwargs) ? fargs : (Core.kwcall, getfield(kwargs, :data), fargs...)
    seed_id!() # a BBCode thing.
    mc, count_ref = build_callable(typeof(all_args))
    return TapedTask(taped_globals, all_args, mc, count_ref)
end

"""
    fresh_copy(mc::T) where {T<:MistyClosure}

Creates an independent copy of `mc` by (carefully) replacing the `Ref`s it
contains in its `captures`. The resulting `MistyClosure` is safe to run.

This is achieved by replacing most `Ref`s with new `Ref`s of the same (el)type,
but with nothing stored in them -- values will be stored in them when the
new `MistyClosure` is called. The only exception are `DynamicCallable`s and
`LazyCallable`s -- these are constructed when the `MistyClosure`s IR is derived,
so fresh instances of them are placed in the associated `Ref`.

The position counter is reset to `-1` (indicating that execution should proceed
from the start of the IR, rather than eg. jumping to a line following a
`produce` statement.
"""
function fresh_copy(mc::T) where {T<:MistyClosure}
    new_captures = map(mc.oc.captures) do r
        if eltype(r) <: DynamicCallable
            return Base.RefValue(DynamicCallable())
        elseif eltype(r) <: LazyCallable
            return _typeof(r)(eltype(r)())
        else
            return _typeof(r)()
        end
    end
    new_position = new_captures[end]
    new_position[] = -1
    return replace_captures(mc, new_captures), new_position
end

"""
    set_taped_globals!(t::TapedTask, new_taped_globals)::Nothing

Set the `taped_globals` of `t` to `new_taped_globals`. Any calls to
[`get_taped_globals`](@ref) in future calls to `consume(t)` (either directly, or implicitly
via iteration) will see this new value.
"""
function set_taped_globals!(t::TapedTask{T}, new_taped_globals::T)::Nothing where {T}
    t.taped_globals = new_taped_globals
    return nothing
end

"""
    Base.copy(t::TapedTask)

Makes a completely independent copy of `t`. `consume` can be applied to either the copy of
`t` or the original without advancing the state of the other.
"""
Base.copy(t::T) where {T<:TapedTask} = deepcopy(t)

"""
    consume(t::TapedTask)

Run `t` until it makes a call to `produce`. If this is the first time that `t` has been
called, it starts execution from the entry point. If `consume` has previously been called on
`t`, it will resume from the last `produce` call. If there are no more `produce` calls,
`nothing` will be returned.
"""
@inline function consume(t::TapedTask)
    task_local_storage(:task_variable, t.taped_globals)
    v = t.mc.oc(t.fargs...)
    return v isa ProducedValue ? v[] : nothing
end

"""
    might_produce(sig::Type{<:Tuple})::Bool

`true` if a call to method with signature `sig` is permitted to contain
`Libtask.produce` statements.

This is an opt-in mechanism. the fallback method of this function returns `false` indicating
that, by default, we assume that calls do not contain `Libtask.produce` statements.
"""
might_produce(::Type{<:Tuple}) = false

# Helper struct used in `derive_copyable_task_ir`.
struct TupleRef
    n::Int
end

# Unclear whether this is needed.
get_value(x::GlobalRef) = getglobal(x.mod, x.name)
get_value(x::QuoteNode) = x.value
get_value(x) = x

"""
    is_produce_stmt(x)::Bool

`true` if `x` is an expression of the form `Expr(:call, produce, %x)` or a similar `:invoke`
expression, otherwise `false`.
"""
function is_produce_stmt(x)::Bool
    if Meta.isexpr(x, :invoke) && length(x.args) == 3 && x.args[1] isa Core.MethodInstance
        # This branch is hit on Julia 1.11 and earlier.
        return x.args[1].specTypes <: Tuple{typeof(produce),Any}
    elseif Meta.isexpr(x, :invoke) && length(x.args) == 3 && x.args[1] isa Core.CodeInstance
        # This branch is hit on Julia 1.12.
        return x.args[1].def.specTypes <: Tuple{typeof(produce),Any}
    elseif Meta.isexpr(x, :call) && length(x.args) == 2
        return get_value(x.args[1]) === produce
    else
        return false
    end
end

"""
    stmt_might_produce(x, ret_type::Type)::Bool

`true` if `x` might contain a call to `produce`, and `false` otherwise.
"""
function stmt_might_produce(x, ret_type::Type)::Bool

    # Statement will terminate in an unusual fashion, so don't bother recursing.
    # This isn't _strictly_ correct (there could be a `produce` statement before the
    # `throw` call is hit), but this seems unlikely to happen in practice. If it does, the
    # user should get a sensible error message anyway.
    ret_type == Union{} && return false

    # Statement will terminate in the usual fashion, so _do_ bother recusing.
    is_produce_stmt(x) && return true
    @static if VERSION >= v"1.12-"
        # On Julia 1.12 x.args has CodeInstances rather than MethodInstances. We use .def
        # to get the MethodInstances.
        Meta.isexpr(x, :invoke) && return might_produce(x.args[1].def.specTypes)
    else
        Meta.isexpr(x, :invoke) && return might_produce(x.args[1].specTypes)
    end
    if Meta.isexpr(x, :call)
        # This is a hack -- it's perfectly possible for `DataType` calls to produce in general.
        f = get_function(x.args[1])
        _might_produce = !isa(f, Union{Core.IntrinsicFunction,Core.Builtin,DataType})
        return _might_produce
    end
    return false
end

get_function(x) = x
get_function(x::Expr) = eval(x)
get_function(x::GlobalRef) = isconst(x) ? getglobal(x.mod, x.name) : x.binding

"""
    produce_value(x::Expr)

Returns the value that a `produce` statement returns. For example, for the statment
`produce(%x)`, this function will return `%x`.
"""
function produce_value(x::Expr)
    is_produce_stmt(x) || throw(error("Not a produce statement. Please report this error."))
    Meta.isexpr(x, :invoke) && return x.args[3]
    return x.args[2] # must be a `:call` Expr.
end

struct ProducedValue{T}
    x::T
end
ProducedValue(::Type{T}) where {T} = ProducedValue{Type{T}}(T)

@inline Base.getindex(x::ProducedValue) = x.x

"""
    inc_args(stmt::T)::T where {T}

Returns a new `T` which is equal to `stmt`, except any `Argument`s present in `stmt` are
incremented by `1`. For example
```jldoctest
julia> Libtask.inc_args(Core.ReturnNode(Core.Argument(1)))
:(return _2)
```
"""
inc_args(x::Expr) = Expr(x.head, map(__inc, x.args)...)
inc_args(x::ReturnNode) = isdefined(x, :val) ? ReturnNode(__inc(x.val)) : x
inc_args(x::IDGotoIfNot) = IDGotoIfNot(__inc(x.cond), x.dest)
inc_args(x::IDGotoNode) = x
function inc_args(x::IDPhiNode)
    new_values = Vector{Any}(undef, length(x.values))
    for n in eachindex(x.values)
        if isassigned(x.values, n)
            new_values[n] = __inc(x.values[n])
        end
    end
    return IDPhiNode(x.edges, new_values)
end
inc_args(::Nothing) = nothing
inc_args(x::GlobalRef) = x
inc_args(x::Core.PiNode) = Core.PiNode(__inc(x.val), __inc(x.typ))

__inc(x::Argument) = Argument(x.n + 1)
__inc(x) = x

const TypeInfo = Tuple{Vector{Any},Dict{ID,Type}}

"""
    _typeof(x)

Central definition of typeof, which is specific to the use-required in this package.
Largely the same as `Base._stable_typeof`, differing only in a handful of
situations, for example:
```jldoctest
julia> Base._stable_typeof((Float64,))
Tuple{DataType}

julia> Libtask._typeof((Float64,))
Tuple{Type{Float64}}
```
"""
_typeof(x) = Base._stable_typeof(x)
_typeof(x::Tuple) = Tuple{map(_typeof, x)...}
_typeof(x::NamedTuple{names}) where {names} = NamedTuple{names,_typeof(Tuple(x))}

"""
    get_type(info::ADInfo, x)

Returns the static / inferred type associated to `x`.
"""
get_type(info::TypeInfo, x::Argument) = info[1][x.n - 1]
get_type(info::TypeInfo, x::ID) = CC.widenconst(info[2][x])
get_type(::TypeInfo, x::QuoteNode) = _typeof(x.value)
get_type(::TypeInfo, x) = _typeof(x)
function get_type(::TypeInfo, x::GlobalRef)
    return isconst(x) ? _typeof(getglobal(x.mod, x.name)) : x.binding.ty
end
function get_type(::TypeInfo, x::Expr)
    x.head === :boundscheck && return Bool
    return error("Unrecognised expression $x found in argument slot.")
end

function derive_copyable_task_ir(ir::BBCode)::Tuple{BBCode,Tuple,Vector{Any}}

    # The location from which all state can be retrieved. Since we're using `OpaqueClosure`s
    # to implement `TapedTask`s, this appears via the first argument.
    refs_id = Argument(1)

    # Increment all arguments by 1.
    for bb in ir.blocks, (n, inst) in enumerate(bb.insts)
        bb.insts[n] = CC.NewInstruction(
            inc_args(inst.stmt), inst.type, inst.info, inst.line, inst.flag
        )
    end

    # Construct map between SSA IDs and their index in the state data structure and back.
    # Also construct a map from each ref index to its type. We only construct `Ref`s
    # for statements which return a value e.g. `IDGotoIfNot`s do not have a meaningful
    # return value, so there's no need to allocate a `Ref` for them.
    ssa_id_to_ref_index_map = Dict{ID,Int}()
    ref_index_to_ssa_id_map = Dict{Int,ID}()
    ref_index_to_type_map = Dict{Int,Type}()
    id_to_type_map = Dict{ID,Type}()
    is_used_dict = characterise_used_ids(collect_stmts(ir))
    n = 0
    for bb in ir.blocks
        for (id, stmt) in zip(bb.inst_ids, bb.insts)
            id_to_type_map[id] = CC.widenconst(stmt.type)
            stmt.stmt isa IDGotoNode && continue
            stmt.stmt isa IDGotoIfNot && continue
            stmt.stmt === nothing && continue
            stmt.stmt isa ReturnNode && continue
            is_used_dict[id] || continue
            n += 1
            ssa_id_to_ref_index_map[id] = n
            ref_index_to_ssa_id_map[n] = id
            ref_index_to_type_map[n] = CC.widenconst(stmt.type)
        end
    end

    # Specify data structure containing `Ref`s for all of the SSAs.
    _refs = Any[Ref{ref_index_to_type_map[p]}() for p in 1:length(ref_index_to_ssa_id_map)]

    # Ensure that each basic block ends with a non-producing statement. This is achieved by
    # replacing any fall-through terminators with `IDGotoNode`s. This is not strictly
    # necessary, but simplifies later stages of the pipeline, as discussed variously below.
    for (n, block) in enumerate(ir.blocks)
        if terminator(block) === nothing
            # Fall-through terminator, so next block in `ir.blocks` is the unique successor
            # block of `block`. Final block cannot have a fall-through terminator, so asking
            # for element `n + 1` is always going to be valid.
            successor_id = ir.blocks[n + 1].id
            push!(block.insts, new_inst(IDGotoNode(successor_id)))
            push!(block.inst_ids, ID())
        end
    end

    # For each existing basic block, create a sequence of `NamedTuple`s which
    # define the manner in which it must be split.
    # A block will in general be split as follows:
    # 1 - %1 = φ(...)
    # 1 - %2 = φ(...)
    # 1 - %3 = call_which_must_not_produce(...)
    # 1 - %4 = produce(%3)
    # 2 - %5 = call_which_must_not_produce(...)
    # 2 - %6 = call_which_might_produce(...)
    # 3 - %7 = call_which_must_not_produce(...)
    # 3 - terminator (GotoIfNot, GotoNode, etc)
    #
    # The numbers on the left indicate which split each statement falls. The first
    # split comprises all statements up until the first produce / call-which-might-produce.
    # Consequently, the first split will always contain any `PhiNode`s present in the block.
    # The next set of statements up until the next produce / call-which-might-produce form
    # the second split, and so on.
    # We enforced above the condition that the final statement in a basic block must not
    # produce. This ensures that the final split does not produce. While not strictly
    # necessary, this simplifies the implementation (see below).
    #
    # As a result of the above, a basic block will be associated to exactly one split if it
    # does not contain any statements which may produce.
    #
    # Each `NamedTuple` contains a `start` index and `last` index, indicating the position
    # in the block at which the corresponding split starts and finishes.
    all_splits = map(ir.blocks) do block
        split_ends = vcat(
            findall(
                inst -> stmt_might_produce(inst.stmt, CC.widenconst(inst.type)),
                block.insts,
            ),
            length(block),
        )
        return map(enumerate(split_ends)) do (n, split_end)
            return (start=(n == 1 ? 0 : split_ends[n - 1]) + 1, last=split_end)
        end
    end

    # Owing to splitting blocks up, we will need to re-label some `GotoNode`s and
    # `GotoIfNot`s. To understand this, consider the following block, whose original `ID`
    # we assume to be `ID(old_id)`.
    # ID(new_id) - %1 = φ(ID(3) => ...)
    # ID(new_id) - %3 = call_which_must_not_produce(...)
    # ID(new_id) - %4 = produce(%3)
    # ID(old_id) - GotoNode(ID(5))
    #
    # In the above, the entire block was original associated to a single ID, `ID(old_id)`,
    # but is now split into two sections. We keep the original ID for the final split, and
    # assign a new one to the first split. As a result, any `PhiNode`s in other blocks
    # which have edges incoming from `ID(old_id)` will remain valid.
    # However, if we adopt this strategy for all blocks, `ID(5)` in the `GotoNode` at the
    # end of the block will refer to the wrong block if the block original associated to
    # `ID(5)` was itself split, since the "top" of that block will have a new `ID`.
    #
    # To resolve this, we:
    # 1. Associate an ID to each split in each block, ensuring that the ID for the final
    #   split of each block is the same ID as that of the original block.
    all_split_ids = map(zip(ir.blocks, all_splits)) do (block, splits)
        return vcat([ID() for _ in splits[1:(end - 1)]], block.id)
    end

    # 2. Construct a map between the ID of each block and the ID associated to its split.
    top_split_id_map = Dict{ID,ID}(b.id => x[1] for (b, x) in zip(ir.blocks, all_split_ids))

    # 3. Update all `GotoNode`s and `GotoIfNot`s to refer to these new names.
    for block in ir.blocks
        t = terminator(block)
        if t isa IDGotoNode
            block.insts[end] = new_inst(IDGotoNode(top_split_id_map[t.label]))
        elseif t isa IDGotoIfNot
            block.insts[end] = new_inst(IDGotoIfNot(t.cond, top_split_id_map[t.dest]))
        end
    end

    # A set of blocks from which we might wish to resume computation.
    resume_block_ids = Vector{ID}()

    # A list onto which we'll push the type of any statement which might produce.
    possible_produce_types = Any[]

    # This where most of the action happens.
    #
    # For each split of each block, we must
    # 1. translate all statements which accept any SSAs as arguments, or return a value,
    #   into statements which read in data from the `Ref`s containing the value associated
    #   to each SSA, and write the result to `Ref`s associated to the SSA of the line in
    #   question.
    # 2. add additional code at the end of the split to handle the possibility that the
    #   last statement produces (per the definition of the splits above). This applies to
    #   all splits except the last, which cannot produce by construction. Exactly what
    #   happens here depends on whether the last statement is a `produce` call, or a
    #   call-which-might-produce -- see below for specifics.
    #
    # This code transforms each block (and its splits) into a new collection of blocks.
    # Note that the total number of new blocks may be greater than the total number of
    # splits, because each split ending in a call-which-might-produce requires more than a
    # single block to implement the required resumption functionality.
    new_bblocks = map(zip(ir.blocks, all_splits, all_split_ids)) do (bb, splits, splits_ids)
        new_blocks = map(enumerate(splits)) do (n, split)
            # We'll push ID-NewInstruction pairs to this as we proceed through the split.
            inst_pairs = IDInstPair[]

            # PhiNodes:
            #
            # A single `PhiNode`
            #
            #   ID(%1) = φ(ID(#1) => 1, ID(#2) => ID(%n))
            #
            # sets `ID(%1)` to either `1` or whatever value is currently associated to
            # `ID(%n)`, depending upon whether the predecessor block was `ID(#1)` or
            # `ID(#2)`. Consequently, a single `PhiNode` can be transformed into something
            # along the lines of:
            #
            #   ID(%1) = φ(ID(#1) => 1, ID(#2) => TupleRef(ref_ind_for_ID(%n)))
            #   ID(%2) = deref_phi(refs, ID(%1))
            #            set_ref_at!(refs, ref_ind_for_ID(%1), ID(%2))
            #
            # where `deref_phi` retrieves the value in position `ref_ind_for_ID(%n)` if
            # ID(%1) is a `TupleRef`, and `1` otherwise, and `set_ref_at!` sets the `Ref`
            # at position `ref_ind_for_ID(%1)` to the value of `ID(%2)`. See the actual
            # implementations below.
            #
            # If we have multiple `PhiNode`s at the start of a block, we must run all of
            # them, then dereference all of their values, and finally write all of the
            # de-referenced values to the appropriate locations. This is because
            # a. we require all `PhiNode`s appear together at the top of a given basic
            #   block, and
            # b. the semantics of `PhiNode`s is that they are all "run" simultaneously. This
            #   only matters if one `PhiNode` in the block can refer to the value stored in
            #   the SSA associated to another. For example, something along the lines of:
            #
            #       ID(%1) = φ(ID(#1) => 1, ID(#2) => ID(%2))
            #       ID(%2) = φ(ID(#1) => 1, ID(#2) => 2)
            #
            #   (we leave it as an exercise for the reader to figure out why this particular
            #   semantic feature of `PhiNode`s is relevant in this specific case).
            #
            # So, in general, the code produced by this block will look roughly like
            #
            #   ID(%1) = φ(...)
            #   ID(%2) = φ(...)
            #   ID(%3) = φ(...)
            #   ID(%4) = deref_phi(refs, ID(%1))
            #   ID(%5) = deref_phi(refs, ID(%2))
            #   ID(%6) = deref_phi(refs, ID(%3))
            #            set_ref_at!(refs, ref_ind_for_ID(%1), ID(%4))
            #            set_ref_at!(refs, ref_ind_for_ID(%2), ID(%5))
            #            set_ref_at!(refs, ref_ind_for_ID(%3), ID(%6))
            if n == 1
                # Find all PhiNodes in the block -- will definitely be in this split.
                phi_inds = findall(x -> x.stmt isa IDPhiNode, bb.insts)

                # Replace SSA IDs with `TupleRef`s, and record these instructions.
                phi_ids = map(phi_inds) do n
                    phi = bb.insts[n].stmt
                    for i in eachindex(phi.values)
                        isassigned(phi.values, i) || continue
                        v = phi.values[i]
                        v isa ID || continue
                        phi.values[i] = TupleRef(ssa_id_to_ref_index_map[v])
                    end
                    phi_id = ID()
                    push!(inst_pairs, (phi_id, new_inst(phi, Any)))
                    return phi_id
                end

                # De-reference values associated to `IDPhiNode`s.
                deref_ids = map(phi_inds) do n
                    id = bb.inst_ids[n]
                    phi_id = phi_ids[n]
                    push!(
                        inst_pairs,
                        (id, new_inst(Expr(:call, deref_phi, refs_id, phi_id))),
                    )
                    return id
                end

                # Update values stored in `Ref`s associated to `PhiNode`s.
                for n in phi_inds
                    ref_ind = ssa_id_to_ref_index_map[bb.inst_ids[n]]
                    expr = Expr(:call, set_ref_at!, refs_id, ref_ind, deref_ids[n])
                    push!(inst_pairs, (ID(), new_inst(expr)))
                end
            end

            # Statements which do not produce:
            #
            # Iterate every statement in the split other than the final one, replacing uses
            # of SSAs with de-referenced `Ref`s, and writing the results of statements to
            # the corresponding `Ref`s.
            _ids = view(bb.inst_ids, (split.start):(split.last - 1))
            _insts = view(bb.insts, (split.start):(split.last - 1))
            for (id, inst) in zip(_ids, _insts)
                stmt = inst.stmt
                if Meta.isexpr(stmt, :invoke) ||
                    Meta.isexpr(stmt, :call) ||
                    Meta.isexpr(stmt, :new) ||
                    Meta.isexpr(stmt, :foreigncall)

                    # Find any `ID`s and replace them with calls to read whatever is stored
                    # in the `Ref`s that they are associated to.
                    for (n, arg) in enumerate(stmt.args)
                        arg isa ID || continue

                        new_id = ID()
                        ref_ind = ssa_id_to_ref_index_map[arg]
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (new_id, new_inst(expr)))
                        stmt.args[n] = new_id
                    end

                    # Push the target instruction to the list.
                    push!(inst_pairs, (id, inst))

                    # If we know it is not possible for this statement to contain any calls
                    # to produce, then simply write out the result to its `Ref`. If it is
                    # never used, then there is no need to store it.
                    if is_used_dict[id]
                        out_ind = ssa_id_to_ref_index_map[id]
                        set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, id)
                        push!(inst_pairs, (ID(), new_inst(set_ref)))
                    end
                elseif Meta.isexpr(stmt, :boundscheck)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :code_coverage_effect)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :gc_preserve_begin)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :gc_preserve_end)
                    push!(inst_pairs, (id, inst))
                elseif Meta.isexpr(stmt, :throw_undef_if_not)
                    push!(inst_pairs, (id, inst))
                elseif stmt isa Nothing
                    push!(inst_pairs, (id, inst))
                elseif stmt isa GlobalRef
                    ref_ind = ssa_id_to_ref_index_map[id]
                    expr = Expr(:call, set_ref_at!, refs_id, ref_ind, stmt)
                    push!(inst_pairs, (id, new_inst(expr)))
                elseif stmt isa Core.PiNode
                    if stmt.val isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.val]
                        val_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (val_id, new_inst(expr)))
                        push!(inst_pairs, (id, new_inst(Core.PiNode(val_id, stmt.typ))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                    set_ind = ssa_id_to_ref_index_map[id]
                    set_expr = Expr(:call, set_ref_at!, refs_id, set_ind, id)
                    push!(inst_pairs, (ID(), new_inst(set_expr)))
                elseif stmt isa IDPhiNode
                    # do nothing -- we've already handled any `PhiNode`s.
                elseif Meta.isexpr(stmt, :loopinfo)
                    push!(inst_pairs, (id, inst))
                else
                    throw(error("Unhandled stmt $stmt of type $(typeof(stmt))"))
                end
            end

            # TODO: explain this better.
            new_blocks = BBlock[]

            # Produce and Terminators:
            #
            # Handle the last statement in the split.
            id = bb.inst_ids[split.last]
            inst = bb.insts[split.last]
            stmt = inst.stmt
            if n == length(splits)
                # This is the last split in the block, so it must end with a non-producing
                # terminator. We handle this in a similar way to the statements above.

                if stmt isa ReturnNode
                    # Reset the position counter to `-1`, so that if this function gets
                    # called again, execution starts from the beginning.
                    expr = Expr(:call, set_resume_block!, refs_id, Int32(-1))
                    push!(inst_pairs, (ID(), new_inst(expr)))
                    # If returning an SSA, it might be one whose value was restored from
                    # before. Therefore, grab it out of storage, rather than assuming that
                    # it is def-ed.
                    if isdefined(stmt, :val) && stmt.val isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.val]
                        val_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (val_id, new_inst(expr)))
                        push!(inst_pairs, (ID(), new_inst(ReturnNode(val_id))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                elseif stmt isa IDGotoIfNot
                    # If the condition is an SSA, it might be one whose value was restored
                    # from before. Therefore, grab it out of storage, rather than assuming
                    # that it is defined.
                    if stmt.cond isa ID
                        ref_ind = ssa_id_to_ref_index_map[stmt.cond]
                        cond_id = ID()
                        expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                        push!(inst_pairs, (cond_id, new_inst(expr)))
                        push!(inst_pairs, (ID(), new_inst(IDGotoIfNot(cond_id, stmt.dest))))
                    else
                        push!(inst_pairs, (id, inst))
                    end
                elseif stmt isa IDGotoNode
                    push!(inst_pairs, (id, inst))
                else
                    error("Unexpected terminator $stmt")
                end
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            elseif is_produce_stmt(stmt)
                # This is a statement of the form
                # %n = produce(arg)
                #
                # We transform this into
                # Libtask.set_resume_block!(refs_id, id_of_next_block)
                # return ProducedValue(arg)
                #
                # The point is to ensure that, next time that this `TapedTask` is called,
                # computation is resumed from the statement _after_ this produce statement,
                # and to return whatever this produce statement returns.

                # Log the result type of this statement.
                arg = stmt.args[Meta.isexpr(stmt, :invoke) ? 3 : 2]
                push!(possible_produce_types, get_type((ir.argtypes, id_to_type_map), arg))

                # When this TapedTask is next called, we should resume from the first
                # statement of the next split.
                resume_id = splits_ids[n + 1]
                push!(resume_block_ids, resume_id)

                # Insert statement to enforce correct resumption behaviour.
                resume_stmt = Expr(:call, set_resume_block!, refs_id, resume_id.id)
                push!(inst_pairs, (ID(), new_inst(resume_stmt)))

                # Insert statement to construct a `ProducedValue` from the value.
                # Could be that the produce references an SSA, in which case we need to
                # de-reference, rather than just return the thing.
                prod_val = produce_value(stmt)
                if prod_val isa ID
                    deref_id = ID()
                    ref_ind = ssa_id_to_ref_index_map[prod_val]
                    expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                    push!(inst_pairs, (deref_id, new_inst(expr)))
                    prod_val = deref_id
                end

                # Set the ref for this statement, as we would for any other call or invoke.
                # The TapedTask may need to read this ref when it resumes, if the return
                # value of `produce` is used within the original function.
                if is_used_dict[id]
                    out_ind = ssa_id_to_ref_index_map[id]
                    set_ref = Expr(:call, set_ref_at!, refs_id, out_ind, prod_val)
                    push!(inst_pairs, (ID(), new_inst(set_ref)))
                end

                # Construct a `ProducedValue`.
                val_id = ID()
                push!(inst_pairs, (val_id, new_inst(Expr(:call, ProducedValue, prod_val))))

                # Insert statement to return the `ProducedValue`.
                push!(inst_pairs, (ID(), new_inst(ReturnNode(val_id))))

                # Construct a single new basic block from all of the inst-pairs.
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))
            else
                # The final statement is one which might produce, but is not itself a
                # `produce` statement. For example
                # y = f(x)
                #
                # becomes (morally speaking)
                # y = f(x)
                # if y isa ProducedValue
                #   set_resume_block!(refs_id, id_of_current_block)
                #   return y
                # end
                #
                # The point is to ensure that, if `f` "produces" (as indicated by `y` being
                # a `ProducedValue`) then the next time that this TapedTask is called, we
                # must resume from the call to `f`, as subsequent runs might also produce.
                # On the other hand, if anything other than a `ProducedValue` is returned,
                # we know that `f` has nothing else to produce, and execution can safely
                # continue to the next split.
                # In addition to the above, we must do the usual thing and ensure that any
                # ssas are read from storage, and write the result of this computation to
                # storage before continuing to the next instruction.
                #
                # You should look at the IR generated by a simple example in the test suite
                # which involves calls that might produce, in order to get a sense of what
                # the resulting code looks like prior to digging into the code below.

                # At present, we're not able to properly infer the values which might
                # potentially be produced by a call-which-might-produce. Consequently, we
                # have to assume they can produce anything.
                #
                # This `Any` only affects the return type of the function being derived
                # here. Importantly, it does not affect the type stability of subsequent
                # statements in this function. As a result, the impact ought to be
                # reasonably limited.
                push!(possible_produce_types, Any)

                # Create a new basic block from the existing statements, since all new
                # statement need to live in their own basic blocks.
                callable_block_id = ID()
                push!(inst_pairs, (ID(), new_inst(IDGotoNode(callable_block_id))))
                push!(new_blocks, BBlock(splits_ids[n], inst_pairs))

                # Derive TapedTask for this statement.
                (callable, callable_args) = if Meta.isexpr(stmt, :invoke)
                    @static if VERSION >= v"1.12-"
                        # On Julia 1.12 stmt.args has CodeInstances rather than
                        # MethodInstances. We use .def to get the MethodInstances.
                        sig = stmt.args[1].def.specTypes
                    else
                        sig = stmt.args[1].specTypes
                    end
                    v = Any[Any]
                    (LazyCallable{sig,callable_ret_type(sig, v)}(), stmt.args[2:end])
                elseif Meta.isexpr(stmt, :call)
                    (DynamicCallable(), stmt.args)
                else
                    display(stmt)
                    println()
                    error("unhandled statement which might produce $stmt")
                end

                # Find any `ID`s and replace them with calls to read whatever is stored
                # in the `Ref`s that they are associated to.
                callable_inst_pairs = IDInstPair[]
                for (n, arg) in enumerate(callable_args)
                    arg isa ID || continue

                    new_id = ID()
                    ref_ind = ssa_id_to_ref_index_map[arg]
                    expr = Expr(:call, get_ref_at, refs_id, ref_ind)
                    push!(callable_inst_pairs, (new_id, new_inst(expr)))
                    callable_args[n] = new_id
                end

                # Allocate a slot in the _refs vector for this callable.
                push!(_refs, Ref(callable))
                callable_ind = length(_refs)

                # Retrieve the callable from the refs.
                callable_id = ID()
                callable_stmt = Expr(:call, get_ref_at, refs_id, callable_ind)
                push!(callable_inst_pairs, (callable_id, new_inst(callable_stmt)))

                # Call the callable.
                result_id = ID()
                result_stmt = Expr(:call, callable_id, callable_args...)
                push!(callable_inst_pairs, (result_id, new_inst(result_stmt)))

                # Determine whether this TapedTask has produced a not-a-`ProducedValue`.
                not_produced_id = ID()
                not_produced_stmt = Expr(:call, not_a_produced, result_id)
                push!(callable_inst_pairs, (not_produced_id, new_inst(not_produced_stmt)))

                # Go to a block which just returns the `ProducedValue`, if a
                # `ProducedValue` is returned, otherwise continue to the next split.
                is_produced_block_id = ID()
                is_not_produced_block_id = ID()
                switch = Switch(
                    Any[not_produced_id],
                    [is_produced_block_id],
                    is_not_produced_block_id,
                )
                push!(callable_inst_pairs, (ID(), new_inst(switch)))

                # Push the above statements onto a new block.
                push!(new_blocks, BBlock(callable_block_id, callable_inst_pairs))

                # Construct block which handles the case that we got a `ProducedValue`. If
                # this happens, it means that `callable` has more things to produce still.
                # This means that we need to call it again next time we enter this function.
                # To achieve this, we set the resume block to the `callable_block_id`,
                # and return the `ProducedValue` currently located in `result_id`.
                push!(resume_block_ids, callable_block_id)
                set_res = Expr(:call, set_resume_block!, refs_id, callable_block_id.id)
                return_id = ID()
                produced_block_inst_pairs = IDInstPair[
                    (ID(), new_inst(set_res)),
                    (return_id, new_inst(ReturnNode(result_id))),
                ]
                push!(new_blocks, BBlock(is_produced_block_id, produced_block_inst_pairs))

                # Construct block which handles the case that we did not get a
                # `ProducedValue`. In this case, we must first push the result to the `Ref`
                # associated to the call, and goto the next split.
                next_block_id = splits_ids[n + 1] # safe since the last split ends with a terminator
                if is_used_dict[id]
                    result_ref_ind = ssa_id_to_ref_index_map[id]
                    set_ref = Expr(:call, set_ref_at!, refs_id, result_ref_ind, result_id)
                else
                    set_ref = nothing
                end
                not_produced_block_inst_pairs = IDInstPair[
                    (ID(), new_inst(set_ref))
                    (ID(), new_inst(IDGotoNode(next_block_id)))
                ]
                push!(
                    new_blocks,
                    BBlock(is_not_produced_block_id, not_produced_block_inst_pairs),
                )
            end
            return new_blocks
        end
        return reduce(vcat, new_blocks)
    end
    new_bblocks = reduce(vcat, new_bblocks)

    # Insert statements at the top.
    cases = map(resume_block_ids) do id
        return ID(), id, Expr(:call, resume_block_is, refs_id, id.id)
    end
    cond_ids = ID[x[1] for x in cases]
    cond_dests = ID[x[2] for x in cases]
    cond_stmts = Any[x[3] for x in cases]
    switch_stmt = Switch(Any[x for x in cond_ids], cond_dests, first(new_bblocks).id)
    entry_stmts = vcat(cond_stmts, nothing, switch_stmt)
    entry_block = BBlock(ID(), vcat(cond_ids, ID(), ID()), map(new_inst, entry_stmts))
    new_bblocks = vcat(entry_block, new_bblocks)

    # New argtypes are the same as the old ones, except we have `Ref`s in the first argument
    # rather than nothing at all.
    new_argtypes = copy(ir.argtypes)
    refs = (_refs..., Ref{Int32}(-1))
    new_argtypes = vcat(typeof(refs), copy(ir.argtypes))

    # Return BBCode and the `Ref`s.
    @static if VERSION >= v"1.12-"
        new_ir = BBCode(
            new_bblocks, new_argtypes, ir.sptypes, ir.debuginfo, ir.meta, ir.valid_worlds
        )
    else
        new_ir = BBCode(new_bblocks, new_argtypes, ir.sptypes, ir.linetables, ir.meta)
    end
    return new_ir, refs, possible_produce_types
end

# Helper used in `derive_copyable_task_ir`.
@inline get_ref_at(refs::R, n::Int) where {R<:Tuple} = refs[n][]

# Helper used in `derive_copyable_task_ir`.
@inline function set_ref_at!(refs::R, n::Int, val) where {R<:Tuple}
    refs[n][] = val
    return nothing
end

# Helper used in `derive_copyable_task_ir`.
@inline function set_resume_block!(refs::R, id::Int32) where {R<:Tuple}
    refs[end][] = id
    return nothing
end

# Helper used in `derive_copyable_task_ir`.
@inline resume_block_is(refs::R, id::Int32) where {R<:Tuple} = !(refs[end][] === id)

# Helper used in `derive_copyable_task_ir`.
@inline deref_phi(refs::R, n::TupleRef) where {R<:Tuple} = refs[n.n][]
@inline deref_phi(::R, x) where {R<:Tuple} = x

# Helper used in `derived_copyable_task_ir`.
@inline not_a_produced(x) = !(isa(x, ProducedValue))

# Implement iterator interface.
function Base.iterate(t::TapedTask, state::Nothing=nothing)
    v = consume(t)
    return v === nothing ? nothing : (v, nothing)
end
Base.IteratorSize(::Type{<:TapedTask}) = Base.SizeUnknown()
Base.IteratorEltype(::Type{<:TapedTask}) = Base.EltypeUnknown()

"""
    LazyCallable

Used to implement static dispatch, while avoiding the need to construct the
callable immediately. When constructed, just stores the signature of the
callable and its return type. Constructs the callable when first called.

All type information is known, so it is possible to make this callable type
stable provided that the return type is concrete.
"""
mutable struct LazyCallable{sig<:Tuple,Tret}
    mc::MistyClosure
    position::Base.RefValue{Int32}
    LazyCallable{sig,Tret}() where {sig,Tret} = new{sig,Tret}()
end

function (l::LazyCallable)(args::Vararg{Any,N}) where {N}
    isdefined(l, :mc) || construct_callable!(l)
    return l.mc(args...)
end

function construct_callable!(l::LazyCallable{sig}) where {sig}
    mc, pos = build_callable(sig)
    l.mc = mc
    l.position = pos
    return nothing
end

"""
    DynamicCallable

Like [`LazyCallable`](@ref), but without any type information. Used to implement
dynamic dispatch.
"""
mutable struct DynamicCallable{V}
    cache::V
end

DynamicCallable() = DynamicCallable(Dict{Any,Any}())

function (dynamic_callable::DynamicCallable)(args::Vararg{Any,N}) where {N}
    sig = _typeof(args)
    callable = get(dynamic_callable.cache, sig, nothing)
    if callable === nothing
        callable = build_callable(sig)
        dynamic_callable.cache[sig] = callable
    end
    return callable[1](args...)
end
