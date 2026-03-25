"""
    Libtask.NotInTapedTaskError <: Exception

This error is thrown when attempting to call `Libtask.get_taped_globals(::Type{T}) where
{T}` from outside of a `TapedTask`.

Note that the other method, `Libtask.get_taped_globals(t::TapedTask)`, can be called from
outside a `TapedTask`.
"""
struct NotInTapedTaskError <: Exception end
function Base.showerror(io::IO, ::NotInTapedTaskError)
    return print(
        io,
        "Libtask.NotInTapedTaskError: The method `get_taped_globals(::Type)` can only be called from inside a TapedTask.",
    )
end

const TASK_VARIABLE_KEY = :task_variable

"""
    get_taped_globals(T::Type)

When this method is called from **inside a `TapedTask`**, this will return whatever is
contained in its `taped_globals` field.

The type `T` is required for optimal performance. If you know that the result of this
operation must return a specific type, specify `T`. If you do not know what type it will
return, pass `Any` -- this will typically yield type instabilities, but will run correctly.
"""
@noinline function get_taped_globals(::Type{T}) where {T}
    # This function is `@noinline`d to ensure that the type-unstable items in here do not
    # appear in a calling function, and cause allocations.
    #
    # The return type of `task_local_storage(TASK_VARIABLE_KEY)` is `Any`. To ensure that
    # this type instability does not propagate through the rest of the code, we `typeassert`
    # the result to be `T`. By doing this, callers of this function will (hopefully) think
    # carefully about how they can figure out what type they have put in global storage.
    tls = task_local_storage()
    if !haskey(tls, TASK_VARIABLE_KEY)
        throw(NotInTapedTaskError())
    end
    return typeassert(tls[TASK_VARIABLE_KEY], T)
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

function _throw_ir_error(@nospecialize(sig::Type{<:Tuple}))
    F = fieldtype(sig, 1)
    f_name = F <: Function ? nameof(F.instance) : F
    arg_types = fieldtypes(sig)[2:end]
    msg = """
        Failed to generate IR for signature $sig.

        This typically means that either:

        1. No method matches `$f_name` with argument types $arg_types.
           Check that you're passing the right number and types of arguments.
        2. Multiple methods match and the call is ambiguous.\n"""
    throw(ArgumentError(msg))
end

"""
    generate_ir(stage::Symbol, f, args...; kwargs...)

Returns an IR output for the call `f(args...; kwargs...)`.

`stage` controls the form of the IR that is returned, and corresponds to each stage in the
generation of a `TapedTask`. The options are:

- `:input_ir` - no transformations applied, this generates a `Core.Compiler.IRCode`
  corresponding to the original function call.

- `:input_bb`: same as `:input_ir` but converted to `Libtask.BasicBlock.BBCode`

- `:transformed_bb`: same as `:input_bb` but with transformations applied to convert it
  into a form that supports the `produce`-`consume` interface.

- `:optimised_bb`: same as `:transformed_bb` but with Libtask optimisations to reduce the
  number of stored references

- `:optimised_ir`: same as `:optimised_bb` but converted back to `Core.Compiler.IRCode`

- `:final`: same as `:optimised_ir` but with Julia's builtin SSA IR optimisations applied
  to it. This is the IRCode that is eventually wrapped in the `MistyClosure`.

This is intended purely as a debugging tool, and is not exported. Breaking changes to the
interface may occur at any time.
"""
function generate_ir(stage::Symbol, fargs...; kwargs...)
    all_args = isempty(kwargs) ? fargs : (Core.kwcall, getfield(kwargs, :data), fargs...)
    sig = typeof(all_args)
    ir_results = Base.code_ircode_by_type(sig)
    if isempty(ir_results)
        _throw_ir_error(sig)
    end
    original_ir = ir_results[1][1]
    stage == :input_ir && return original_ir
    seed_id!()
    original_bb = BBCode(original_ir)
    stage == :input_bb && return original_bb
    transformed_bb, refs, _ = derive_copyable_task_ir(BBCode(original_ir))
    stage == :transformed_bb && return transformed_bb
    topt_bb, refs = eliminate_refs(transformed_bb, refs)
    stage == :optimised_bb && return topt_bb
    transformed_ir = IRCode(topt_bb)
    stage == :optimised_ir && return transformed_ir
    optimise_ir!(transformed_ir)
    stage == :final && return transformed_ir
    throw(ArgumentError("unknown stage $stage"))
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
    world_age = Base.get_world_counter()
    key = CacheKey(world_age, sig)
    if haskey(mc_cache, key)
        return fresh_copy(mc_cache[key])
    else
        ir_results = Base.code_ircode_by_type(sig)
        if isempty(ir_results)
            _throw_ir_error(sig)
        end
        ir = ir_results[1][1]
        # Check whether this is a varargs call.
        isva = which(sig).isva
        bb, refs, types = derive_copyable_task_ir(BBCode(ir))
        bb, refs = eliminate_refs(bb, refs)
        unoptimised_ir = IRCode(bb)
        @static if VERSION > v"1.12-"
            # This is a performance optimisation, copied over from Mooncake, where setting
            # the valid world age to be very strictly just the current age allows the
            # compiler to do more inlining and other optimisation.
            unoptimised_ir = set_valid_world!(unoptimised_ir, world_age)
        end
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

# A cache for holding MistyClosures, keyed by CacheKey.
# The reason to have a type for this, rather than just a global const
# Dict{CacheKey,MistyClosure}, is thread-safety. The global cache is written to when new
# TapedTasks are compiled, and if multiple threads are doing this concurrently races might
# occur. This type uses a ReentrantLock to ensure that any `setindex!` operations are
# atomic, solving the problem.
struct GlobalMCCache
    cache::Dict{CacheKey,MistyClosure}
    lock::ReentrantLock

    GlobalMCCache() = new(Dict{CacheKey,MistyClosure}(), ReentrantLock())
end

Base.haskey(c::GlobalMCCache, key) = haskey(c.cache, key)
Base.getindex(c::GlobalMCCache, key) = getindex(c.cache, key)
Base.setindex!(c::GlobalMCCache, val, key) = @lock c.lock setindex!(c.cache, val, key)

const mc_cache = GlobalMCCache()

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
to a given [`Libtask.TapedTask`](@ref). These functions can be called from *outside* a
`TapedTask`, in order to get or set its taped globals.

However, [`Libtask.get_taped_globals`](@ref) can also be called from inside a `TapedTask`
itself:

```jldoctest sv
julia> function f()
           produce(get_taped_globals(Int))
           produce(get_taped_globals(Int))
           return nothing
       end
f (generic function with 1 method)
```

When constructing a [`Libtask.TapedTask`](@ref), the first argument is the value that
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
    @static if v"1.12.1" > VERSION >= v"1.12.0-"
        @warn """
            Libtask.jl does not work correctly on Julia v1.12.0 and may crash your Julia
             session. Please upgrade to at least v1.12.1. See
             https://github.com/JuliaLang/julia/issues/59222 for the bug in question.
            """
    end
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
    get_taped_globals(tt::TapedTask)

Extract the `taped_globals` field from a `TapedTask`.

This is the same as the value that will be returned by [`get_taped_globals`](@ref) when
called from *inside* `tt`. However, this method can be called from outside `tt`.

See also [`set_taped_globals!`](@ref).
"""
get_taped_globals(t::TapedTask) = t.taped_globals

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
    task_local_storage(TASK_VARIABLE_KEY, t.taped_globals)
    v = t.mc.oc(t.fargs...)
    return v isa ProducedValue ? v[] : nothing
end

"""
    might_produce(sig::Type{<:Tuple})::Bool

`true` if a call to method with signature `sig` is permitted to contain
`Libtask.produce` statements.

This is an opt-in mechanism. The fallback method of this function returns `false` indicating
that, by default, we assume that calls do not contain `Libtask.produce` statements.
"""
might_produce(::Type{<:Tuple}) = false

"""
    might_produce_if_sig_contains(::Type{T})::Bool

Mark *any* method as being able to `produce` if `T` is found anywhere in its signature.

Note that if `T` is an abstract type, you will have to use
`might_produce_if_sig_contains(::Type{<:T})` to mark methods which have subtypes of `T` in
their signature as being able to `produce`.

For example, if `might_produce_if_sig_contains(::Type{<:AbstractFoo}) = true`, then any
method that takes an argument of `Foo <: AbstractFoo` will be treated as having
`might_produce = true`.

!!! warning
    This method should be used with caution, as it is a very broad brush.
    It is only really intended for use with Turing.jl.
"""
might_produce_if_sig_contains(::Type) = false
might_produce_if_sig_contains(::typeof(Vararg)) = false

"""
    @might_produce(f)

If `f` is a function that may call `Libtask.produce` inside it, then `@might_produce(f)`
will generate the appropriate methods needed to ensure that `Libtask.might_produce` returns
`true` for **all** relevant signatures of `f`. This works even if `f` has methods with
keyword arguments.

!!! note
    Because `@might_produce f` is applied to all possible signatures, there are performance
    penalties associated with marking all methods of `f` as produceable if only one method
    can actually call `produce`. If performance is critical, please use the
    [`might_produce`](@ref) function directly.

```jldoctest might_produce_macro
julia> # For this demonstration we need to mark `g` as not being inlineable.
       @noinline function g(x; y, z=0)
           produce(x + y + z)
       end
g (generic function with 1 method)

julia> function f()
           g(1; y=2, z=3)
       end
f (generic function with 1 method)

julia> # This returns nothing because `g` isn't yet marked as being able to `produce`.
       consume(Libtask.TapedTask(nothing, f))

julia> Libtask.@might_produce(g)

julia> # Now it works!
       consume(Libtask.TapedTask(nothing, f))
6
```
"""
macro might_produce(f)
    # See https://github.com/TuringLang/Libtask.jl/issues/197 for discussion of this macro.
    quote
        function $(Libtask).might_produce(::Type{<:Tuple{typeof($(esc(f))),Vararg}})
            return true
        end
        possible_n_kwargs = unique(map(length ∘ Base.kwarg_decl, methods($(esc(f)))))
        if possible_n_kwargs != [0]
            # Oddly we need to interpolate the module and not the function: either
            # `$(might_produce)` or $(Libtask.might_produce) seem more natural but both of
            # those cause the entire `Libtask.might_produce` to be treated as a single
            # symbol. See https://discourse.julialang.org/t/128613
            function $(Libtask).might_produce(
                ::Type{<:Tuple{typeof(Core.kwcall),<:NamedTuple,typeof($(esc(f))),Vararg}}
            )
                return true
            end
            for n in possible_n_kwargs
                # We only need `Any` and not `<:Any` because tuples are covariant.
                kwarg_types = fill(Any, n)
                function $(Libtask).might_produce(
                    ::Type{<:Tuple{<:Function,kwarg_types...,typeof($(esc(f))),Vararg}}
                )
                    return true
                end
            end
        end
    end
end
