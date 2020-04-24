using IRTools
using MacroTools

"""
    copye_object(obj)

Copy an object and mark it as the current task's asset (i.e., the
current task will own that copy).

The logic is:

- If `obj` is already in `_cow_asset`, do nothing because we had
  already copied it.
- If `object_id(obj)` is in `_cow_copylog`, it reflects that the
  `obj` had been copied, and the copy is in `_cow_asset` now, but
  we there are many objects in `_cow_asset` so that we can tell
  which object is the copy of `obj`. In this situation, we can find
  the copy using `task.storage[:_cow_copylog][objectid(obj)]`.

  ```julia
  data = []
  f1(data) # will modify data, so a copy is made.
  f2(data) # will modify data, and find the copy made by f1 in copy log.
  ```
- Else, we copy the object, register it to the asset and copy log.

"""
function copy_object(obj)
    ct = _current_task()
    ct.storage === nothing && (ct.storage = IdDict())
    if !haskey(ct.storage, :_cow_asset)
        ct.storage[:_cow_asset] = IdDict{Any, Bool}()
    end
    if !haskey(ct.storage, :_cow_copylog)
        ct.storage[:_cow_copylog] = Dict{UInt64, Any}()
    end

    haskey(ct.storage[:_cow_asset], obj) && (return obj) # own this obj
    if haskey(ct.storage[:_cow_copylog], objectid(obj)) # had copied this obj
        ret = ct.storage[:_cow_copylog][objectid(obj)]
        ct.storage[:_cow_asset][ret] = true
        return ret
    end

    new_obj = deepcopy(obj)
    ct.storage[:_cow_asset][new_obj] = true
    ct.storage[:_cow_copylog][objectid(obj)] = new_obj
    return new_obj
end

"""
    copy_for_reading(obj)

If current task owns a copy of `obj`, returns the copy, else returns
obj itself.

"""
function copy_for_reading(obj)
    ct = _current_task()
    ct.storage === nothing && return obj
    haskey(ct.storage, :_cow_asset) || return obj
    haskey(ct.storage, :_cow_copylog) || return obj

    haskey(ct.storage[:_cow_asset], obj) && (return obj) # own this obj
    if haskey(ct.storage[:_cow_copylog], objectid(obj)) # had copied this obj
        ret = ct.storage[:_cow_copylog][objectid(obj)]
        ct.storage[:_cow_asset][ret] = true
        return ret
    end
    return obj
end

"""
    maybe_copy(x)

Copy an object when the COW mechanism requires to make a copy. If you
want objects of a certain type to be copied then, add a method to this
function:

```julia
Libtask.maybe_copy(x::MyType) = Libtask.copy_object(x)
```

"""
maybe_copy(x) = x
maybe_copy(x::AbstractArray{<:Number}) = copy_object(x)
maybe_copy(x::AbstractDict) = begin
    ct = _current_task()
    if x === ct.storage
        return x
    end
    copy_object(x)
end

# (func_name => mutated_arg_pos)
const MUTATING_OPS = Dict{Any, Vector{Int}}(
    :setindex! => [1],
    :push! => [1], :pushfirst! => [1],
    :pop! => [1], :popfirst! => [1],
    :append! => [1],
    :delete! => [1], :deleteat! => [1],
    :setdiff! => [1],
)

"""
    mutating(e)

Predicate if an expression is a mutating operation.
If true, returns `ture` and the position of the mutated argument;
else returns `(false, [0])`.

Examples:

- `push!(a, v)` -> (true, [1])
- `print(a)` -> (false, [0])

"""
function mutating(e)
    isa(e, Expr) || return (false, [0])
    e.head == :call || return (false, [0])
    (args, offset) = if e.args[1] === IRTools.self
        (e.args[2:end], 1)
    else
        (e.args[1:end], 0)
    end

    #@show args[1]

    if isa(args[1], GlobalRef)
        if haskey(MUTATING_OPS, args[1])
            return (true, MUTATING_OPS[args[1]] .+ offset)
        elseif haskey(MUTATING_OPS, args[1].name)
            return (true, MUTATING_OPS[args[1].name] .+ offset)
        end
    end

    if isa(args[1], IRTools.Variable) # call to a closure
        # @show "closure called!"
        idx = collect(1:length(args) - 1)
        return (true, idx .+ offset)
    end
    return (false, [0])
end


"""
    find_mutated_vars(ir::IRTools.IR)

Find all variables that are mutated in the given IR.

"""
function find_mutated_vars(ir::IRTools.IR)
    vars = Vector{IRTools.Variable}()
    for (v, st) in ir
        mut, mpositions = mutating(st.expr)
        mut || continue
        for mpos in mpositions
            # mpos = 1 when push!(...), mpos = 2 when self(push!, ...)
            isa(st.expr.args[mpos + 1], IRTools.Variable) || continue
            mv = st.expr.args[mpos + 1]
            push!(vars, mv)
        end
    end

    return vars
end

"""
    insert_copy_for_var(ir::IRTools.IR, var)

For a given variable:
- find the expressions mutate it, insert a `maybe_copy` before them;
- find the expressions read it, insert a `copy_for_reading` before them.

"""
function insert_copy_for_var(ir::IRTools.IR, var)
    arg = var
    for (v, st) in ir
        isa(st.expr, Expr) || continue
        (var in st.expr.args) || continue
        mut, mpositions = mutating(st.expr)
        if mut # write
            rk = insert!(ir, v, IRTools.xcall(Libtask, :maybe_copy, arg))
        else # read
            rk = insert!(ir, v, IRTools.xcall(Libtask, :copy_for_reading, arg))
        end
        for i in 1:length(st.expr.args)
            st.expr.args[i] == var && (st.expr.args[i] = rk)
        end
        arg = rk
    end

    return ir
end


"""
    insert_copy_stage_2(ir::IR)

For every muated variable in the given IR, do `insert_copy_for_var`.

"""
function insert_copy_stage_2(ir::IRTools.IR)
    mutated_vars = find_mutated_vars(ir)
    for var in mutated_vars
        insert_copy_for_var(ir, var)
    end
    return ir
end


"""
    insert_copy_stage_1(ir::IR)

If a variable is mutated in an IR, we find its first occurrence, then
insert a `maybe_copy` after that occurrence.
This is for handle read operations after write opertions.

"""
function insert_copy_stage_1(ir::IRTools.IR)
    mutate_vars = Dict{IRTools.Variable, Bool}()
    replacements = Dict{IRTools.Variable, IRTools.Variable}()

    for (v, st) in ir
        mut, mpositions = mutating(st.expr)
        mut || continue
        for mpos in mpositions
            # mpos = 1 when push!(...), mpos = 2 when self(push!, ...)
            if isa(st.expr.args[mpos + 1], IRTools.Variable)
                mutate_vars[st.expr.args[mpos + 1]] = true
            end
        end
    end

    for (mv, _) in mutate_vars
        rk = IRTools.insertafter!(ir, mv, :(_placeholder()))
        replacements[mv] = rk
    end

    ir_new = MacroTools.prewalk(ir) do x
        if (x isa IRTools.Variable) && haskey(replacements, x)
            return replacements[x]
        end
        return x
    end

    for (mv, newk) in replacements
        ir_new[newk] = IRTools.xcall(Libtask, :maybe_copy, mv)
    end

    return ir_new
end

insert_copy(ir) = ir |> insert_copy_stage_1 |> insert_copy_stage_2

"""
    check_func_mutating(func::GlobalRef, ir::IRTools.IR)

Check if a function mutates its arguments, and if it does, mutates
which ones.

"""

function check_func_mutating(func, ir::IRTools.IR)
    # @show "cf", ir
    vars = find_mutated_vars(ir)
    args = IRTools.arguments(ir)
    positions = [x for x in indexin(vars, args) if x !== nothing]
    isempty(positions) || (MUTATING_OPS[func] = positions .- 1)
end

function recurse_no_builtin!(ir, to = IRTools.self)
    for (x, st) in ir
        IRTools.isexpr(st.expr, :call) || continue
        func = st.expr.args[1]
        if isa(func, GlobalRef) && func.mod in (Base, Core)
            continue
        end
        ir[x] = Expr(:call, to, st.expr.args...)
    end
    return ir
end

IRTools.@dynamo function cow(a...)
    ir = IRTools.IR(a...)
    ir === nothing && return
    # a[1] is a
    # - GlobalRef for global + named function, or
    # - a subtype of Function for closure/anonymous function
    isa(a[1], GlobalRef) && check_func_mutating(a[1], ir)
    recurse_no_builtin!(ir)
    ir = insert_copy(ir)
    return ir
end


"""
    @non_cow_func

Make a function skip the COW mechanism.

```julia
@non_cow_func function f1()
    # ...
end
```
"""
macro non_cow_func(func)
    if isa(func, Symbol) || (isa(func, Expr) && func.head === :.)
        return :(cow(::typeof($(esc(func))), a...) = $(esc(func))(a...))
    end

    @capture(shortdef(func), (name_(args__) = body_) |
             (name_(args__) where {T__} = body_)) || error("Need a function definition")
    return quote
        $(esc(func))
        $(esc(:(Libtask.cow(::typeof($(name)), a...) = $(name)(a...))))
    end
end

"""
    @non_cow

Make an expression skip the COW mechanism.

```julia
@now_cow data[idx] = value
```
"""
macro non_cow(expr)
    quote
        f = () -> begin
            $(esc(expr))
        end
        Libtask.non_cow_call(f)
    end
end

@non_cow_func(produce)
@non_cow_func(consume)
@non_cow_func non_cow_call(func, args...) = func(args...)


# debug
function cmp_cow_ir(func, args...)
    ir_00 = IRTools.@code_ir func(args...)
    print("\nIR [Original] -------------\n")
    print(ir_00)
    ir_01 = insert_copy_stage_1(ir_00)
    print("\nIR [Stage 1]  -------------\n")
    print(ir_01)
    ir_12 = insert_copy_stage_2(ir_01)
    print("\nIR [Stage 2]  -------------\n")
    print(ir_12)
    ir_00 = IRTools.@code_ir func(args...)
    ir_02 = insert_copy_stage_2(ir_00)
    print("\nIR [Only Stage 2]  --------\n")
    print(ir_02)
    print("\n")
end
