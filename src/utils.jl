function get_mi(ci::Core.CodeInstance)
    @static isdefined(CC, :get_ci_mi) ? CC.get_ci_mi(ci) : ci.def
end
get_mi(mi::Core.MethodInstance) = mi

"""
    replace_captures(oc::Toc, new_captures) where {Toc<:OpaqueClosure}

Given an `OpaqueClosure` `oc`, create a new `OpaqueClosure` of the same type, but with new
captured variables. This is needed for efficiency reasons -- if `build_rrule` is called
repeatedly with the same signature and intepreter, it is important to avoid recompiling
the `OpaqueClosure`s that it produces multiple times, because it can be quite expensive to
do so.
"""
function replace_captures(oc::Toc, new_captures) where {Toc<:OpaqueClosure}
    return __replace_captures_internal(oc, new_captures)
end

@eval function __replace_captures_internal(oc::Toc, new_captures) where {Toc<:OpaqueClosure}
    return $(Expr(
        :new, :(Toc), :new_captures, :(oc.world), :(oc.source), :(oc.invoke), :(oc.specptr)
    ))
end

"""
    replace_captures(mc::Tmc, new_captures) where {Tmc<:MistyClosure}

Same as `replace_captures` for `Core.OpaqueClosure`s, but returns a new `MistyClosure`.
"""
function replace_captures(mc::Tmc, new_captures) where {Tmc<:MistyClosure}
    return Tmc(replace_captures(mc.oc, new_captures), mc.ir)
end

"""
    optimise_ir!(ir::IRCode, show_ir=false)

Run a fairly standard optimisation pass on `ir`. If `show_ir` is `true`, displays the IR
to `stdout` at various points in the pipeline -- this is sometimes useful for debugging.
"""
function optimise_ir!(ir::IRCode; show_ir=false, do_inline=true)
    if show_ir
        println("Pre-optimization")
        display(ir)
        println()
    end
    CC.verify_ir(ir)
    ir = __strip_coverage!(ir)
    ir = CC.compact!(ir)
    local_interp = CC.NativeInterpreter()
    # local_interp = BugPatchInterpreter() # 319 -- see patch_for_319.jl for context
    mi = __get_toplevel_mi_from_ir(ir, @__MODULE__)
    ir = __infer_ir!(ir, local_interp, mi)
    if show_ir
        println("Post-inference")
        display(ir)
        println()
    end
    inline_state = CC.InliningState(local_interp)
    CC.verify_ir(ir)
    if do_inline
        ir = CC.ssa_inlining_pass!(ir, inline_state, true) #=propagate_inbounds=#
        ir = CC.compact!(ir)
    end
    ir = __strip_coverage!(ir)
    ir = CC.sroa_pass!(ir, inline_state)

    @static if VERSION < v"1.11-"
        ir = CC.adce_pass!(ir, inline_state)
    else
        ir, _ = CC.adce_pass!(ir, inline_state)
    end

    ir = CC.compact!(ir)
    # CC.verify_ir(ir, true, false, CC.optimizer_lattice(local_interp))
    @static if VERSION >= v"1.12-"
        CC.verify_linetable(ir.debuginfo, div(length(ir.debuginfo.codelocs), 3), true)
    else
        CC.verify_linetable(ir.linetable, true)
    end
    if show_ir
        println("Post-optimization")
        display(ir)
        println()
    end
    return ir
end

@static if VERSION < v"1.11.0"
    get_inference_world(interp::CC.AbstractInterpreter) = CC.get_world_counter(interp)
else
    get_inference_world(interp::CC.AbstractInterpreter) = CC.get_inference_world(interp)
end

# Given some IR, generates a MethodInstance suitable for passing to infer_ir!, if you don't
# already have one with the right argument types. Credit to @oxinabox:
# https://gist.github.com/oxinabox/cdcffc1392f91a2f6d80b2524726d802#file-example-jl-L54
function __get_toplevel_mi_from_ir(ir, _module::Module)
    mi = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ())
    mi.specTypes = Tuple{map(CC.widenconst, ir.argtypes)...}
    mi.def = _module
    return mi
end

# Run type inference and constant propagation on the ir. Credit to @oxinabox:
# https://gist.github.com/oxinabox/cdcffc1392f91a2f6d80b2524726d802#file-example-jl-L54
function __infer_ir!(ir, interp::CC.AbstractInterpreter, mi::CC.MethodInstance)
    # TODO(mhauru) Why is this line here? This function is no longer defined in 1.12
    @static if VERSION >= v"1.12-"
        nargs = length(ir.argtypes) - 1
        # TODO(mhauru) How do we figure out isva? I don't think it's in ir or mi, see above
        # prints.
        isva = false
        propagate_inbounds = true
        spec_info = CC.SpecInfo(nargs, isva, propagate_inbounds, nothing)
        min_world = world = get_inference_world(interp)
        max_world = Base.get_world_counter()
        irsv = CC.IRInterpretationState(
            interp, spec_info, ir, mi, ir.argtypes, world, min_world, max_world
        )
        rt = CC.ir_abstract_constant_propagation(interp, irsv)
    else
        method_info = CC.MethodInfo(true, nothing) #=propagate_inbounds=#
        min_world = world = get_inference_world(interp)
        max_world = Base.get_world_counter()
        irsv = CC.IRInterpretationState(
            interp, method_info, ir, mi, ir.argtypes, world, min_world, max_world
        )
        rt = CC._ir_abstract_constant_propagation(interp, irsv)
    end
    return ir
end

# In automatically generated code, it is meaningless to include code coverage effects.
# Moreover, it seems to cause some serious inference probems. Consequently, it makes sense
# to remove such effects before optimising IRCode.
function __strip_coverage!(ir::IRCode)
    for n in eachindex(stmt(ir.stmts))
        if Meta.isexpr(stmt(ir.stmts)[n], :code_coverage_effect)
            stmt(ir.stmts)[n] = nothing
        end
    end
    return ir
end

stmt(ir::CC.InstructionStream) = @static VERSION < v"1.11.0-rc4" ? ir.inst : ir.stmt

"""
    opaque_closure(
        ret_type::Type,
        ir::IRCode,
        @nospecialize env...;
        isva::Bool=false,
        do_compile::Bool=true,
    )::Core.OpaqueClosure{<:Tuple, ret_type}

Construct a `Core.OpaqueClosure`. Almost equivalent to
`Core.OpaqueClosure(ir, env...; isva, do_compile)`, but instead of letting
`Core.compute_oc_rettype` figure out the return type from `ir`, impose `ret_type` as the
return type.

# Warning

User beware: if the `Core.OpaqueClosure` produced by this function ever returns anything
which is not an instance of a subtype of `ret_type`, you should expect all kinds of awful
things to happen, such as segfaults. You have been warned!

# Extended Help

This is needed because we make extensive use of our ability to know the return
type of a couple of specific `OpaqueClosure`s without actually having constructed them.
Without the capability to specify the return type, we have to guess what type
`compute_ir_rettype` will return for a given `IRCode` before we have constructed
the `IRCode` and run type inference on it. This exposes us to details of type inference,
which are not part of the public interface of the language, and can therefore vary from
Julia version to Julia version (including patch versions). Moreover, even for a fixed Julia
version it can be extremely hard to predict exactly what type inference will infer to be the
return type of a function.

Failing to correctly guess the return type can happen for a number of reasons, and the kinds
of errors that tend to be generated when this fails tell you very little about the
underlying cause of the problem.

By specifying the return type ourselves, we remove this dependence. The price we pay for
this is the potential for segfaults etc if we fail to specify `ret_type` correctly.
"""
function opaque_closure(
    ret_type::Type,
    ir::IRCode,
    @nospecialize env...;
    isva::Bool=false,
    do_compile::Bool=true,
)
    # This implementation is copied over directly from `Core.OpaqueClosure`.
    ir = CC.copy(ir)
    @static if VERSION >= v"1.12-"
        # On v1.12 OpaqueClosure expects the first arg to be the environment.
        ir.argtypes[1] = typeof(env)
    end
    nargtypes = length(ir.argtypes)
    nargs = nargtypes - 1
    @static if VERSION >= v"1.12-"
        sig = CC.compute_oc_signature(ir, nargs, isva)
    else
        sig = Base.Experimental.compute_oc_signature(ir, nargs, isva)
    end
    src = ccall(:jl_new_code_info_uninit, Ref{CC.CodeInfo}, ())
    src.slotnames = [Symbol(:_, i) for i in 1:nargtypes]
    src.slotflags = fill(zero(UInt8), nargtypes)
    src.slottypes = copy(ir.argtypes)
    @static if VERSION > v"1.12-"
        ir.debuginfo.def === nothing &&
            (ir.debuginfo.def = :var"generated IR for OpaqueClosure")
        src.min_world = ir.valid_worlds.min_world
        src.max_world = ir.valid_worlds.max_world
        src.isva = isva
        src.nargs = nargtypes
    end
    src = CC.ir_to_codeinf!(src, ir)
    src.rettype = ret_type
    return Base.Experimental.generate_opaque_closure(
        sig, Union{}, ret_type, src, nargs, isva, env...; do_compile
    )::Core.OpaqueClosure{sig,ret_type}
end

function optimized_opaque_closure(rtype, ir::IRCode, env...; kwargs...)
    oc = opaque_closure(rtype, ir, env...; kwargs...)
    world = UInt(oc.world)
    set_world_bounds_for_optimization!(oc)
    optimized_oc = optimize_opaque_closure(oc, rtype, env...; kwargs...)
    return optimized_oc
end

function optimize_opaque_closure(oc::Core.OpaqueClosure, rtype, env...; kwargs...)
    method = oc.source
    ci = method.specializations.cache
    world = UInt(oc.world)
    ir = reinfer_and_inline(ci, world)
    ir === nothing && return oc # nothing to optimize
    return opaque_closure(rtype, ir, env...; kwargs...)
end

# Allows optimization to make assumptions about binding access,
# enabling inlining and other optimizations.
function set_world_bounds_for_optimization!(oc::Core.OpaqueClosure)
    ci = oc.source.specializations.cache
    ci.inferred === nothing && return nothing
    ci.inferred.min_world = oc.world
    return ci.inferred.max_world = oc.world
end

function reinfer_and_inline(ci::Core.CodeInstance, world::UInt)
    interp = CC.NativeInterpreter(world)
    mi = get_mi(ci)
    argtypes = collect(Any, mi.specTypes.parameters)
    irsv = CC.IRInterpretationState(interp, ci, mi, argtypes, world)
    irsv === nothing && return nothing
    for stmt in irsv.ir.stmts
        inst = stmt[:inst]
        if Meta.isexpr(inst, :loopinfo) ||
            Meta.isexpr(inst, :pop_exception) ||
            isa(inst, CC.GotoIfNot) ||
            isa(inst, CC.GotoNode) ||
            Meta.isexpr(inst, :copyast)
            continue
        end
        stmt[:flag] |= CC.IR_FLAG_REFINED
    end
    CC.ir_abstract_constant_propagation(interp, irsv)
    state = CC.InliningState(interp)
    ir = CC.ssa_inlining_pass!(irsv.ir, state, CC.propagate_inbounds(irsv))
    return ir
end

"""
    misty_closure(
        ret_type::Type,
        ir::IRCode,
        @nospecialize env...;
        isva::Bool=false,
        do_compile::Bool=true,
    )

Identical to [`opaque_closure`](@ref), but returns a `MistyClosure` closure rather
than a `Core.OpaqueClosure`.
"""
function misty_closure(
    ret_type::Type,
    ir::IRCode,
    @nospecialize env...;
    isva::Bool=false,
    do_compile::Bool=true,
)
    return MistyClosure(opaque_closure(ret_type, ir, env...; isva, do_compile), Ref(ir))
end

function optimized_misty_closure(
    ret_type::Type,
    ir::IRCode,
    @nospecialize env...;
    isva::Bool=false,
    do_compile::Bool=true,
)
    return MistyClosure(
        optimized_opaque_closure(ret_type, ir, env...; isva, do_compile), Ref(ir)
    )
end
