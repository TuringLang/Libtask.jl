using LinearAlgebra
using Statistics

using InteractiveUtils

const MOD_METHODS = Dict{Module, Vector{Symbol}}()

methods = methodswith(AbstractArray)

for method in methods
    mod = method.module
    names = get!(MOD_METHODS, mod, Vector{Symbol}())
    push!(names, method.name)
end

for (k, v) in MOD_METHODS
    print(k)
    print(":\n\t")
    show(v)
    print("\n\n")
end
