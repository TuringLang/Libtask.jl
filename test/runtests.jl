using JuliaFormatter, Libtask, Test

@testset "Libtask" begin

    include("copyable_task.jl")
    include("issues.jl")
end
