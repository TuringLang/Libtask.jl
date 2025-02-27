using JuliaFormatter, Libtask, Test

@testset "Libtask" begin
    @testset "quality" begin
        Aqua.test_all(Libtask)
        @test JuliaFormatter.format(Mooncake; verbose=false, overwrite=false)
    end
    include("copyable_task.jl")
    include("issues.jl")
end
