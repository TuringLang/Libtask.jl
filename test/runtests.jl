include("front_matter.jl")
@testset "Libtask" begin
    @testset "quality" begin
        Aqua.test_all(Libtask)
        @test JuliaFormatter.format(Libtask; verbose=false, overwrite=false)
    end
    include("copyable_task.jl")
    # include("issues.jl")
end
