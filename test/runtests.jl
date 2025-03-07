include("front_matter.jl")
@testset "Libtask" begin
    @testset "quality" begin
        # ScopedValues is stale on 1.11.
        Aqua.test_all(Libtask; stale_deps=VERSION < v"1.11" ? true : false)
        @test JuliaFormatter.format(Libtask; verbose=false, overwrite=false)
    end
    include("copyable_task.jl")
    # include("issues.jl")
end
