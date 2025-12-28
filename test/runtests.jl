using Aqua, Libtask, Test

@testset verbose = true "Libtask" begin
    Aqua.test_all(Libtask)
    include("copyable_task.jl")
end
