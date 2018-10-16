using Libtask
using Test

r = @testset "Broken Functions Tests" begin

    @testset "Error Test" begin

        function ftest()
            x = 1
            while true
                @error "test"
                produce(x)
                x += 1
            end
        end

        t = Task(ftest)
        try
            consume(t)
        catch ex
            @test isa(ex, InterruptException)
        end
        @test isa(t.exception, InterruptException)
    end

    @testset "OutOfBounds Test Before" begin
        function ftest()
            x = zeros(2)
            while true
                x[1] = 1
                x[2] = 2
                x[3] = 3
                produce(x[1])
            end
        end

        t = Task(ftest)
        try
            consume(t)
        catch ex
            @test isa(ex, BoundsError)
        end
        @test isa(t.exception, BoundsError)
    end
end
Test.print_test_results(r)
