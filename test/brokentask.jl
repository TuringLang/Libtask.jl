using Libtask
using Test

@testset "Broken Functions Tests" begin

    @testset "Error Test" begin
        function ftest()
            x = 1
            while true
                error("error test")
                produce(x)
                x += 1
            end
        end

        t = CTask(ftest)
        try
            consume(t)
        catch ex
            @test ex.etype == ErrorException
        end
        @test isa(t.exception, ErrorException)
    end

    @testset "OutOfBounds Test Before `produce`" begin
        function ftest()
            x = zeros(2)
            while true
                x[1] = 1
                x[2] = 2
                x[3] = 3
                produce(x[1])
            end
        end

        t = CTask(ftest)
        try
            consume(t)
        catch ex
            @test ex.etype == BoundsError
        end
        @test isa(t.exception, BoundsError)
    end

    @testset "OutOfBounds Test After `produce`" begin
        function ftest()
            x = zeros(2)
            while true
                x[1] = 1
                x[2] = 2
                produce(x[2])
                x[3] = 3
            end
        end

        t = CTask(ftest)
        @test consume(t) == 2
        try
            consume(t)
        catch ex
            @test ex.etype == BoundsError
        end
        @test isa(t.exception, BoundsError)
    end

    @testset "OutOfBounds Test After `copy`" begin
        function ftest()
            x = zeros(2)
            while true
                x[1] = 1
                x[2] = 2
                produce(x[2])
                x[3] = 3
            end
        end

        t = CTask(ftest)
        @test consume(t) == 2
        t_copy = copy(t)
        try
            consume(t_copy)
        catch ex
            @test ex.etype == BoundsError
        end
        @test isa(t_copy.exception, BoundsError)
    end
end
