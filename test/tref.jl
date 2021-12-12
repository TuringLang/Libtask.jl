@testset "tref" begin
    # Test atomic values.
    @testset "atomic" begin
        function f()
            t = TRef(1)
            t[] = 0
            for _ in 1:6
                produce(t[])
                t[]
                t[] += 1
            end
        end

        ctask = CTask(f)

        consume(ctask)
        consume(ctask)

        a = copy(ctask)
        consume(a)
        consume(a)

        @test consume(ctask) == 2
        @test consume(a) == 4
    end

    # Test dictionary functionality.
    @testset "dictionary" begin
        function f()
            t = TRef(Dict("A" => 1, 5 => "B"))
            t["A"] = 0
            for _ in 1:6
                produce(t["A"])
                t["A"]
                t["A"] += 1
            end
        end

        ctask = CTask(f)

        consume(ctask)
        consume(ctask)

        a = copy(ctask)
        consume(a)
        consume(a)

        @test consume(ctask) == 2
        @test consume(a) == 4
    end

    @testset "array" begin
        # Create a TRef storing a matrix.
        x = TRef([1 2 3; 4 5 6])
        x[1, 3] = 900
        @test x[1,3] == 900

        # TRef holding an array.
        y = TRef([1,2,3])
        y[2] = 19
        @test y[2] == 19
    end
end
