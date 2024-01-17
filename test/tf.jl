using Libtask

foo(x) = sin(cos(x))
bar(x) = foo(foo(x))

Libtask.is_primitive(::typeof(foo), args...) = false

@testset "tapedfunction" begin
    # Test case 1: stack allocated objects are deep copied.
    @testset "Instruction{typeof(__new__)}" begin
        mutable struct S
            i::Int
            S(x, y) = new(x + y)
        end

        tf = Libtask.TapedFunction(S, 1, 2)
        s1 = tf(1, 2)
        @test s1.i == 3
        newins = findall(x -> isa(x, Libtask.Instruction{typeof(Libtask.__new__)}), tf.tape)
        @test length(newins) == 1
    end

    @testset "Compiled Tape" begin
        function g(x, y)
            if x>y
                r= string(sin(x))
            else
                r= sin(x) * cos(y)
            end
            return r
        end

        tf = Libtask.TapedFunction(g, 1., 2.)
        ctf = Libtask.compile(tf)
        r = ctf(1., 2.)

        @test typeof(r) === Float64
    end
    @testset "recurse into function" begin
        # tf = Libtask.TapedFunction(bar, 5.0)
        # count = 0
        # tf(4.0; callback=() -> (count += 1))
        # @test count == 9

        function recurse(n::Int)
            if n == 0
                return 0
            end
            recurse(n-1)
            produce(n)
        end
        Libtask.is_primitive(::typeof(recurse), args...) = false
        ttask = TapedTask(recurse, 3)

        @test consume(ttask) == 1
        @test consume(ttask) == 2
        @test consume(ttask) == 3
        @test consume(ttask) === nothing

        function recurse2(n::Int)
            if n == 0
                return 0
            end
            produce(n)
            recurse2(n-1)
        end
        Libtask.is_primitive(::typeof(recurse2), args...) = false
        ttask = TapedTask(recurse2, 3)

        @test consume(ttask) == 3
        @test consume(ttask) == 2
        @test consume(ttask) == 1
        @test consume(ttask) === nothing
    end

    @testset "Not optimize mutating call" begin

        function f!(a)
            a[1] = 2
            return 1
        end
        
        function g1()
            a = [1,2]
            a[2] = f!(a)
            produce(a[1])
        end
        
        ttask = TapedTask(g1)
        @test consume(ttask) == 2
        @test consume(ttask) === nothing
    end

    @testset "Not optimize producing call" begin
        function f2()
            produce(2)
            return 1
        end

        function g2()
            a = [1]
            a[1] = f2()
            produce(a[1])
        end

        ttask = TapedTask(g2)
        @test consume(ttask) == 2
        @test consume(ttask) == 1
        @test consume(ttask) === nothing        
    end
end