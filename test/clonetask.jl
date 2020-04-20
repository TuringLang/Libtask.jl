# Test task copying

using Libtask
using Test

@testset "Task Copying Tests" begin

    @testset "Test case 1: stack allocated objects are deep copied." begin
        function f_ct()
            i = 0;
            while true
                produce(i)
                i = 1 + i
            end
        end

        t = CTask(f_ct)

        @test consume(t) == 0
        @test consume(t) == 1
        a = copy(t);
        @test consume(a) == 2
        @test consume(a) == 3
        @test consume(t) == 2
        @test consume(t) == 3
    end

    @testset "Test case 2: head-allocated objects are shared when cow=false" begin
        function f_ct2()
            data = [0 1 2];
            while true
                produce(data[1])
                data[1] = 1 + data[1]
            end
        end

        t = CTask(f_ct2, cow=false)

        @test consume(t) == 0
        @test consume(t) == 1
        a = copy(t);
        @test consume(a) == 2
        @test consume(a) == 3
        @test consume(t) == 4
        @test consume(t) == 5
        @test consume(a) == 6
        @test consume(a) == 7

    end

    @testset "Test case 3: Exception" begin

        function g_break()
            data = 0
            while true
                data[3] = 1
                produce(data)
                data = data + 1
            end
        end

        t = CTask(g_break)
        @test_throws Libtask.CTaskException consume(t)
    end
end
