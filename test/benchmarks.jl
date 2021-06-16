using BenchmarkTools
using Libtask


macro rep(cnt, exp)
    blk =:(begin end)
    for _ in 1:eval(cnt)
        push!(blk.args, esc(exp))
    end
    blk
end

INTENSITY = 6

indexing(a, x, y) = @rep INTENSITY a[x, y]
setindexing(a, x, y) = @rep INTENSITY a[x, y] = 1
broadcasting(a) = @rep INTENSITY a .+ a

println("= Benchmarks on Arrays =")
A = rand(1000, 1000)
x, y =  abs.(rand(Int, 2) .% 999) .+ 1
print("indexing: ")
@btime indexing($A, $x, $y)
print("set indexing: ")
@btime setindexing($A, $x, $y)
print("broadcast: ")
@btime broadcasting($A)

println("= Benchmarks on TArrays =")
TA = Libtask.localize(deepcopy(A))
print("indexing: ")
@btime indexing($TA, $x, $y)
print("set indexing: ")
@btime setindexing($TA, $x, $y)
print("broadcast: ")
@btime broadcasting($TA)

print("= Benchmarks on task and TArray copying")
function f()
    t = TArray(Int, 1)
    t[1] = 0
    while true
        produce(t[1])
        t[1]
        t[1] = 1 + t[1]
    end
end

@btime begin
    ctask = CTask(f)
    consume(ctask)
    consume(ctask)
    a = copy(ctask)
    consume(a)
    consume(a)
end
