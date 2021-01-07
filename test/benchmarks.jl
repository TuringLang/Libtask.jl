using BenchmarkTools
using Libtask


 macro rep(cnt, exp)
     blk =:(begin end)
     for _ in 1:eval(cnt)
         push!(blk.args, esc(exp))
     end
     blk
 end

INTENSITY = 2

println("= Benchmarks on Arrays =")
A = rand(1000, 1000)
x, y =  abs.(rand(Int, 2) .% 999) .+ 1
print("indexing: ")
@btime @rep INTENSITY $A[$x, $y]
print("set indexing: ")
@btime @rep INTENSITY $A[$x, $y] = 1
print("broadcast: ")
@btime @rep INTENSITY $A .+ $A

println("= Benchmarks on TArrays =")
TA = Libtask.localize(deepcopy(A))
print("indexing: ")
@btime @rep INTENSITY $TA[$x, $y]
print("set indexing: ")
@btime @rep INTENSITY $TA[$x, $y] = 1
print("broadcast: ")
@btime @rep INTENSITY $TA .+ $TA
