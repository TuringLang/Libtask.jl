using BenchmarkTools
using Libtask

println("= Benchmarks on Arrays =")
A = rand(100, 100)
x, y =  abs.(rand(Int, 2) .% 100)
print("indexing: ")
@btime $A[$x, $y] + $A[$x, $y]
print("set indexing: ")
@btime $A[$x, $y] = 1
print("broadcast: ")
@btime $A .+ $A

println("= Benchmarks on TArrays =")
TA = Libtask.localize(deepcopy(A))
print("indexing: ")
@btime $TA[$x, $y] + $TA[$x, $y]
print("set indexing: ")
@btime $TA[$x, $y] = 1
print("broadcast: ")
@btime $TA .+ $TA
