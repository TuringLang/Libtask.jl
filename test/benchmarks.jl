using BenchmarkTools
using Libtask

INTENSITY = 3

println("= Benchmarks on Arrays =")
A = rand(1000, 1000)
x, y =  abs.(rand(Int, 2) .% 999) .+ 1
print("indexing: ")
@btime for _ in 1:INTENSITY; $A[$x, $y]; end
print("set indexing: ")
@btime for _ in 1:INTENSITY; $A[$x, $y] = 1; end
print("broadcast: ")
@btime for _ in 1:INTENSITY; $A .+ $A; end

println("= Benchmarks on TArrays =")
TA = Libtask.localize(deepcopy(A))
print("indexing: ")
@btime for _ in 1:INTENSITY; $TA[$x, $y]; end
print("set indexing: ")
@btime for _ in 1:INTENSITY; $TA[$x, $y] = 1; end
print("broadcast: ")
@btime for _ in 1:INTENSITY; $TA .+ $TA; end
