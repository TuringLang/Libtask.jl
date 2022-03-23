using Libtask
using LinearAlgebra
using BenchmarkTools

####################################################################

function benchmark_driver!(f, x...; f_displayname=string(f))
    println("benchmarking $(f_displayname)...")
    tf = Libtask.TapedFunction(f, x)

    print("  Run Original Function:")
    @btime $f($(x)...)
    GC.gc()

    print("  Run TapedFunction:")
    @btime $tf($(x)...)
    GC.gc()

    ctf = Libtask.compile(tf)
    print("  Run TapedFunction (compiled):")
    @btime $ctf($(x)...)
    GC.gc()
end

####################################################################


function rosenbrock(x)
    i = x[2:end]
    j = x[1:end-1]
    return sum((1 .- j).^2 + 100*(i - j.^2).^2)
end

x = rand(100000)
benchmark_driver!(rosenbrock, x)

####################################################################

function ackley(x::AbstractVector)
    a, b, c = 20.0, -0.2, 2.0*π
    len_recip = inv(length(x))
    sum_sqrs = zero(eltype(x))
    sum_cos = sum_sqrs
    for i in x
        sum_cos += cos(c*i)
        sum_sqrs += i^2
    end
    return (-a * exp(b * sqrt(len_recip*sum_sqrs)) -
            exp(len_recip*sum_cos) + a + MathConstants.e)
end

x = rand(100000)
benchmark_driver!(ackley, x)

####################################################################
function generate_matrix_test(n)
    return x -> begin
        # @assert length(x) == 2n^2 + n
        a = reshape(x[1:n^2], n, n)
        b = reshape(x[n^2 + 1:2n^2], n, n)
        return log.((a * b) + a - b)
    end
end

n = 100
matrix_test = generate_matrix_test(n)
x = collect(1.0:(2n^2 + n))
benchmark_driver!(matrix_test, x; f_displayname="matrix_test")

####################################################################
relu(x) = log.(1.0 .+ exp.(x))
sigmoid(n) = 1. / (1. + exp(-n))

function neural_net(w1, w2, w3, x1)
    x2 = relu(w1 * x1)
    x3 = relu(w2 * x2)
    return sigmoid(LinearAlgebra.dot(w3, x3))
end

xs = (randn(10,10), randn(10,10), randn(10), rand(10))
benchmark_driver!(neural_net, xs...)

####################################################################

println("done")
