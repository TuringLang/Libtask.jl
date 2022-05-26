using Libtask
using LinearAlgebra
using BenchmarkTools

####################################################################

function benchmark_driver!(f, x...; f_displayname=string(f))
    x = (x..., nothing)

    println("benchmarking $(f_displayname)...")
    tf = Libtask.TapedFunction(f, x...);

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

    print("  Run TapedTask: ")
    x = (x[1:end-1]..., produce);
    # show the number of produce calls inside `f`
    f_task = (f, x; verbose=false) -> begin
        tt = TapedTask(f, x...);
        c = 0
        while consume(tt)!==nothing
            c+=1
        end
        verbose && print("#produce=", c, "; ");
    end
    # Note that we need to pass `f` instead of `tf` to avoid
    #  default continuation in `TapedTask` constructor, see, e.g.
    #  https://github.com/TuringLang/Libtask.jl/pull/135
    f_task(f, x; verbose=true) # print #produce calls
    @btime $f_task($f, $x)
    GC.gc()
end

####################################################################


function rosenbrock(x, callback=nothing)
    i = x[2:end]
    j = x[1:end-1]
    ret = sum((1 .- j).^2 + 100*(i - j.^2).^2)
    callback !== nothing && callback(ret)
    return ret
end

x = rand(100000)
benchmark_driver!(rosenbrock, x)

####################################################################

function ackley(x::AbstractVector, callback=nothing)
    a, b, c = 20.0, -0.2, 2.0*π
    len_recip = inv(length(x))
    sum_sqrs = zero(eltype(x))
    sum_cos = sum_sqrs
    for i in x
        sum_cos += cos(c*i)
        sum_sqrs += i^2
        callback !== nothing && callback(sum_sqrs)
    end
    return (-a * exp(b * sqrt(len_recip*sum_sqrs)) -
            exp(len_recip*sum_cos) + a + MathConstants.e)
end

x = rand(100000)
benchmark_driver!(ackley, x)

####################################################################
function generate_matrix_test(n)
    return (x, callback=nothing) -> begin
        # @assert length(x) == 2n^2 + n
        a = reshape(x[1:n^2], n, n)
        b = reshape(x[n^2 + 1:2n^2], n, n)
        ret = log.((a * b) + a - b)
        callback !== nothing && callback(ret)
        return ret
    end
end

n = 100
matrix_test = generate_matrix_test(n)
x = collect(1.0:(2n^2 + n))
benchmark_driver!(matrix_test, x; f_displayname="matrix_test")

####################################################################
relu(x) = log.(1.0 .+ exp.(x))
sigmoid(n) = 1. / (1. + exp(-n))

function neural_net(w1, w2, w3, x1, callback=nothing)
    x2 = relu(w1 * x1)
    x3 = relu(w2 * x2)
    ret = sigmoid(LinearAlgebra.dot(w3, x3))
    callback !== nothing && callback(ret)
    return ret
end

xs = (randn(10,10), randn(10,10), randn(10), rand(10))
# benchmark_driver!(neural_net, xs...)

####################################################################

println("======= breakdown benchmark =======")

x = rand(100000)
tf = Libtask.TapedFunction(ackley, x, nothing)
tf(x, nothing);
ins = tf.tape[45]
b = ins.input[1]

@show ins.input |> length
@btime map(x -> Libtask._lookup(tf, x), ins.input)
@btime Libtask._lookup(tf, b)
# @btime b.get(tf, b.id)
@btime tf.bindings[b]

println("done")
