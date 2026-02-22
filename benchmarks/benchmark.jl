using Libtask
using LinearAlgebra
using Chairmarks: @b

# Each benchmark function takes a `maybe_produce` as its last argument, defaulting to
# `identity` (a no-op). The benchmark driver calls the function twice: once with the
# default (measuring raw performance) and once via a TapedTask that passes `produce` as the
# last argument (measuring the overhead of the produce/consume machinery).
function benchmark(f, x...)
    printstyled(string(f), "\n"; bold=true)

    # Baseline: call f directly with maybe_produce=identity (the default).
    baseline = @b $f($(x)...)

    # TapedTask: pass `produce` so every `maybe_produce(...)` call yields a value.
    function f_via_task(f, x)
        tt = TapedTask(nothing, f, x..., produce)
        n = 0
        while consume(tt) !== nothing
            n += 1
        end
        return n
    end
    n_produces = f_via_task(f, x)
    taped = @b $f_via_task($f, $x)

    noun = n_produces == 1 ? "produce" : "produces"
    label = "taped ($n_produces $noun)"
    print(rpad("baseline", length(label)), "  ")
    display(baseline)
    print(label, "  ")
    display(taped)
    ratio = round(taped.time / baseline.time; digits=1)
    println(rpad("ratio", length(label)), "  ", "$(ratio)x")
    println()
    return nothing
end

function rosenbrock(x, maybe_produce=identity)
    i = x[2:end]
    j = x[1:(end - 1)]
    ret = sum((1 .- j) .^ 2 + 100 * (i - j .^ 2) .^ 2)
    maybe_produce(ret)
    return ret
end
benchmark(rosenbrock, rand(100_000))

function ackley(x::AbstractVector, maybe_produce=identity)
    a, b, c = 20.0, -0.2, 2.0 * π
    len_recip = inv(length(x))
    sum_sqrs = zero(eltype(x))
    sum_cos = sum_sqrs
    for i in x
        sum_cos += cos(c * i)
        sum_sqrs += i^2
        maybe_produce(sum_sqrs)
    end
    return -a * exp(b * sqrt(len_recip * sum_sqrs)) - exp(len_recip * sum_cos) +
           a +
           MathConstants.e
end
benchmark(ackley, rand(100_000))

function matrix_test(x, maybe_produce=identity)
    n = 100
    a = reshape(x[1:(n^2)], n, n)
    b = reshape(x[(n^2 + 1):(2n^2)], n, n)
    ret = log.((a * b) + a - b)
    maybe_produce(ret)
    return ret
end
benchmark(matrix_test, collect(1.0:(2 * 100^2 + 100)))

relu(x) = log.(1.0 .+ exp.(x))
sigmoid(n) = 1.0 / (1.0 + exp(-n))
function neural_net(w1, w2, w3, x1, maybe_produce=identity)
    x2 = relu(w1 * x1)
    x3 = relu(w2 * x2)
    ret = sigmoid(LinearAlgebra.dot(w3, x3))
    maybe_produce(ret)
    return ret
end
benchmark(neural_net, randn(10, 10), randn(10, 10), randn(10), rand(10))
