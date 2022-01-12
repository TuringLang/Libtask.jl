using Turing, Test, AbstractMCMC, DynamicPPL, Random

import AbstractMCMC.AbstractSampler

function check_numerical(chain,
                         symbols::Vector,
                         exact_vals::Vector;
                         atol=0.2,
                         rtol=0.0)
    for (sym, val) in zip(symbols, exact_vals)
        E = val isa Real ?
            mean(chain[sym]) :
            vec(mean(chain[sym], dims=1))
        @info (symbol=sym, exact=val, evaluated=E)
        @test E ≈ val atol=atol rtol=rtol
    end
end

function check_MoGtest_default(chain; atol=0.2, rtol=0.0)
    check_numerical(chain,
                    [:z1, :z2, :z3, :z4, :mu1, :mu2],
                    [1.0, 1.0, 2.0, 2.0, 1.0, 4.0],
                    atol=atol, rtol=rtol)
end

@model gdemo_d(x, y) = begin
    s ~ InverseGamma(2, 3)
    m ~ Normal(0, sqrt(s))
    x ~ Normal(m, sqrt(s))
    y ~ Normal(m, sqrt(s))
    return s, m
end

alg = CSMC(15)
chain = sample(gdemo_d(1.5, 2.0), alg, 5_00)

@show chain

check_numerical(chain, [:s, :m], [49/24, 7/6], atol=0.1)
