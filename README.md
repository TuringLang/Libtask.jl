# Libtask

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://turinglang.github.io/Libtask.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://turinglang.github.io/Libtask.jl/dev)
[![Libtask Testing](https://github.com/TuringLang/Libtask.jl/workflows/Libtask%20Testing/badge.svg)](https://github.com/TuringLang/Libtask.jl/actions?branch=main)


Resumable and copyable functions (a.k.a. coroutines) for Julia, with optional function-specific globals.
See the [docs](https://turinglang.github.io/Libtask.jl/dev) for example usage.

Used in the [Turing](https://github.com/TuringLang/Turing.jl) probabilistic programming language to implement various particle-based inference methods, for example, those in [AdvancedPS.jl](https://github.com/TuringLang/AdvancedPS.jl/).

[ResumableFunctions.jl](https://github.com/JuliaDynamics/ResumableFunctions.jl) provides resumable functions similar to Libtask's, but lacks copying. 
