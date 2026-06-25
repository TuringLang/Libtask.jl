# AGENTS.md

## Purpose

Libtask.jl provides resumable, **copyable** functions (coroutines) for Julia, with optional
function-specific globals. [Turing.jl](https://github.com/TuringLang/Turing.jl) (via
[AdvancedPS.jl](https://github.com/TuringLang/AdvancedPS.jl)) uses it for particle-based
inference, where copying a partially-run task — not just resuming it — is the load-bearing
feature.

Mechanism: `Base.code_ircode_by_type` recovers typed IR; `derive_copyable_task_ir` splits it
at every `produce` site, lifts variables that must survive a suspension into `Ref`s, and emits
a re-enterable `MistyClosure`. `copy(::TapedTask)` deep-copies those `Ref`s to fork state.

Priorities, in order: (1) `produce`/`consume`/`copy` correctness — a copy resumes
independently and reproduces a fresh run exactly; (2) faithful control flow (branches, loops,
phi nodes, exception regions), failing loudly and locally on unsupported constructs
(`assert_can_handle_control_flow`) rather than resuming wrongly; (3) type stability and low
allocation on the `consume` hot path; (4) minimal dependencies and a small IR-transform core.

## Repository Layout

- `src/Libtask.jl` — module entry, exports (`TapedTask`, `consume`, `produce`,
  `get_taped_globals`, `set_taped_globals!`, `NotInTapedTaskError`).
- `src/copyable_task.jl` — public API + `build_callable` (IR-derivation/compile/cache entry),
  the `MistyClosure` cache, and `copy`.
- `src/transformation.jl` — core IR transform: `produce`-site detection (`is_produce_stmt`,
  `stmt_might_produce`), block splitting, `Ref` creation, reassembly.
- `src/refelim.jl` — `eliminate_refs`: live-variable analysis dropping `Ref` loads/stores not
  needed across `produce` boundaries.
- `src/bbcode.jl` — `BasicBlockCode`, the basic-block IR repr **copied from Mooncake.jl** to
  avoid a dependency; keep in sync with Mooncake when fixing IR bugs.
- `src/utils.jl` — IR helpers (`replace_captures`, `optimise_ir!`, `misty_closure`,
  `Core.Compiler` version shims).
- `src/test_utils.jl` — `TestUtils`: the `Testcase` driver.
- `test/` — `runtests.jl` runs Aqua + `copyable_task.jl`; `test/integration/turing/` is a
  separate-environment Turing.jl suite.
- `docs/src/` — `index.md` (user) and `internals.md` (documented-internals docstring index).

## Working Conventions

- **Lives on `Core.Compiler` internals**, unstable across Julia versions — the most fragile
  part of the codebase. Guard version-specific behaviour with `@static if VERSION ...` (e.g.
  `set_valid_world!`, the `get_mi` shim); test on every supported version. The `[compat]`
  range (`~1.10.8, 1.11.6`) and the CI matrix (`.github/workflows/CI.yml`) move together.
- Fix IR problems by making the transform correct, not by special-casing the public API.
  `transformation.jl`/`refelim.jl` are the single source of truth for the resumable IR.
- `produce` and `get_taped_globals(::Type)` are **reserved statements**, not real functions:
  `produce` is `@noinline` with a dummy side-effect so the transform can find/rewrite it (it
  never runs under `consume`). Don't "optimise away" what resists inlining/constant-folding.
- Concrete `Core.Compiler` types (`IRCode`, `SSAValue`, …) in the transform layer are
  load-bearing, not over-constraint; annotate as specifically as dispatch requires.
- **Workflow for any bug fix or new feature**: investigate → root-cause → understand the big
  picture → *verify the fix before committing to it*. Reproduce with an MWE, inspect derived IR
  (`Libtask.generate_ir`), and confirm the hypothesis by temporarily editing or monkey-patching
  (e.g. `@eval`-ing a replacement method, hacking the local checkout) until the MWE behaves —
  only then write the real change. Don't edit the transform on a guess. Prefer targeted fixes
  over new helpers or refactors; run `minimise` before committing.
- Write clear, local errors for unsupported inputs (naked `produce`, unhandled control flow) —
  a clear `ArgumentError` at construction beats a confusing failure in compiled IR.
- Internals may change freely; exports, the `produce`/`consume`/`copy` contract, and names in
  `docs/src/internals.md` need tests, docs, and stable errors — update docs when they change.
- Format with JuliaFormatter, **blue** style: `julia -e 'using JuliaFormatter; format(".")'`.

## Concurrency

- `build_callable` is serialized under `build_callable_lock` (`ReentrantLock`) — deliberate:
  IR derivation mutates a shared `ID` counter (`seed_id!`/`ID()`) and the `MistyClosure` cache,
  and concurrent derivation segfaulted (#227). Keep all ID generation and cache mutation inside
  this lock; don't add a second IR-deriving code path outside it.
- `bbcode.jl` indexes its `ID` counter by `Threads.threadid()` — suspect (task migration makes
  IDs unstable); the lock, not `threadid()`, is what makes it safe today.
- `copy(::TapedTask)` is `deepcopy`: the copy must share **no** mutable state (`Ref`s,
  position). Any change to state storage must keep this — test that mutating a copy doesn't
  affect the source.

## Testing

- MWE first, then the smallest focused test, then broader groups. Prefer extending existing
  `Testcase`s over adding new ones; prune with `minimise`.
- Register behaviour as `TestUtils.Testcase`s where possible — the driver iterates the task,
  `copy`s after every iteration, and checks every copy resumes to the same result sequence
  (plus optional allocation flags), exercising the copy/resume contract for free. Reserve
  bespoke `test/` code for what the driver can't express.
- Cover control-flow variety (branches, loops, nested produce, exception regions) and varied
  value types. Bug fixes land with a focused regression test; version/world-age fixes get an
  isolated direct test. Check `consume` allocations (`@allocated` post-warmup) and stability
  (`@code_warntype`) for perf-sensitive paths.
- Never disable tests or weaken perf/allocation assertions to get CI green — ask first.
- `test/integration/turing/` runs in its own environment and is part of the contract: transform
  or semantics changes may need updates there even when core tests pass.

## Documentation

`docs/make.jl` drives Documenter; `index.md` is user-facing, `internals.md` indexes documented
internals. Update docstrings (and `internals.md` when adding/removing a documented internal)
when changing the public API, transform internals, or developer helpers.
