# AGENTS.md

## Purpose

Libtask.jl provides resumable, **copyable** functions (coroutines) for Julia, with optional
state private to each `TapedTask`. It supports particle-based inference in the Turing
ecosystem, where resampling requires independent copies of partially run tasks.

Mechanism: `Base.code_ircode_by_type` recovers typed IR. `derive_copyable_task_ir` splits it
at every statement that may `produce` and stores SSA values needed after a suspension in
`Ref`s; `eliminate_refs` removes unnecessary storage. `build_callable` compiles the result as
a re-enterable `MistyClosure`. `copy(::TapedTask)` deep-copies the task-owned state.

Priorities, in order: (1) `produce`/`consume`/`copy` correctness — a copy preserves the
source's suspension point, evolves independently, and produces the same remaining sequence;
(2) faithful control flow (branches, loops, phi nodes, exception regions), failing locally
on unsupported constructs rather than resuming incorrectly; (3) type stability and low
allocation on the `consume` hot path; (4) minimal dependencies and a small IR-transform core.

## Repository layout

- `src/Libtask.jl` — module entry, exports (`TapedTask`, `consume`, `produce`,
  `get_taped_globals`, `set_taped_globals!`, `NotInTapedTaskError`).
- `src/copyable_task.jl` — public API + `build_callable` (IR-derivation/compile/cache entry),
  the `MistyClosure` cache, and `copy`.
- `src/transformation.jl` — core IR transform: `produce`-site detection (`is_produce_stmt`,
  `stmt_might_produce`), block splitting, `Ref` creation, reassembly.
- `src/refelim.jl` — `eliminate_refs`: live-variable analysis dropping `Ref` loads/stores not
  needed across `produce` boundaries.
- `src/bbcode.jl` — `BasicBlockCode`, the stable-ID basic-block representation used by the
  transform.
- `src/utils.jl` — IR helpers (`replace_captures`, `optimise_ir!`, `misty_closure`,
  `Core.Compiler` version shims).
- `src/test_utils.jl` — `TestUtils`: the `Testcase` driver.
- `test/` — `runtests.jl` runs Aqua + `copyable_task.jl`; `test/integration/turing/` is a
  separate-environment Turing.jl suite.
- `docs/src/` — `index.md` (user) and `internals.md` (documented-internals docstring index).

## Working conventions

- Compiler internals are the most fragile part of the codebase. Use compile-time version or
  feature guards where their representation differs. Treat `Project.toml`, the CI workflows,
  and guards or warnings in the source as authoritative for supported Julia versions; keep
  them aligned and run every configured test line when support changes.
- Fix IR problems by making the transform correct, not by special-casing the public API.
  Make resumable-IR changes in `transformation.jl` or `refelim.jl`.
- `produce` is an `@noinline` marker function with a dummy side effect so direct calls remain
  visible to the transform. Calls through another function suspend only when the callee is
  recognised by `might_produce` or `@might_produce`; otherwise `produce` executes as an
  ordinary call and does not suspend. `get_taped_globals(::Type)` is a normal `@noinline`
  lookup of task-local storage populated by `consume`, not a transformed statement. Do not
  remove the annotations or dummy side effect without replacing their purpose.
- Concrete `Core.Compiler` types (`IRCode`, `SSAValue`, …) in the transform layer intentionally
  constrain dispatch. Generalise them only when callers require it and the IR tests cover it.
- **Workflow for any bug fix or new feature**: investigate → root-cause → understand the big
  picture → *verify the fix before committing to it*. Reproduce with an MWE, inspect derived IR
  (`Libtask.generate_ir`), and confirm the hypothesis by temporarily editing or monkey-patching
  (e.g. `@eval`-ing a replacement method, hacking the local checkout) until the MWE behaves —
  only then write the real change. Don't edit the transform on a guess. Prefer targeted fixes
  over new helpers or refactors; reduce the reproducer, regression test, and diff before
  committing.
- Write clear, local errors for unsupported inputs (naked `produce`, unhandled control flow) —
  a clear `ArgumentError` at construction beats a confusing failure in compiled IR.
- Internals may change freely. Public exports and the `produce`/`consume`/`copy` contract need
  tests, documentation, and stable errors. Keep `docs/src/internals.md` synchronized with
  documented internal names.
- Match the formatter and style configured by `.github/workflows/Format.yml` and
  `.JuliaFormatter.toml`. Install the configured formatter in an isolated environment rather
  than relying on a global installation.

## Concurrency

- `build_callable` is serialized under `build_callable_lock` (`ReentrantLock`) — deliberate:
  IR derivation mutates a shared `ID` counter (`seed_id!`/`ID()`) and the `MistyClosure` cache,
  and concurrent production derivation segfaulted (#227). Keep every production IR-deriving
  path and cache mutation inside this lock.
- Debugging and direct `BasicBlockCode` utilities are outside this concurrency guarantee and
  must not run concurrently with another ID-generating path unless they acquire the same
  lock. Treat ID generation as one critical section rather than relying on the counter's
  implementation for uniqueness.
- `copy(::TapedTask)` is `deepcopy`: the copy must share no task-owned mutable state, including
  captured `Ref`s and the position counter. Any change to state storage must preserve this;
  test that mutating a copy does not affect the source.

## Testing

- MWE first, then the smallest focused test, then broader groups. Prefer extending existing
  `Testcase`s over adding new ones; reduce the final regression to the smallest case that
  still fails without the fix.
- Register behaviour as `TestUtils.Testcase`s where possible — the driver iterates the task,
  `copy`s after every iteration, and checks every copy resumes to the same result sequence
  (plus optional allocation flags), exercising the copy/resume contract for free. Reserve
  bespoke `test/` code for what the driver can't express.
- Cover control-flow variety (branches, loops, nested produce, exception regions) and varied
  value types. Bug fixes land with a focused regression test; version/world-age fixes get an
  isolated direct test. Use the existing post-warmup allocation helper and `@code_warntype`
  for performance-sensitive paths.
- Never disable tests or weaken perf/allocation assertions to get CI green — ask first.
- `test/integration/turing/` runs in its own environment and is part of the contract: transform
  or semantics changes may need updates there even when core tests pass.

## Documentation

`docs/make.jl` drives Documenter; `index.md` is user-facing, `internals.md` indexes documented
internals. Update docstrings (and `internals.md` when adding/removing a documented internal)
when changing the public API, transform internals, or developer helpers.
