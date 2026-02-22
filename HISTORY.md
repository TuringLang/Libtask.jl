# 0.9.13

Fix a bug where SSA registers in `throw_undef_if_not` expressions were not being correctly handled.
This affected `TapedTask`s where the underlying function had variables that were conditionally defined.

Also adds an internal function, `Libtask._generate_ir`, which is useful for debugging.
This is not part of the public API; however if you are developing Libtask it is a useful function for quickly inspecting the IR that Libtask generates, both pre- and post-optimisation.

# 0.9.12

Added a new method, `Libtask.get_taped_globals(tt::TapedTask)`.
This method extracts the taped globals stored inside `tt`.

Note that this is distinct from the existing method `Libtask.get_taped_globals(::Type{T}) where {T}`.
That method is meant for being called *inside* a TapedTask, and it extracts the taped globals from the task that it is currently inside.

This PR also improves the error message for the latter method.
When calling `Libtask.get_taped_globals(::Type{T}) where {T}` outside of a TapedTask, this now throws a `Libtask.NotInTapedTaskError` (which can be specifically caught) and shows a better error message.

# 0.9.11

When constructing a `TapedTask` with a method that will error when run (e.g. method doesn't exist, or is ambiguous) a more helpful error is shown.

# 0.9.10

Fix a bug introduced in 0.9.9 that made certain phi nodes with Union types fail a type assertion.

# 0.9.9

Remove manual opaque closure optimisation functions in favour of setting the world age and letting the compiler do more work for us, and providing it with some more type information. This changes no functionality, and shouldn't change performance either, but simplifies code.

# 0.9.8

Enables built docs for the current release version of Libtask.

# 0.9.7

Fix a concurrency bug, where Libtask would sometimes crash with a "Multiple concurrent writes to Dict detected!" error when TapedTasks were being executed concurrently.

# 0.9.6

Add support for Julia v1.12.

# 0.9.0

From version 0.9.0, the old `TArray` and `TRef` types are completely removed, where previously they were only deprecated. Additionally, the internals have been completely overhauled, and the public interface more precisely defined. See the docs for more info.

# 0.6.0

From v0.6.0 Libtask is implemented by recording all the computing to a tape and copying that tape. Before that version, it is based on a tricky hack on the Julia internals. You can check the commit history of this repo to see the details.
