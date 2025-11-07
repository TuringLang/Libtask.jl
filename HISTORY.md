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
