# 0.9.6

Add support for Julia v1.12.

# 0.9.0

From version 0.9.0, the old `TArray` and `TRef` types are completely removed, where previously they were only deprecated. Additionally, the internals have been completely overhauled, and the public interface more precisely defined. See the docs for more info.

# 0.6.0

From v0.6.0 Libtask is implemented by recording all the computing to a tape and copying that tape. Before that version, it is based on a tricky hack on the Julia internals. You can check the commit history of this repo to see the details.
