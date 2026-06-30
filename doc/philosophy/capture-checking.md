# Capture Checking

Soundness uses Scala's capture checking to strengthen the guarantees that its
[scoped capabilities](delimited-scopes.md) already express. Capture checking tracks
which capabilities a value depends on, so the compiler can prove that a resource, an
error handler, or a concurrency context does not escape the block that established it.
What scoping states by structure, capture checking enforces by type: a lazy value, a
stream, or a closure cannot smuggle a capability out of the scope in which it was
valid, and an attempt to do so is a compile error rather than a runtime fault.
