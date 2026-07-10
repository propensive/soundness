# Capture Checking

Soundness uses Scala's capture checking to strengthen the guarantees that its
[scoped capabilities](delimited-scopes.md) already express. Capture checking tracks
which capabilities a value depends on, so the compiler can prove that a resource, an
error handler, or a concurrency context does not escape the block that established it.
What scoping states by structure, capture checking enforces by type.

What becomes impossible is the family of escape bugs that scoping alone cannot stop —
each one a value smuggling a dead capability out of its block:

```scala
// a lazy stream escaping the file that feeds it
val lines = file.open(_.stream[Text])
lines.head   // without capture checking: reads from a closed handle

// a closure escaping its error handler
val f = safely(() => parse(input))
f()          // without capture checking: raises with no handler in scope

// a task escaping its supervision
val task = supervise(async(compute()))
task.await() // without capture checking: awaits under a completed supervisor
```

Under capture checking, each of these is a compile error: the stream captures the open
handle, the closure captures the `Tactic`, the task captures the `Monitor`, and none of
those capabilities may outlive its scope. The fix the compiler forces is the right one —
consume the stream inside the block, run the fallible code where its handler lives,
await the task under its supervisor.

The guarantee runs deeper than discipline could: laziness, closures and concurrency are
exactly the features that defeat by-eye reasoning about lifetimes, because they detach
*when* code runs from *where* it was written. Capture checking reattaches them — the
capability's scope travels in the type, so an escape is impossible to write rather than
inadvisable to attempt.
