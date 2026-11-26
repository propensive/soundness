# Streams merged under a supervisor

Merging several streams runs a pump for each of them, on a thread of its own. A pump is a
task, and a task must belong to a scope, so a `Confluence` cannot be made where nothing
supervises it.

```scala fuel
compile
frame
  label Three streams, and nothing to supervise their pumps
  errors show
  code
      import threads.platformThreads, probates.cancelProbate
      import strategies.throwUnsafely

      val merged = Confluence(Stream(t"Viola"), Stream(t"Olivia"), Stream(t"Orsino")).memoize
frame
  label Within a supervised scope
  highlight supervise  the scope which owns the pumps
  code
      import threads.platformThreads, probates.cancelProbate
      import strategies.throwUnsafely

      val merged = supervise:
        Confluence(Stream(t"Viola"), Stream(t"Olivia"), Stream(t"Orsino")).memoize
##
```

[Combining and replicating streams](../../soundness/modules/streams.md#combining-and-replicating-streams)
describes the merge and its backpressure, and
[structured concurrency](../../philosophy/structured-concurrency.md) the rule.
