# The kind of thread is an import

A task runs on whatever the thread model in scope provides. The code which spawns and awaits
is the same on platform threads, on virtual threads and on a pool of carriers, and one import
moves a whole file from one to another.

```scala fuel
compile
alternative Virtual threads
  highlight virtualThreads  a virtual thread for each task
  code
      import threads.virtualThreads
      import probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        List(async(154), async(126), async(37)).sequence.await()
alternative Platform threads
  highlight platformThreads  a platform thread for each task
  code
      import threads.platformThreads
      import probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        List(async(154), async(126), async(37)).sequence.await()
alternative A pool
  highlight pooledThreads  an idle carrier, where one is waiting
  code
      import threads.pooledThreads
      import probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        List(async(154), async(126), async(37)).sequence.await()
##
```

[Threads](../../soundness/modules/concurrency.md#threads) compares the models, and
[declarative context](../../philosophy/declarative-context.md) explains why the choice is not
an argument of `async`.
