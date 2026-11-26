# What becomes of an unfinished child

A task does not finish until its children have, so a parent which returns early leaves a
question: what should happen to a child which is still running? The probate in scope answers
it, for every task beneath the import.

```scala fuel
compile
alternative It is canceled
  highlight cancelProbate
  code
      import threads.virtualThreads, strategies.throwUnsafely
      import probates.cancelProbate

      val scene = supervise:
        val parent = async:
          async(snooze(0.5*Second))
          t"Exeunt"

        parent.await()
alternative It is waited for
  highlight awaitProbate
  code
      import threads.virtualThreads, strategies.throwUnsafely
      import probates.awaitProbate

      val scene = supervise:
        val parent = async:
          async(snooze(0.5*Second))
          t"Exeunt"

        parent.await()
alternative It is an error
  highlight failProbate
  code
      import threads.virtualThreads, strategies.throwUnsafely
      import probates.failProbate

      val scene = supervise:
        val parent = async:
          async(snooze(0.5*Second))
          t"Exeunt"

        parent.await()
##
```

The example is compiled, and is not run, since it pauses. Under each choice, `parent.await()`
returns only when the child has settled.
[Cancellation in both directions](../../soundness/modules/concurrency.md#cancellation-in-both-directions)
says which probate suits which kind of work, and
[structured concurrency](../../philosophy/structured-concurrency.md) why the parent waits.
