# A task needs a supervisor

A task belongs to the scope which started it, and that scope does not end until the task has.
Where there is no such scope, there is nothing for a task to belong to, and `async` does not
compile.

```scala fuel
compile
frame
  label A task with no supervisor
  errors show
  code
      import threads.virtualThreads

      val task = async(6*7)
frame
  label A task within a supervised scope
  focus supervise:..await()
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val answer = supervise:
        val task = async(6*7)
        task.await()
##
```

[A supervised scope](../../soundness/modules/concurrency.md#a-supervised-scope) describes what
happens to tasks which are still running when the scope ends.
