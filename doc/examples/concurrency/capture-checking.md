# A task cannot leave its supervisor

`supervise` does not return until its tasks have settled, so a task which is handed out of the
scope would be awaited under a supervisor which has gone. The task's type records the scope it
depends on, and the compiler refuses to let it out.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A task returned from its scope
  errors show
  highlight async(6*7)  the task is the result of the block
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val task = supervise(async(6*7))
frame
  label Its result returned instead
  highlight .await()  awaited where the supervisor still is
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val answer = supervise(async(6*7).await())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(answer.show)
```

[Spawning a task](../../soundness/modules/concurrency.md#spawning-a-task) explains how a task
finds its parent, and [capture checking](../../philosophy/capture-checking.md) how the compiler
follows it.
