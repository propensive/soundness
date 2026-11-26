# Nothing spawns a task once the scope has ended

A function which starts a task needs a supervisor when it is called, not when it is written. If
such a function could be returned from `supervise`, it would spawn into a scope which has
already finished, so the compiler keeps the function inside the scope as firmly as it keeps a
task there.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which would spawn later
  errors show
  highlight async(6*7)  spawns when the function is called
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val spawn = supervise(() => async(6*7))
frame
  label Two tasks, spawned and awaited within the scope
  focus supervise:..right.await()
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val sum = supervise:
        val left = async(6*7)
        val right = async(7*8)
        left.await() + right.await()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sum.show)
```

[A supervised scope](../../soundness/modules/concurrency.md#a-supervised-scope) describes the
scope, and [structured concurrency](../../philosophy/structured-concurrency.md) why a task is
never without one.
