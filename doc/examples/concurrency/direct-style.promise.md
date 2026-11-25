# A value which arrives later

A `Promise` is a value which another task will supply. The side which wants it calls `await`,
and gets it as the result of that call: there is no callback to register and nothing to map
over.

```scala fuel
compile
frame
  label A promise
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        val promise = Promise[Int]()
        promise
frame
  label Fulfilled elsewhere
  highlight fulfill(154)  on another task
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        val promise = Promise[Int]()
        daemon(promise.fulfill(154))
        promise
frame
  label And awaited here
  highlight await()  waits, and gives the value
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val lines = supervise:
        val promise = Promise[Int]()
        daemon(promise.fulfill(154))
        promise.await()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lines.show)
```

[Promises](../../soundness/modules/concurrency.md#promises) describes them, and
[direct style](../../philosophy/direct-style.md) why waiting is an ordinary call.
