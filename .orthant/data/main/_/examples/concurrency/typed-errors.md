# An error raised in a task arrives where it is awaited

A task's type records the errors its body can raise, and `await` raises them again in the
task which is waiting. An error which happens on another thread is therefore handled, or
declared, like any other.

```scala fuel
compile
frame
  label Nothing handles a number which does not parse
  errors show
  code
      import threads.virtualThreads, probates.cancelProbate

      def port(setting: Text): Int raises Async.Error = supervise:
        val task = async(setting.as[Int])
        task.await()
frame
  label Both failures, as an absent value
  highlight safely
  code
      import threads.virtualThreads, probates.cancelProbate

      def port(setting: Text): Optional[Int] = safely:
        supervise:
          val task = async(setting.as[Int])
          task.await()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(port(t"8080").inspect)
Out.println(port(t"eighty").inspect)
```

[Errors](../../soundness/modules/errors.md) describes how a failure in a task is handled, and
[error handling](../../philosophy/error-handling.md) why it is in the type.
