# Parallel code keeps its shape

Two computations run one after the other become two which run together by wrapping each in
`async` and asking each for its result with `await`. The expression which combines them does
not change, and no `Future` is mapped over to reach it.

```scala fuel
compile
frame
  label One after the other
  code
      def count(play: Text): Int = play.length

      val total =
        val first = count(t"Twelfth Night")
        val second = count(t"King Lear")
        first + second
frame
  label Each in a task of its own
  highlight async  starts at once, on its own thread
  line 6
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def count(play: Text): Int = play.length

      val total = supervise:
        val first = async(count(t"Twelfth Night"))
        val second = async(count(t"King Lear"))
        first.await() + second.await()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(total.show)
```

[Spawning a task](../../soundness/modules/concurrency.md#spawning-a-task) describes `async`
and `await`, and [direct style](../../philosophy/direct-style.md) why waiting is written as an
ordinary call.
