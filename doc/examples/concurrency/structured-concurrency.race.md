# The first to finish, and no stragglers

`race` gives the result of whichever task finishes first. The others are canceled, since their
results are no longer wanted, and the scope does not end until they have stopped.

```scala fuel
compile
frame
  label Two tasks
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val winner = supervise:
        val tortoise = async:
          snooze(5.0*Second)
          t"tortoise"

        val hare = async(t"hare")
        List(tortoise, hare)
frame
  label The first of them
  highlight race()  the loser is canceled in its snooze
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      val winner = supervise:
        val tortoise = async:
          snooze(5.0*Second)
          t"tortoise"

        val hare = async(t"hare")
        List(tortoise, hare).race()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(winner)
```

[Combining tasks](../../soundness/modules/concurrency.md#combining-tasks) has `sequence`
beside it, and [structured concurrency](../../philosophy/structured-concurrency.md) explains
why the loser is not left running.
