# Many tasks, awaited as one

A list of tasks becomes a task of a list with `sequence`. The tasks run in parallel, the
results come back in the order the tasks were written, and all of it is over before
`supervise` returns.

```scala fuel
compile
frame
  label Three tasks
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def count(play: Text): Int = play.length

      val counts = supervise:
        val plays = List(t"Hamlet", t"Othello", t"Cymbeline")
        plays.map(play => async(count(play)))
frame
  label One task of three results
  highlight sequence
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def count(play: Text): Int = play.length

      val counts = supervise:
        val plays = List(t"Hamlet", t"Othello", t"Cymbeline")
        plays.map(play => async(count(play))).sequence
frame
  label The results
  highlight await()  a list, in the order of the plays
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def count(play: Text): Int = play.length

      val counts = supervise:
        val plays = List(t"Hamlet", t"Othello", t"Cymbeline")
        plays.map(play => async(count(play))).sequence.await()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(counts.inspect)
```

[Combining tasks](../../soundness/modules/concurrency.md#combining-tasks) has `race`, `map`
and `bind` beside it.
