# A thousand jobs on eight tasks

A task is a thread, and a thousand cheap jobs should not each have one. `concurrently` shares
numbered jobs among a fixed number of tasks, and returns the results in the order of the jobs
once every task has finished.

```scala fuel
compile
frame
  label The jobs
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def square(index: Int): Int = index*index
frame
  label Run eight at a time
  highlight 1000,  jobs, numbered from zero
  highlight 8)  tasks, each taking the next job
  code
      import threads.virtualThreads, probates.cancelProbate, strategies.throwUnsafely

      def square(index: Int): Int = index*index

      val squares = supervise(concurrently(1000, 8)(square))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(squares.to[List].excerpt(0, 5).inspect)
```

[Bounded parallelism](../../soundness/modules/concurrency.md#bounded-parallelism) explains how
the load balances itself.
