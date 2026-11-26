# A deeper stack, for one block

The stack of a thread is fixed when the thread is made, and deep recursion needs a larger one.
The thread model is a contextual value, so one which makes threads with a stack of a given size
is bound for the block which needs it, and the tasks spawned there get it.

```scala fuel
compile
frame
  label A recursion too deep for an ordinary thread
  code
      import probates.cancelProbate, strategies.throwUnsafely
      import threads.virtualThreads

      def depth(count: Int): Int = if count == 0 then 0 else 1 + depth(count - 1)

      def deep(): Int = supervise(async(depth(400000)).await())
frame
  label Threads with a stack of 512 MiB, for this block
  highlight threads.stackThreads(512.mib)  the thread model, for this block only
  code
      import probates.cancelProbate, strategies.throwUnsafely

      def depth(count: Int): Int = if count == 0 then 0 else 1 + depth(count - 1)

      def deep(): Int =
        given Threading = threads.stackThreads(512.mib)
        supervise(async(depth(400000)).await())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(deep().show)
```

[Threads](../../soundness/modules/concurrency.md#threads) lists the thread models which are
imported, and [declarative context](../../philosophy/declarative-context.md) explains why one
can be bound as easily.
