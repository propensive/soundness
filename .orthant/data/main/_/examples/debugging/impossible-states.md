# A frame is not a thread

The debug protocol names every thread, frame, method and object by a number. Each kind of
identifier has a type of its own here, a tagged view of one opaque integer, so the identifier
of a frame cannot be given where that of a thread is asked for.

```scala fuel
compile
frame
  label A frame's identifier, used to name a thread
  errors show
  code
      import strategies.throwUnsafely

      def names()(using Debug): List[Text] =
        debug.threads().bind: thread =>
          debug.frames(thread).map: (frame, location) =>
            debug.name(frame)
frame
  label The thread's own identifier
  highlight debug.name(thread)  a ThreadId, as the method asks
  code
      import strategies.throwUnsafely

      def names()(using Debug): List[Text] =
        debug.threads().bind: thread =>
          debug.frames(thread).map: (frame, location) =>
            debug.name(thread)
##
```

The example is compiled, and is not run, since a `Debug` exists only while another JVM is being
debugged. [The protocol vocabulary](../../soundness/modules/debugging.md#the-protocol-vocabulary)
lists the identifier types, and [impossible states](../../philosophy/impossible-states.md) the
idea behind them.
