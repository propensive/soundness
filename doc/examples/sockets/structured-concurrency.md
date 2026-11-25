# A listener needs a supervisor

`listen` runs a handler on a task of its own for each connection. Those tasks must belong to
something, so a listener cannot be started where nothing supervises it.

```scala fuel
compile
frame
  label A listener which nothing supervises
  errors show
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def echo(): Unit =
        Port[Tcp]().listen[Data](connection => connection.source.memoize):
          ()
frame
  label A listener in a supervised scope
  highlight supervise  every handler's task ends before this block does
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def echo(): Unit =
        supervise:
          Port[Tcp]().listen[Data](connection => connection.source.memoize):
            ()
##
```

The example is compiled, and is not run, since it would bind a port.
[Serving](../../soundness/modules/sockets.md#serving) describes `listen`, and
[structured concurrency](../../philosophy/structured-concurrency.md) the rule.
