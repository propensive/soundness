# Socket options, stated once

The options of a socket are not arguments to `listen`. They are collected from scope, each
with a type which says which transports it is for, so an import sets the policy for every
socket bound beneath it.

```scala fuel
compile
frame
  label A listener with the platform's defaults
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def serve(): Unit =
        supervise:
          Port[Tcp]().listen[Data](connection => connection.source.memoize):
            ()
frame
  label One which may rebind its address at once
  highlight socketOptions.reuseAddressSocketOption  SO_REUSEADDR
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate
      import socketOptions.reuseAddressSocketOption

      def serve(): Unit =
        supervise:
          Port[Tcp]().listen[Data](connection => connection.source.memoize):
            ()
frame
  label And which has a larger buffer
  highlight given..65536)
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate
      import socketOptions.reuseAddressSocketOption

      given Socket.Option.ReceiveBuffer = socketOptions.receiveBuffer(65536)

      def serve(): Unit =
        supervise:
          Port[Tcp]().listen[Data](connection => connection.source.memoize):
            ()
##
```

The example is compiled, and is not run, since it would bind a port.
[Socket options](../../soundness/modules/sockets.md#socket-options) lists them all, and
[declarative context](../../philosophy/declarative-context.md) explains the approach.
