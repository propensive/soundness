# One `listen` for every transport

A TCP port, a UDP port and a Unix domain socket are all served with `listen`. The type of the
socket value chooses the transport, and with it what the handler receives: a connection, or a
packet.

```scala fuel
compile
alternative TCP
  highlight Port[Tcp]()
  highlight connection  a stream to read, and a reply to send
    occurrence 1
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def serve(): Unit =
        supervise:
          Port[Tcp]().listen[Data](connection => connection.source.memoize):
            ()
alternative UDP
  highlight Port[Udp]()
  highlight packet  a datagram and its sender
    occurrence 1
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def serve(): Unit =
        supervise:
          Port[Udp]().listen[Data](packet => UdpResponse.Reply(packet.data)):
            ()
alternative Unix domain
  highlight Uds(t"/run/elsinore.sock")
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate

      def serve(): Unit =
        supervise:
          Uds(t"/run/elsinore.sock").listen[Data](connection => connection.source.memoize):
            ()
##
```

The example is compiled, and is not run, since it would bind sockets.
[Serving](../../soundness/modules/sockets.md#serving) has the table of transports, and
[small APIs](../../philosophy/small-apis.md) the reason there is one name.
