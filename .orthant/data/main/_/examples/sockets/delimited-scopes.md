# A server which lasts as long as its block

`listen` takes a handler and a block. The socket is bound before the block starts and is
released when it ends, so a server which a piece of code needs exists for exactly that code.

```scala fuel
compile
frame
  label A handler for each connection
  highlight connection..memoize  echoes what it reads
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate, codepages.utf8Codepage

      def reply(): Unit =
        val socket = Uds(t"/run/elsinore.sock")

        supervise:
          socket.listen[Data](connection => connection.source.memoize):
            ()
frame
  label A client, inside the block
  highlight socket.session  the listener is running here, and nowhere else
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import threads.platformThreads, probates.awaitProbate, codepages.utf8Codepage

      def reply(): Data =
        val socket = Uds(t"/run/elsinore.sock")

        supervise:
          socket.listen[Data](connection => connection.source.memoize):
            socket.session: connection ?=>
              connection.send(Stream(t"Who's there?".in[Data]))
              connection.source.memoize
##
```

The example is compiled, and is not run, since it would bind a socket.
[Serving](../../soundness/modules/sockets.md#serving) has more, and
[delimited scopes](../../philosophy/delimited-scopes.md) the idea.
