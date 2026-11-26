# A reply is read before its connection closes

`session` lends a connection to a block, and closes it when the block ends. The read side of
the connection is a stream which pulls from the socket, so returning it would leave the caller
with a stream over a closed socket. Draining it with `memoize` gives bytes which no longer depend
on anything.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A stream which would be read after the socket had closed
  errors show
  highlight connection.source
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import codepages.utf8Codepage

      val reply = Uds(t"/run/elsinore.sock").session: connection ?=>
        connection.send(Stream(t"Who's there?".in[Data]))
        connection.source
frame
  label The reply, read into memory inside the block
  highlight .memoize
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import codepages.utf8Codepage

      val reply: Data = Uds(t"/run/elsinore.sock").session: connection ?=>
        connection.send(Stream(t"Who's there?".in[Data]))
        connection.source.memoize
##
```

The example is compiled, and is not run, since it would open a connection.
[Sessions](../../soundness/modules/sockets.md#sessions) describes the scope.
