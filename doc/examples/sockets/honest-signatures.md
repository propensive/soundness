# A connection to another machine needs the internet

A Unix domain socket is a file on the same machine, and connecting to it asks for nothing more
than a backend. An endpoint with an IP address is somewhere else, and the code which connects
to it must have been given the network.

```scala fuel
compile
frame
  label A socket on this machine
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import codepages.utf8Codepage

      val greeting: Data = Uds(t"/run/elsinore.sock").session: connection ?=>
        connection.send(Stream(t"Who's there?".in[Data]))
        connection.source.memoize
frame
  label An address, with no network in scope
  errors show
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import codepages.utf8Codepage

      val greeting: Data = (ip"10.0.16.4" on Port[Tcp](8080)).session: connection ?=>
        connection.send(Stream(t"Who's there?".in[Data]))
        connection.source.memoize
frame
  label The network, granted by an import
  highlight internetAccess.online
  code
      import socketBackends.javaBaseSockets, strategies.throwUnsafely, logging.silentLogging
      import codepages.utf8Codepage, internetAccess.online

      val greeting: Data = (ip"10.0.16.4" on Port[Tcp](8080)).session: connection ?=>
        connection.send(Stream(t"Who's there?".in[Data]))
        connection.source.memoize
##
```

The example is compiled, and is not run, since it would open a connection.
[Sessions](../../soundness/modules/sockets.md#sessions) describes `session`, and
[honest signatures](../../philosophy/honest-signatures.md) why the network is asked for.
