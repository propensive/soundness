# A server belongs to a scope

A server handles each connection on a task of its own, and those tasks need a supervisor. A
server started inside `supervise` lasts as long as that block, and one started anywhere else
does not compile.

```scala fuel
compile
frame
  label A server which nothing supervises
  errors show
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      def serve(): Unit =
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"All the world's a stage")
frame
  label A server in a supervised scope
  highlight supervise  does not end while the server is running
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"All the world's a stage")
##
```

The example is compiled, and is not run, since it would bind a port.
[A server](../../soundness/modules/http-server.md#a-server) says how a server is stopped, and
[structured concurrency](../../philosophy/structured-concurrency.md) why it must be
supervised.
