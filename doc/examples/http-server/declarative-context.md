# The same handler, on another front-end

How connections are served is not part of a handler. A `Frontend` chosen by an import decides
whether each connection has a virtual thread of its own, or whether a few selector threads
serve them all.

```scala fuel
compile
alternative A thread for each connection
  highlight frontends.threadPerConnectionFrontend  a handler may block
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage, frontends.threadPerConnectionFrontend

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"All the world's a stage")
alternative An event loop
  highlight frontends.reactiveFrontend  a handler must not block
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage, frontends.reactiveFrontend

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"All the world's a stage")
##
```

The example is compiled, and is not run, since it would bind a port.
[Front-ends](../../soundness/modules/http-server.md#front-ends) compares the two, and
[declarative context](../../philosophy/declarative-context.md) explains the approach.
