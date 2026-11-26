# Routing is a pattern match

A handler is a block which gives a response, and the request it answers is in scope as
`request`. Choosing a response by the request's method and target needs no routing table,
only a `match`.

```scala fuel
compile
frame
  label One response to every request
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"All the world's a stage")
frame
  label Routed on the target
  highlight request.target
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          request.target match
            case t"/"      => Http.Response(Http.Ok)(t"All the world's a stage")
            case t"/exit"  => Http.Response(Http.Ok)(t"Pursued by a bear")
            case _         => Http.Response(Http.NotFound)(t"No such scene")
frame
  label And on the method
  highlight request.method
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          (request.method, request.target) match
            case (Http.Get, t"/")      => Http.Response(Http.Ok)(t"All the world's a stage")
            case (Http.Post, t"/exit") => Http.Response(Http.Ok)(t"Pursued by a bear")
            case _                     => Http.Response(Http.NotFound)(t"No such scene")
##
```

The example is compiled, and is not run, since it would bind a port.
[A server](../../soundness/modules/http-server.md#a-server) describes the handler.
