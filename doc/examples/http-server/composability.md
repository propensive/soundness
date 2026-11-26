# A JSON value is a response

The body of a response is any value which can be served, and its type supplies the
`Content-Type`. Text is served as `text/plain`. With one import a `Json` value is served as
`application/json`, and a case class becomes one with `in[Json]`.

```scala fuel
compile
frame
  label Text, served as text/plain
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage

      case class Play(title: Text, acts: Int)

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"Hamlet")
frame
  label A value, served as application/json
  highlight servables.jsonServable
  highlight in[Json]
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage, servables.jsonServable
      import formatting.compactJsonFormatting

      case class Play(title: Text, acts: Int)

      def serve(): Unit = supervise:
        SocketServer(8080).handle:
          Http.Response(Http.Ok)(Play(t"Hamlet", 5).in[Json])
##
```

The example is compiled, and is not run, since it would bind a port.
[Responses](../../soundness/modules/http-server.md#responses) lists the media type of each
kind of body, and [composability](../../philosophy/composability.md) explains why no adapter
is needed.
