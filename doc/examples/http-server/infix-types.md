# A WebSocket message, typed by its carrier

A WebSocket handler takes each message as a value. `Cue over Json` is a `Cue` which arrived as
JSON, and the reply is made in the same words, so the frames of the protocol never appear.

```scala fuel
compile
frame
  label A message
  highlight Cue..Json  decoded from each frame as it arrives
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import formatting.compactJsonFormatting

      case class Cue(line: Int)

      def prompt(): Unit = supervise:
        SocketServer(8080).handle:
          webSocket(): (cue: Cue over Json) =>
            Control.Continue(())
frame
  label And its reply
  highlight over[Json]
  code
      import threads.virtualThreads, probates.awaitProbate, logging.silentLogging
      import strategies.throwUnsafely, webserverErrorPages.minimalErrorPage
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import formatting.compactJsonFormatting

      case class Cue(line: Int)

      def prompt(): Unit = supervise:
        SocketServer(8080).handle:
          webSocket(): (cue: Cue over Json) =>
            Control.Reply(Cue(cue.line + 1).over[Json], ())
##
```

The example is compiled, and is not run, since it would bind a port.
[WebSockets](../../soundness/modules/http-server.md#websockets) has the table of answers, and
[infix types](../../philosophy/infix-types.md) explains `over`.
