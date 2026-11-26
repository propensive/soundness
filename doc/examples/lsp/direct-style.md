# A question put to a language server

The protocol is asynchronous: requests and responses are matched by an id, and may cross. The
client hides that. Each request is a method which returns its typed answer, so a conversation
with a server reads from top to bottom.

```scala fuel
compile
frame
  label A server is launched, and greeted
  highlight session:  the process lives as long as this block
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def explain(script: Text): Unit = supervise:
        Lsp.Server(sh"rust-analyzer").session: server ?=>
          server.initialize(root = t"file:///globe")
          server.initialized()
frame
  label It is shown a document
  highlight server.open  a notification: nothing comes back
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def explain(script: Text): Unit = supervise:
        Lsp.Server(sh"rust-analyzer").session: server ?=>
          server.initialize(root = t"file:///globe")
          server.initialized()
          server.open(t"file:///globe/hamlet.rs", t"rust", script)
frame
  label And asked about a position in it
  highlight Optional[Lsp.Hover]  the answer, not a future of it
  highlight server.hover  a request: this line waits for the response
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import threads.virtualThreads, probates.awaitProbate
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      def explain(script: Text): Optional[Lsp.Hover] = supervise:
        Lsp.Server(sh"rust-analyzer").session: server ?=>
          server.initialize(root = t"file:///globe")
          server.initialized()
          server.open(t"file:///globe/hamlet.rs", t"rust", script)
          server.hover(t"file:///globe/hamlet.rs", Lsp.Position(3, 1))
##
```

The example is compiled, and is not run, since it launches a language server.
[Talking to a server](../../soundness/modules/lsp.md#talking-to-a-server) has the other
requests, and [direct style](../../philosophy/direct-style.md) what makes this possible.
