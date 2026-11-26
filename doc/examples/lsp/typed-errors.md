# A command which cannot be run says why

A handler which cannot answer raises an `Lsp.Error` with one of the protocol's reasons. Each
handler is lent somewhere to raise it, and the server turns it into a JSON-RPC error response
with the wire code of the reason and the id of the request. No handler builds that response.

```scala fuel
compile
frame
  label A command which always answers
  code
      import Lsp.*, errorDiagnostics.stackTracesDiagnostics

      def features()(using Lsp.Registry): Unit =
        command(t"globe.raiseCurtain"):
          Unset
frame
  label And one which refuses
  highlight raise
  highlight Lsp.Error.Reason.RequestFailed  wire code -32803
  code
      import Lsp.*, errorDiagnostics.stackTracesDiagnostics

      def features()(using Lsp.Registry): Unit =
        command(t"globe.raiseCurtain"):
          raise(Lsp.Error(Lsp.Error.Reason.RequestFailed, t"the theatre is closed"))
          Unset
##
```

The example is compiled, and is not run, since a server speaks to an editor over standard input
and output. [Reporting errors](../../soundness/modules/lsp.md#reporting-errors) describes what
becomes of a fault raised for a request, and for a notification.
