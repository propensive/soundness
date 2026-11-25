# A proxy amends a reply by making another

A proxy sits between an editor and a real language server, and may change what passes. A reply
is decoded to the same immutable case class which a server would have returned, and a rewriter
is a function from one such value to the next.

```scala fuel
compile
frame
  label Every hover is marked
  highlight hover.copy  a new Hover; the server's own is untouched
  code
      import Lsp.*

      def amendments()(using Lsp.Proxy): Unit =
        rewrite.hover: hover =>
          hover.copy(contents = MarkupContent(value = t"_(aside)_ ${hover.contents.value}"))
frame
  label Hints are filtered out of the diagnostics
  highlight _.filter  a list in, a list out
  code
      import Lsp.*

      def amendments()(using Lsp.Proxy): Unit =
        rewrite.hover: hover =>
          hover.copy(contents = MarkupContent(value = t"_(aside)_ ${hover.contents.value}"))

        rewrite.diagnostics(_.filter(_.severity != DiagnosticSeverity.Hint))
frame
  label The server's capabilities are amended too
  highlight _.copy(hoverProvider..true)
  code
      import Lsp.*

      def amendments()(using Lsp.Proxy): Unit =
        rewrite.hover: hover =>
          hover.copy(contents = MarkupContent(value = t"_(aside)_ ${hover.contents.value}"))

        rewrite.diagnostics(_.filter(_.severity != DiagnosticSeverity.Hint))
        rewrite.capabilities(_.copy(hoverProvider = true))
##
```

The example is compiled, and is not run, since a proxy launches a server and speaks to an
editor. [Proxying a server](../../soundness/modules/lsp.md#proxying-a-server) lists what can be
rewritten, and [immutability](../../philosophy/immutability.md) why a change is a new value.
