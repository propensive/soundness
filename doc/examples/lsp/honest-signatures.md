# A handler sees the payload of its own request

Inside a handler the request's payload is ambient: `document`, `position`, `newName`. Each
registration lends only what its request carries. Opening a document is not about a position,
so `position` in an `opened` handler does not compile.

```scala fuel
compile
frame
  label A position, where the request has none
  errors show
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        opened:
          client.logMessage(MessageType.Info, document.word(position).or(t"nothing"))
frame
  label A hover request has one
  highlight position  lent by hover, with the document
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        hover:
          document.word(position).let: word =>
            Hover(MarkupContent(value = t"**$word**"))
##
```

The example is compiled, and is not run, since a server speaks to an editor over standard input
and output. [Defining a server](../../soundness/modules/lsp.md#defining-a-server) has the table
of ambient names, and [honest signatures](../../philosophy/honest-signatures.md) the principle.
