# A capability is advertised by implementing it

A language server tells the editor what it can do, and then has to do it. Here there is no
record of capabilities to fill in. Registering a handler is what advertises its capability, so a
server cannot claim a feature it lacks, or implement one it forgot to announce.

```scala fuel
compile
frame
  label A server which can do nothing
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit = ()
frame
  label It hovers, and says that it does
  highlight hover:  hoverProvider is advertised from here
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        hover:
          document.word(position).let: word =>
            Hover(MarkupContent(value = t"**$word**"))
frame
  label It completes, after a colon
  highlight complete(t":"):  the trigger character belongs to the registration
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        hover:
          document.word(position).let: word =>
            Hover(MarkupContent(value = t"**$word**"))

        complete(t":"):
          CompletionList(items = List(CompletionItem(label = t"Exeunt")))
##
```

The example is compiled, and is not run, since a server speaks to an editor over standard input
and output. [Defining a server](../../soundness/modules/lsp.md#defining-a-server) has a whole
server, and [impossible states](../../philosophy/impossible-states.md) the idea of leaving a
wrong state with no way to be written.
