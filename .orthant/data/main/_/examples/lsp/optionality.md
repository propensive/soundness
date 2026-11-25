# A hover with nothing under the cursor

A request to hover names a position, and the position may be on a word or on white space. So
`document.word` returns an `Optional[Text]`, and a hover handler returns an `Optional[Hover]`:
where there is no word, there is no hover, and neither case needs a `null` or an empty reply.

```scala fuel
compile
frame
  label A word is taken for granted
  errors show
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        hover:
          Hover(MarkupContent(value = document.word(position)))
frame
  label A hover where there is a word, and none where there is not
  highlight let  an absent word is an absent hover
  code
      import Lsp.*

      def features()(using Lsp.Registry): Unit =
        hover:
          document.word(position).let: word =>
            Hover(MarkupContent(value = t"**$word**"))
##
```

The example is compiled, and is not run, since a server speaks to an editor over standard input
and output. [Defining a server](../../soundness/modules/lsp.md#defining-a-server) describes the
handlers, and [optionality](../../philosophy/optionality.md) the type their results share.
