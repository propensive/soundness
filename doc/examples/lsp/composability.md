# A reply the library does not model, rewritten as JSON

A proxy forwards every method, including those which have no Scala type here. The result of any
of them can still be amended: `rewrite.result` names the method and is given its result as
`Json`, so the whole of the [JSON](../../soundness/modules/json.md) library applies to it.

```scala fuel
compile
frame
  label A typed reply, for a method which is modeled
  highlight rewrite.completions  a CompletionList
  code
      import Lsp.*

      def amendments()(using Lsp.Proxy): Unit =
        rewrite.completions(_.copy(isIncomplete = false))
frame
  label A JSON reply, for one which is not
  highlight rewrite.result  any method, by name
  highlight as[List[Text]]  decoded
  highlight in[Json]  and encoded again
  code
      import Lsp.*

      def amendments()(using Lsp.Proxy): Unit =
        rewrite.completions(_.copy(isIncomplete = false))

        rewrite.result(t"globe/cast"): json =>
          json.as[List[Text]].filter(_ != t"Ghost").in[Json]
##
```

The example is compiled, and is not run, since a proxy launches a server and speaks to an
editor. [Proxying a server](../../soundness/modules/lsp.md#proxying-a-server) has the table of
combinators, and [composability](../../philosophy/composability.md) the reason two libraries
meet without an adapter.
