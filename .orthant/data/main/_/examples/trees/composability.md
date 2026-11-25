# A history, drawn in lanes

The graph library has no drawing in it, and the diagrams are not made for any one graph. A
`Dag` is given to a `LaneDagDiagram` as it is, and comes out as the lanes of a version
history.

```scala fuel
compile
frame
  label A history which branches and merges
  code
      import strategies.throwUnsafely

      val drafts = Dag
        ( t"First Folio" -> Set(t"Second Quarto", t"prompt book"),
          t"Second Quarto" -> Set(t"foul papers"),
          t"prompt book" -> Set(t"foul papers"),
          t"foul papers" -> Set() )
frame
  label The same history, as a diagram
  highlight LaneDagDiagram(drafts)  the graph, as it is
  code
      import strategies.throwUnsafely
      import laneDagStyles.boxDrawingLaneDagStyle, textMetrics.uniformMetric

      val drafts = Dag
        ( t"First Folio" -> Set(t"Second Quarto", t"prompt book"),
          t"Second Quarto" -> Set(t"foul papers"),
          t"prompt book" -> Set(t"foul papers"),
          t"foul papers" -> Set() )

      val lanes = LaneDagDiagram(drafts).render(draft => t" $draft")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.basicTermcap

lanes.each(Out.println(_))
```

[Graphs](../../soundness/modules/trees.md#graphs) has the other layouts, and
[composability](../../philosophy/composability.md) the reason nothing joins the two.
