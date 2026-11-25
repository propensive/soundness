# One graph, three drawings

The three graph diagrams lay a graph out in different ways, and are used in the same way. Each
is made from the graph, and each has a `render` which takes a label for a node and gives the
lines.

```scala fuel
compile
alternative In lanes
  highlight LaneDagDiagram  as a version history is drawn
  code
      import strategies.throwUnsafely
      import laneDagStyles.boxDrawingLaneDagStyle, textMetrics.uniformMetric

      val drafts = Dag(t"Folio" -> Set(t"Quarto", t"prompt book"), t"Quarto" -> Set(t"papers"),
          t"prompt book" -> Set(t"papers"), t"papers" -> Set())

      val lines = LaneDagDiagram(drafts).render(draft => t" $draft")
alternative In layers
  highlight LayeredDagDiagram  by depth, with few crossings
  code
      import strategies.throwUnsafely
      import laneDagStyles.boxDrawingLaneDagStyle, textMetrics.uniformMetric

      val drafts = Dag(t"Folio" -> Set(t"Quarto", t"prompt book"), t"Quarto" -> Set(t"papers"),
          t"prompt book" -> Set(t"papers"), t"papers" -> Set())

      val lines = LayeredDagDiagram(drafts).render(draft => t" $draft")
alternative As a grid
  highlight DagDiagram  every edge drawn
  code
      import strategies.throwUnsafely
      import dagStyles.boxDrawingDagStyle, textMetrics.uniformMetric

      val drafts = Dag(t"Folio" -> Set(t"Quarto", t"prompt book"), t"Quarto" -> Set(t"papers"),
          t"prompt book" -> Set(t"papers"), t"papers" -> Set())

      val lines = DagDiagram(drafts).render(draft => t" $draft")
##
```

[Choosing a graph layout](../../soundness/modules/trees.md#choosing-a-graph-layout) says which
suits which graph, and [small APIs](../../philosophy/small-apis.md) why they are used alike.
