# Anything with the shape of a graph

A `Dag`, a `Digraph` and a plain `Map` from nodes to their successors are different types with
one vocabulary. `sources`, `successors` and `reachable` are asked of each in the same words,
since each is `Nodal`.

```scala fuel
compile
alternative A `Dag`
  highlight Dag  checked for cycles
    occurrence 1
  code
      import strategies.throwUnsafely

      val histories = Dag
        ( t"Henry V" -> Set(t"Henry IV"),
          t"Henry IV" -> Set(t"Richard II"),
          t"Richard II" -> Set() )

      val first: Set[Text] = histories.sources
      val before: Set[Text] = histories.reachable(t"Henry V")
alternative A `Digraph`
  highlight Digraph  cycles allowed
  code
      import strategies.throwUnsafely

      val histories = Digraph
        ( t"Henry V" -> Set(t"Henry IV"),
          t"Henry IV" -> Set(t"Richard II"),
          t"Richard II" -> Set() )

      val first: Set[Text] = histories.sources
      val before: Set[Text] = histories.reachable(t"Henry V")
alternative A `Map`
  highlight Map  an ordinary collection
  code
      import strategies.throwUnsafely

      val histories = Map
        ( t"Henry V" -> Set(t"Henry IV"),
          t"Henry IV" -> Set(t"Richard II"),
          t"Richard II" -> Set() )

      val first: Set[Text] = histories.sources
      val before: Set[Text] = histories.reachable(t"Henry V")
##
```

[Graphs](../../soundness/modules/graphs.md#about) introduces the typeclass, and
[small APIs](../../philosophy/small-apis.md) the reason for one vocabulary.
