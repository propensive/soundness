# How deep a node sits

`layered` arranges a graph in layers for drawing, and a ranking decides the layer of each
node. The ranking is not an argument. An import chooses it, for every layering in its scope.

```scala fuel
compile
alternative The longest path
  code
      import strategies.throwUnsafely

      val deep = Dag(1 -> Set(), 5 -> Set(), 2 -> Set(1), 3 -> Set(2, 5))
      val layer: Optional[Int] = deep.layered.rank.at(5)
alternative Balanced
  highlight balancedRanking  for every layering in this scope
  code
      import strategies.throwUnsafely
      import rankings.balancedRanking

      val deep = Dag(1 -> Set(), 5 -> Set(), 2 -> Set(1), 3 -> Set(2, 5))
      val layer: Optional[Int] = deep.layered.rank.at(5)
##
```

Node 5 depends on nothing, so by default it is in the first layer. Under the balanced
ranking it sinks to the layer above node 3, the only node which depends on it.
[Layering](../../soundness/modules/graphs.md#layering) explains both, and
[declarative context](../../philosophy/declarative-context.md) the import.
