removes redundant edges from the `:Dag`

Calculates the _reduction_ of the DAG by removing any direct edges between nodes which already have indirect
edges between them.

For example,
```scala
Dag(1 -> 2, 2 -> 5, 5 -> 7, 1 -> 7)
```
would reduced to,
```scala
Dag(1 -> 2, 2 -> 5, 5 -> 7)
```
since the edge, `1 -> 7` is redundant.
