traverses the `:Dag` in topologically-sorted order, applying the function `fn` to each node plus the `:Set` of
result of applying `fn` to each of that node's dependencies

This is a useful convenience for handling dataflow through a graph, where each node represents some processing
that must be done on each node, and where that processing also depends on the results from previous nodes. The
result of the `traversal` method is a `:Map[T, S]`, from nodes to their resultant values.