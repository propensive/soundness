constructs a subgraph of the antecedent `:Dag` including only the nodes in the specified `Set` 

This is equivalent to calling `filter` on the `Dag` with a predicate that includes only elements in the
specified `Set`.

Any elements in the `Set` which do not exist in the `Dag` are ignored.
