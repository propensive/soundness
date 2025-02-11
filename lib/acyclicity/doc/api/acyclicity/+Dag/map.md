transforms the graph from a `:Dag[T]` into a `:Dag[S]` by applying the lambda to each node

Produces a new `:Dag` of identical edge and node structure to the antecedent `Dag`, but where the lambda has been
applied to each node.
