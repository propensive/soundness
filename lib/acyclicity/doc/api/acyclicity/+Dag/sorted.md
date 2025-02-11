returns a topologically-sorted `:List` of all the nodes in the `Dag`

Performs a topological sort whereby each element appears before all its dependents; or conversely, each element
appears after all of its dependencies. In general, this order is not unique.
