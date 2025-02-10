constructs a new `:Dag` without the specified node and whose edges bypass that node

A new `Dag` is constructed removing the specified node and its incoming and outgoing edges. For each pair of
incoming edge removed and outgoing edge removed, a new edge is added from the source node of the incoming edge
to the destination node of the outgoing edge. This ensures that the reachability of the remaining nodes in the
`Dag` is retained.
