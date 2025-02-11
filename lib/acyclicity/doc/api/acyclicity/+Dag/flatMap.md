constructs a new `:Dag` by transforming each node into a new subgraph

The lambda parameter transforms each node in the antecedent `:Dag` into a subgraph of nodes. Every incoming edge
to the antecedent node becomes an incoming edge to each node in mapped subgraph, and every outgoing edge from
the antecedent node becomes an outgoing edge to each node in the mapped subgraph, before redundant edges are
removed.
