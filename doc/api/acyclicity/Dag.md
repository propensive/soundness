an immutable representation of a directed acyclic graph of nodes and the edges between them

Internally, the edges are stored as a `:Map` from each node to its `:Set` of dependents.
