Mercator makes it possible to abstract over monads and functors, automatically
constructing contextual instances for types with the requisite methods.

This allows generic implementations of `sequence` and `traverse` to be provided
for all types which define `map` and `flatMap`, provided an appropriate
implementation of `Point` (providing the monadic "unit" operation) can be found.

