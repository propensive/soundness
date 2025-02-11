The idea of a `Show` typeclass is well established as a better alternative to using `toString`, but it
is limited by its inability to distinguish different audiences. A fully-generic typeclass for
arbitrary different audiences is possible, but is usually better handled with different `Text` types
too. Spectacular's compromise of distinguishing with `Show` and `Debug` typeclasses will be sufficient
for most purposes, and has the distinction that `debug` will always provide _some_ `Text` value, while
`show` will require an appropriate `Show` instance to be provided.

