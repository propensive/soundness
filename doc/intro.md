Scala's `Option` is the traditional way to represent values which may be either
_present_ (in which case a value is specified) or _absent_. `Option` is a
simple ADT, but union types, along with some helper methods, can provide much
better ergonomics in most circumstances. _Vacuous_ offers an `Optional` type
which provides this functionality.
