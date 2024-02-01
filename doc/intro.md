Many projects will find it useful to be able to define symbolic operators as
extension methods for a variety of different types. Unfortunately, such methods
do not always coexist happily, and overload resolution between different—but
like-named—extension methods do not happily coexist in the same project: the
compiler is often unable to disambiguate between different methods.  Contextual
resolution is, however, much more reliable, so _Symbolism_ provides a single
definition of each of the arithmetic and comparison operators, which defers
their implementation to typeclasses inferred from their parameter types.

