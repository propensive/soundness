When writing macros or generic type-level code, we often need a way to
statically represent collections of values, without them to existing at runtime
and requiring allocation on the heap. Scala's `Tuple` type provides a generic
way of representing a heterogeneous sequence of values at the type-level, but
the inline or macro code that's necessary to work with them—in particular, to
decompose them into values—can be intricate, fragile and repetitive. _Typonym_
makes it easy to convert between value-level and type-level representations of
three collection types, `List`s, `Set`s and `Map`s, as well as straigtforward
conversions of singleton literal types.

