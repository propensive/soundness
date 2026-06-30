# Zero Cost

_(A suggested addition — not on the original list.)_

Soundness pays for its safety at compiletime, not at runtime. Opaque types, inlining,
and type-level computation mean that the guarantees a type carries — the units on a
quantity, a validated name, a checked path — usually compile down to the bare value they
wrap, with no wrapper object allocated and no check left to run. Safety is therefore not
a tax on performance: the very code that is provably correct is also as fast as the
unchecked version a careless programmer would have written by hand.
