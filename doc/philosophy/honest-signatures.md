# Honest Signatures

A signature in Soundness tells the whole truth about what an operation needs. The
capabilities and contextual values an implementation depends on are declared in the
signature of its `given`, not acquired silently behind the scenes, so the real
requirements of a piece of code are visible where it is defined. Typeclass definitions,
in turn, are kept minimal: a typeclass describes the single capability it abstracts and
no more, rather than swelling to anticipate every possible implementation. A narrow,
honest signature is both easier to satisfy and easier to trust, because nothing it
requires is hidden and nothing it promises is unused.
