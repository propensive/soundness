# Honest Signatures

A signature in Soundness tells the truth, the whole truth, and nothing but the truth
about what an operation needs. The most important part is the *nothing but*: a
signature declares no requirement unless it is always needed. A typeclass describes the
single capability it abstracts, without widening its methods to accommodate what some
implementation might want — no context parameter "just in case", no error declared
because one implementation can fail.

Requirements that only certain implementations have belong on those implementations. A
`given` declares in its own signature the capabilities its particular implementation
depends on — a network-backed instance requires `Online`, a fallible one a `Tactic` —
and the requirement is discharged where the instance is chosen, not imposed on the
typeclass and every other implementation of it. So the effective requirements of a
method are determined by the instances it actually uses: the same generic code is pure
with a pure instance and networked with a networked one, and its own signature claims
neither.

The result is signatures that can be trusted in both directions. Nothing an operation
requires is hidden, so a caller sees the true cost; and nothing it declares is unused,
so a requirement in a signature is always meaningful. A minimal typeclass is also
easier to implement — the gap between what an interface demands and what most
implementations need is exactly where dishonest signatures come from.
