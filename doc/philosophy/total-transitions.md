# Total Transitions

The second golden rule, the companion to making
[impossible states unrepresentable](impossible-states.md): every operation that moves a
value from one state to another should be total — defined for every input it can be
given. When all the starting states are valid and every transition between them is
total, a program that begins in a correct state can never leave one. Partiality — an
operation undefined for some of its inputs — is the gap through which invalid states
creep back in, so Soundness designs it out, typically by giving an operation a return
type rich enough to express the cases it cannot otherwise handle.

The compiler polices totality wherever it is allowed to, and it should be allowed to. A
pattern match that misses a case is partiality in its plainest form, and Scala reports
it — so the warnings that require matches to be exhaustive should be switched on and
treated as errors, never suppressed as noise. Under that regime, adding a case to an
enumeration produces a compile error at every match that has not considered it: the
compiler walks the consequences of a change through the codebase, which is precisely the
analysis a programmer would otherwise perform by memory. A deliberately partial match is
still expressible — `.absolve` marks the assertion, visibly and searchably — so the
discipline costs nothing except accidental partiality, which is the kind that becomes a
bug.
