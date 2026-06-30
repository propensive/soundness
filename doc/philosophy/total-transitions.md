# Total Transitions

The second golden rule, the companion to making
[impossible states unrepresentable](impossible-states.md): every operation that moves a
value from one state to another should be total — defined for every input it can be
given. When all the starting states are valid and every transition between them is
total, a program that begins in a correct state can never leave one. Partiality — an
operation undefined for some of its inputs — is the gap through which invalid states
creep back in, so Soundness designs it out, typically by giving an operation a return
type rich enough to express the cases it cannot otherwise handle.
