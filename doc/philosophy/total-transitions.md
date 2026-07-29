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

## Making an operation total

An operation is partial when it has nothing to return for some of its inputs. There are
three honest ways to make it total, and one dishonest one.

**Widen the return type to admit absence.** A lookup that may find nothing returns an
`Optional`, so "not found" is a value rather than an exception or a `null`:

```scala
enumerable.value(t"North")    // Optional[Direction]
tracked.locate(pointer)       // Optional[Position] — an unresolved path is not an error
```

**Widen it to admit failure.** Where the caller should know *why*, the operation raises a
typed error and the response is chosen at the call site:

```scala
def parsePort(text: Text): Port raises PortError raises NumberError
```

**Narrow the input type.** Best of all, where it is available: if the operation is
undefined for some inputs, take an input type that excludes them. `Port` exists precisely
so that `connect` need not be partial.

The dishonest way is to return a plausible value for the undefined cases — zero, empty,
the first element — which converts a detectable failure into a wrong answer that
propagates.

## Exhaustivity as a change-propagation tool

The most valuable consequence of treating exhaustivity warnings as errors is not that
today's matches are complete. It is what happens tomorrow, when a case is added:

```scala
enum Connection:
  case Open(socket: Socket)
  case Failed(reason: Text)
  case Closed
  case Draining          // added today
```

Every match on `Connection` that has not considered `Draining` now fails to compile, and
the compiler lists them. That is precisely the analysis a careful programmer would
otherwise perform from memory, across a codebase they may not have written — done
exhaustively, in seconds, on every build.

This is why the warning must be an *error*. As a warning it is advisory, and advisory
diagnostics accumulate until nobody reads them; as an error it is a tool that makes
adding a case safe.

## Deliberate partiality, marked

Some matches genuinely cannot be exhaustive, usually because the compiler cannot see an
invariant the code has already established. Suppressing the warning would hide the
accidental cases along with the deliberate one, so the assertion is marked instead:

```scala
expr.absolve match
  case '{ $value: Int } => …
```

`absolve` is Scala's `runtimeChecked` under a name that reads as what it means. It marks
one expression, so nothing else is silenced; it is greppable, so every such assertion can
be found and reviewed; and it is a claim the writer is making, which is exactly the
status such an assertion should have.

## Where totality is not achievable

Two limits, worth stating.

**The world is partial.** Opening a file that exists may still fail — the disk, the
permissions, a race with another process. No return type makes that total; the honest
response is a typed error, which is totality of the *signature* rather than of the
operation.

**Some invariants outrun the type system.** Whether a hostname resolves, whether two
independently-validated values are consistent with each other, whether a schema and a
document that both parsed actually agree — these are checkable, but not by construction.
The discipline is to make the check produce a value that carries its result, so that
downstream code inherits the guarantee rather than repeating the check.

See [impossible states](impossible-states.md) for the companion rule — the two are
useless apart, since total transitions between meaningless states preserve nothing worth
preserving.
