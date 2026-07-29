# Impossible States

The first of Soundness's two golden rules: the types of a program should admit only
states that are valid, so that an impossible state cannot even be written down.
Rather than constructing a value and then checking whether it is legal, the type makes
an illegal value impossible to construct, moving the error from runtime to compiletime.
A type that can hold only meaningful values needs no defensive checks downstream,
because the compiler has already ruled out everything else. This pairs with the
companion rule that transitions between states should be [total](total-transitions.md).

## The shape of the rule

Most validation is written the wrong way round. A value is constructed, then checked,
and the check's result is discarded — so every subsequent function receives a value that
*might* be valid and must either trust or re-check it:

```scala
val port = readPort()          // an Int; could be anything
if port < 1 || port > 65535 then fail()
connect(port)                  // connect must trust, or check again
```

The type-first version makes the check the *only* way to obtain the value, so possessing
one is proof:

```scala
val port = text.as[Port]       // a Port, or a typed failure
connect(port)                  // nothing left to check
```

`connect` cannot be given an out-of-range port, because no such `Port` exists.

## Three ways a type rules a state out

**By refinement.** A number carries its permitted range in its type, so the range is
checked where the value is written and arithmetic works out the range of the result:

```scala
val rate: 0.0 ~ 1.0 = 0.001
val bad:  0.0 ~ 1.0 = 1.5      // does not compile
```

**By parameterization.** A property that would otherwise be a field becomes a type
argument, and values differing in it become different types. Money in one currency
cannot be added to money in another; a path on one platform cannot be used where another
is expected; a task on one timeline cannot be subtracted from a task on another:

```scala
Eur(3.01) + Gbp(2.50)          // does not compile
Instant(0L).over[Tai] - Instant(0L)   // does not compile
```

**By construction.** A literal is parsed against its grammar as the code compiles, so a
malformed one is a compile error rather than a value:

```scala
2012-Feb-30                    // does not compile: no such date
url"https://example.com/"      // checked against the URL grammar
media"application/jsom"        // does not compile: did you mean application/json?
```

The third is [safety by construction](safety-by-construction.md), which this rule's
practical work mostly consists of.

## Encoding a state machine

The rule is most valuable where a type has *modes*. The usual encoding gives one class
every field any mode needs, with the invalid combinations excluded by comment:

```scala
case class Connection(socket: Optional[Socket], error: Optional[Text], closed: Boolean)
```

Nothing prevents a `Connection` that is closed *and* holds a socket, or one with neither
a socket nor an error, and every reader must reconstruct which combinations were meant.
The enumerated form admits only the states that exist:

```scala
enum Connection:
  case Open(socket: Socket)
  case Failed(reason: Text)
  case Closed
```

The four impossible combinations are now unwriteable, and — because the compiler checks
exhaustiveness — every place that handles a `Connection` is told when a case is added.

## What it costs

Three costs, all real.

**Constructing is fallible.** Turning text into a `Port` can fail, so it needs an error
strategy in scope. The check has not disappeared; it has moved to a single place and
left a proof behind.

**Types get more specific, and inference works harder.** A method that used to take an
`Int` now takes a `Port[Tcp]`, and callers must produce one. That is the point — but it
does mean a refactor propagates further than it would have.

**Not everything can be ruled out.** A hostname's syntax is checkable; whether it
resolves is not. The discipline is to encode what is genuinely determined by the value
and to leave the rest to [honest signatures](honest-signatures.md) and typed errors,
rather than to invent a type that promises more than it can prove.

See [total transitions](total-transitions.md) for the companion rule, and
[safety by construction](safety-by-construction.md) for how the checking is done.
