# Expressive Errors

When an error does occur, Soundness makes it as informative as it can be. An error is
an immutable value carrying the full detail of what went wrong — what was expected, what
was found, and where — rather than a bare message or an opaque code. Because that detail
is structured data, it can be inspected, rendered legibly for a person, or matched on by
handling code, and the same richness that helps a developer diagnose a fault lets a
program respond to it precisely.

Above all, errors are designed for pattern matching. An error is a case class, its
distinct causes are the cases of a `Reason` enumeration, and the values that explain the
failure are fields — so handling code deconstructs the error rather than parsing its
message:

```scala
recover:
  case Io.Error(path, _, Io.Error.Reason.Nonexistent, _)      => create(path)
  case Io.Error(_, _, Io.Error.Reason.PermissionDenied, _)    => escalate()
. protect(openConfiguration())
```

A handler that matches on structure is checked by the compiler — a renamed reason breaks
the match, not the production behaviour — and can be exactly as discriminating as the
situation needs, where string-matching on messages is fragile in every way. The message a
person reads and the structure a program matches are two renderings of one value, so they
can never disagree.

## The anatomy of an error

The shape is consistent enough to be worth stating. An error is a case class carrying the
values that explain it, its distinct causes are a `Reason` enumeration, and its message is
built with the `m"…"` interpolator from those same values:

```scala
object IpAddressError:
  enum Reason(val number: Int) extends Clarification:
    case Ipv4ByteOutOfRange(byte: Int)        extends Reason(1)
    case Ipv4ByteNotNumeric(byte: Text)       extends Reason(2)
    case Ipv6WrongNumberOfGroups(count: Int)  extends Reason(7)
    case Ipv6MultipleDoubleColons             extends Reason(8)

  object Reason:
    given communicable: Reason is Communicable =
      case Ipv4ByteOutOfRange(byte)       => m"the number $byte is not in the range 0-255"
      case Ipv4ByteNotNumeric(byte)       => m"the part $byte is not a number"
      case Ipv6WrongNumberOfGroups(count) => m"the address has $count groups, but should have 8"
      case Ipv6MultipleDoubleColons       => m":: appears more than once"
```

Note that a reason may carry data of its own — the byte that was out of range, the number
of groups actually found — so the diagnosis is specific rather than categorical.

The numbers are the error's identity. An error type has a globally-unique `SN-NNN` code
and each reason a number within it, and every code has a page in `doc/errors` explaining
the cause and the usual remedies — so `SN-077` in a user's report can be looked up rather
than guessed at.

Three further properties follow from the shape, and each is worth having.

The message is *derived* from the fields, so it cannot describe a state the value is not
in. A `Reason` is a closed set, so a handler matching on it is checked for exhaustiveness
and a new reason is reported at every site that has not considered it. And the values
that explain the failure are available to the handler, not merely rendered into a
sentence.

## Messages are values too

A `Message` is not a `Text`. It is a structured value built from interpolated parts, so
that anything `Communicable` can be embedded and rendered consistently wherever the
message is shown:

```scala
m"the port $text is not valid because $reason"
```

The reason renders through its own `Communicable` instance rather than through
`toString`, so the phrasing lives with the type it describes and one wording serves every
message that embeds it. This is also the seam through which messages could be localized:
the structure survives to the point of rendering rather than being flattened into a string
at the point of construction.

## Errors are pure

An error may never hold a live capability, and the compiler enforces it: `Error` extends
`caps.Pure`.

The reason is precise. `throw` is the one channel [capture checking](capture-checking.md)
cannot see — an exception caught outside a scope arrives with its captures erased — so an
error carrying a capability would be a hole in the guarantee that scoped things do not
escape. Purity closes it: no error, however it travels, can smuggle a file handle or an
error handler out of the scope that confined it.

This has a design consequence worth knowing. An error may carry the *path* that could not
be opened, but not the open handle; the *URL* that failed, but not the connection. In
practice that is what a diagnosis wants anyway.

## Stack traces are optional

Capturing a stack trace is the expensive part of constructing an exception, and much of
the time it is wasted: an error that is expected and handled two frames away has no use
for one.

Whether an error captures a trace is therefore a contextual choice:

```scala
import errorDiagnostics.stackTracesDiagnostics   // capture, for debugging
import errorDiagnostics.emptyDiagnostics         // omit, where the error is expected
```

The `Diagnostics` parameter appears in every error's constructor for this reason. It is
the one piece of ceremony the design imposes on defining an error, and it buys the
ability to use typed errors in hot paths without paying for traces nobody will read.

## What it costs

**Defining an error is more work than throwing a string.** A case class, a `Reason`
enumeration, a `Communicable` instance and a `Diagnostics` parameter is real ceremony
against `throw new RuntimeException("bad port")`.

The defence is that the ceremony is proportional to the error's importance and is paid
once per error type, while the benefit is paid out at every handler — and that the
alternative degrades predictably: string messages get matched on, then the messages get
improved, and the matching breaks silently.

**Reason enumerations are a commitment.** Adding a case is a source-breaking change for
exhaustive handlers, which is the exhaustivity check working as intended, but it does
mean the set of reasons deserves thought when the error is designed.

See [error handling](error-handling.md) for how these values are raised and responded to,
and [total transitions](total-transitions.md) for why the exhaustiveness matters.
