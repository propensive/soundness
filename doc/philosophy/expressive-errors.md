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
  case IoError(path, _, IoError.Reason.Nonexistent) => create(path)
  case IoError(_, _, IoError.Reason.PermissionDenied) => escalate()
. protect(openConfiguration())
```

A handler that matches on structure is checked by the compiler — a renamed reason breaks
the match, not the production behaviour — and can be exactly as discriminating as the
situation needs, where string-matching on messages is fragile in every way. The message a
person reads and the structure a program matches are two renderings of one value, so they
can never disagree.
