# Delimited Scopes

Soundness uses context functions to delimit the blocks within which an extra capability
or piece of context is available. Entering such a block — to handle errors, to run
concurrent tasks, to hold a resource open — makes the relevant contextual values
available inside it and withdraws them at its boundary. The reach of a capability is
therefore visible in the structure of the code itself: a capability applies exactly
where the block says it does, and not beyond. This structural confinement is what
[capture checking](capture-checking.md) then enforces in the types.

## One shape, everywhere

The same pattern introduces a resource, an error handler, a concurrency context and a
lent secret. Recognising it once means recognising all of them:

```scala
path.open[File](Write): handle ?=>       // a file, open for this block
  handle.write(data)

recover:
  case error: Io.Error => fallback
. protect(readConfiguration())           // a handler, for this block

supervise:                               // a supervisor, for this block
  async(compute())

key.uncloak:                             // a key, lent to this block
  message.encrypt(InitializationVector.random)
```

In each case the capability exists inside the braces and nowhere else, and the block's
extent is the capability's extent — visible on the page, without reading any
documentation about lifetimes.

## Why a block rather than a constructor

The alternative is to construct the capability, use it, and release it:

```scala
val handle = open(path)
try handle.write(data) finally handle.close()
```

This has three defects that the block form does not. The `close` can be forgotten, or
placed where an early return skips it. The handle exists as a value *before* it is valid
and *after* it is not, so its type says nothing about when it may be used. And nesting
two of them doubles the ceremony, with the release order stated by hand.

The block form fixes each: there is no release to write, the capability is only in scope
where it is valid, and nesting two resources is nesting two blocks.

## Scope is a precise instrument

Because the mechanism is ordinary lexical scope, the granularity is whatever the code
needs. A given imported at the top of a file configures the file; one declared in a
method configures the method, overriding the wider choice; a `locally` block confines an
exception to a few lines:

```scala
import formatting.compactJsonFormatting        // this file

def debugDump(json: Json): Text =
  given Json.Formatting = indentedJsonFormatting  // this method only
  json.show

locally:
  import textSanitizers.strictSanitizer        // these two lines
  decoder.decoded(untrustedBytes)
```

Nothing has to support this granularity specially. It falls out of using the language's
own scoping rather than a bespoke configuration mechanism.

## What "withdrawn at the boundary" really means

Structurally, the capability leaves scope when the block ends. That alone would not stop
a value from *carrying* it out — a lazy stream still holding an open file, a closure
still holding an error handler, a task still holding its supervisor. Those escapes are
exactly what by-eye reasoning misses, because laziness, closures and concurrency detach
when code runs from where it was written.

[Capture checking](capture-checking.md) closes the gap: the capability's scope travels
in the value's type, so an escape is a compile error rather than a runtime surprise.
Delimited scopes state the intent; capture checking makes it a guarantee.

## What it costs

Two costs, both real.

**Rightward drift.** Each nested capability indents the code, and a function needing four
of them is four levels in before it begins. Scala's colon-lambda syntax keeps this to one
level per capability rather than one level plus a closing brace, but deeply capable code
still looks deeply nested — and where that becomes unreadable, the answer is to factor
the inner work into a method that declares what it needs, not to widen the scopes.

**The capability cannot simply be stored.** A value that needs a file handle at some
later, unrelated moment cannot hold one; it must either do its work inside the scope or
take a memoized copy of what it needed. That is a genuine constraint, and it is the
constraint doing the work: "hold this resource indefinitely and hope" is precisely the
pattern being ruled out.

See [declarative context](declarative-context.md) for what is put into these scopes, and
[structured concurrency](structured-concurrency.md) for the case where the scoped thing
is a set of running tasks.
