# Declarative Context

Soundness configures behaviour declaratively, through contextual values in scope rather
than through arguments threaded by hand or flags consulted at runtime. Choosing an
output format, an error strategy, a character encoding, or a particular implementation
is a matter of bringing the right `given` into scope, after which the choice applies
automatically to everything within that scope. Because the configuration is
declarative, it is independent of control flow and easy to reason about: what is in
scope determines what happens, wherever the affected code runs.

The power of the approach is that scope is a precise instrument. A given imported at the
top of a file configures the file; one declared inside a block configures only that
block, overriding any wider choice; and a `locally` block confines an exception to a few
lines:

```scala
import formatting.compactJsonFormatting     // the file's default

def debugDump(json: Json): Text =
  given Json.Formatting = indentedJsonFormatting   // this function only
  json.show

locally:
  import textSanitizers.strictSanitizer     // these lines reject bad input
  decoder.decoded(untrustedBytes)
```

Configuration composes like the code it configures. A project defines its house choices
once and shares them with `export`, so `import myproject.conventions.*` equips any file
with the team's error strategy, encodings and formats — configuration as a library,
versioned and reviewed like one.

Two pieces of tooling keep the approach honest. Contextual values that represent
capabilities or configuration extend the `Findable` marker, which the Soundness compiler
plugin uses to distinguish *findable* context — things a user is expected to supply or
import — from internal machinery. And when a required given is missing, importing
`explainMissingContext` turns the compiler's bare "no implicit found" into a diagnosis:
it searches the classpath for the instances that *would* satisfy the search and names
the imports that provide them, so the response to a missing context is a one-line
import, not an archaeology session.

## What goes into scope

The range is worth seeing at once, because the uniformity is the point. A single
mechanism carries choices that other designs would spread across constructor arguments,
global settings, builder methods and runtime flags:

```scala
import strategies.throwUnsafely            // what a failure does
import charEncoders.utf8Encoder            // how text becomes bytes
import formatting.compactJsonFormatting    // how JSON is rendered
import dateFormats.iso8601DateFormat       // how dates are shown
import affirmations.yesNoAffirmation       // how a boolean is shown
import threading.virtualThreading          // what a task runs on
import probates.cancelProbate              // what happens to unfinished tasks
import httpBackends.native                 // which HTTP transport is used
import filesystemBackends.virtualMachine   // which filesystem implementation
import calendars.gregorianCalendar         // which calendar a date literal means
```

One idiom to learn, and the same reasoning — "what is in scope determines what happens" —
applies to all of them.

## Naming makes the choice legible

A given is named for what it *selects*, not for its type, because the import line is what
a reader sees. `import strategies.accrue` says something about the code below it;
`import strategies.given` would not.

This is why the convention matters more here than elsewhere: a contextual value is
invoked without being written at the call site, so the import is the only place its
choice is visible. A poorly-named given makes contextual configuration exactly as opaque
as its critics claim it is.

## What it costs

The honest objection to contextual configuration is action at a distance, and it is not
wrong.

**The effect is not local to the call site.** A `json.show` renders differently depending
on an import possibly a hundred lines above, and nothing at the call site says so. A
reader must know to look. The mitigation is scope precision — the narrower the scope, the
closer the given is to what it affects — but the tension is real, and a given imported
file-wide genuinely is action at a distance.

**Ambiguity is a compile error, and a confusing one.** Two givens of the same type in
scope make the code ambiguous, which is safe but reported in terms of implicit
resolution rather than "you have chosen two JSON formats". Importing two convention
bundles that disagree produces exactly this.

**Refactoring can silently change behaviour.** Moving a function to another file moves it
out of one set of imports and into another. The types still check; the behaviour may
differ. This is the sharpest edge of the approach, and the reason a project's conventions
are better collected into one shared object than assembled ad hoc per file.

**There is no runtime override.** Because the configuration is resolved as the code
compiles, it cannot be changed by a command-line flag or a configuration file without
threading the decision explicitly. Where runtime configurability is genuinely wanted, the
value must be an ordinary parameter — and recognising which of the two a given choice
needs is a design decision, not an automatic one.

See [delimited scopes](delimited-scopes.md) for how the scope is established, and
[honest signatures](honest-signatures.md) for why the requirements being in the signature
is what makes this legible at all.
