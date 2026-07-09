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
  given JsonFormatting = indentedJsonFormatting   // this function only
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
