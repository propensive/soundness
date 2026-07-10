## Testing

### About

Tests in Soundness are values: a test names what it verifies, produces a value, and an assertion
states what must hold of it. Around that core sit two capabilities most frameworks lack. When an
equality assertion fails, the report is not two `toString`s but a structural *contrast* — a diff of
the expected and observed values, aligned field by field and element by element, with the actual
differences highlighted. And because Soundness moves so many checks to compiletime, tests can
assert that code *does not compile*: a block that should be rejected is captured, and its compile
errors become values to inspect.

### On testing

A test framework's real product is its failure output. "Expected X but found Y", printed as two
long strings, leaves the reader diffing by eye; and in a codebase whose invariants live in the type
system, half of what needs verifying is precisely that certain code is impossible to write — which
an ordinary test cannot express at all, since it would have to compile first.

Soundness addresses both: failures render as structured comparisons, and compile errors are
first-class test subjects. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Suites and tests

A suite implements `run()`; a test wraps the expression under scrutiny and asserts on its value,
with `suite` grouping related tests:

```scala
object Tests extends Suite(m"Parser tests"):
  def run(): Unit =
    suite(m"Numbers"):
      test(m"a decimal integer parses"):
        t"42".decode[Int]
      . assert(_ == 42)
```

`check` is `assert` returning the value, for chaining further work on it; `matches` asserts that a
value fits a pattern; and `aspire` marks an assertion that *should* hold but is known not to yet —
recorded distinctly, neither failing the build nor forgotten.

Approximate comparison is built in for the numeric cases where exact equality is wrong:

```scala
test(m"the mean converges"):
  results.mean.vouch
. assert(_ === 0.0 +/- 0.02)
```

### Failure as a contrast

When an equality assertion fails, the two values are *decomposed* — case classes into fields,
collections into aligned elements — and juxtaposed, so the report shows the tree with the
differing leaves marked: an age that differs by two, a list with one element substituted, a text
with the changed characters banded. Sequences align through the same [diffing](diffing.md) used
elsewhere, so an insertion shows as an insertion, not as every subsequent element "differing".

### Asserting that code does not compile

`demilitarize` compiles the code it encloses and, instead of failing the build, delivers the
compile errors as values:

```scala
test(m"a malformed version literal is rejected"):
  demilitarize:
    v"1.2"
  . map(_.message)
. assert(_.nonEmpty)
```

This is how Soundness's own guarantees are tested — that a wrong-currency addition, an invalid
literal, an undeclared relation genuinely fails to compile — and any project building invariants
into types needs the same: the *impossibility* is the feature, and this makes it testable.

### Running

A `Suite` is a main class: run directly, it executes its tests with live progress on a terminal
and plain output on CI, reports each failure with its contrast, and exits nonzero if any test
failed — all a build needs to gate on.
