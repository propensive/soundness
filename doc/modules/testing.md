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
        t"42".as[Int]
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

A test may also carry a *moniker*: a compiletime-checked identifier that names it stably, so a
selection or a chart can address it even after its description is reworded:

```scala
test(n"square", m"square a number")(3*3).assert(_ == 9)
```

### Spreading a test over axes

A test is a name and zero or more *axes*. Given an axis, the body runs once per value and each
verdict is recorded at that coordinate, so one test covers a family of cases without becoming a
loop whose failures all report the same name:

```scala
test(m"double every value").over(Axis(t"n")(1, 2, 3, 4)): n =>
  n*2
. assert((n, result) => result == n*2)
```

An enumeration's companion is an axis in its own right, needing no list of values:

```scala
test(m"enum companions form axes").over(Codec): codec =>
  codec.ordinal
. assert((codec, ordinal) => ordinal >= 0 && ordinal < 3)
```

Two axes spread a test over a grid, rendered as a crosstab. A partial body leaves gaps, which
appear as empty positions rather than as failures — an honest way to say a combination does not
apply:

```scala
test(m"biaxial spread with a gap").over(Axis(t"x")(1, 2, 3), Axis(t"y")(10, 20)):
  case (x, y) if x + y != 23 => x*y
. assert((x, y, result) => result == x*y)
```

The assertion sees the axis values alongside the result, so a predicate can depend on where it
is in the grid.

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

### Measuring, not just asserting

A test need not end in a verdict. Three further kinds record measurements, and all four land in
the same report: a name, its axes, and a sparse map of cells whose figures come from one closed
set, so every kind renders the same way and charts the same way.

A `Bench` measures speed. Its body is a quoted expression, [staged](staging.md) and dispatched in
its own JVM, so the measurement is not distorted by the rest of the suite:

```scala
val bench = Bench()

bench(m"decode directly")(target = 1*Second, operationSize = size):
  '{ Benchmarks.decodeUsersDirect() }
```

A `Stress` measures what a throughput figure hides: allocation per operation, peak heap, and the
live set retained under sustained concurrency. It runs for a wall-clock target at a stated
concurrency, and may be given a constrained heap or a CPU limit, so a design can be shown to hold
up where it matters:

```scala
val stress = Stress()
val constrained = Stress(heap = t"128m")

stress(m"cross-thread hand-off")(target = 2*Second, concurrency = 16):
  '{ … }
```

A `Profile` answers where the time went, rendering a histogram of the hottest methods by self
time from JFR execution samples, coloured by package:

```scala
val profile = Profile()

profile(m"pipeline hotspots")(target = 5*Second):
  '{ … }
```

Benchmarks spread over axes as tests do, staging one program per distinct implementation and
dispatching each cell in its own JVM; a baseline anchors one axis value and the rest render
relative to it. A stress *sweep* records its steps as coordinates on an emergent axis of a single
test, rather than as a family of differently-named tests.

### Running and selecting

A `Suite` is a main class: run directly, it executes its tests with live progress on a terminal
and plain output on CI, reports each failure with its contrast, and exits nonzero if any test
failed — all a build needs to gate on.

A suite executable also accepts selection terms, so a subset can be run without editing the code.
Hashes, monikers and name globs identify tests and union with each other; `kind:` filters and
axis constraints intersect with that union:

```sh
mysuite square 'parser*'      # by moniker, then by name glob
mysuite kind:bench            # only the benchmarks
mysuite parser=jacinta 'N<32' # only cells matching these axis constraints
mysuite --list                # enumerate what matches, and run nothing
```

Unselected assertions never execute, and unselected benchmark cells are skipped, so a selection
costs only what it runs.
