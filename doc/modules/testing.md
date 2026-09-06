## Testing

### About

Tests in Soundness are values: a test names what it verifies, produces a value, and an assertion
states what must hold of it. Around that core sit two capabilities most frameworks lack. When an
equality assertion fails, the report is not two `toString`s but a structural *contrast* — a diff of
the expected and observed values, aligned field by field and element by element, with the actual
differences highlighted. And because Soundness moves so many checks to compiletime, tests can
assert that code *does not compile*: a block that should be rejected is captured, and its compile
errors become values to inspect. Testing that something is impossible is what a
library built on [correctness](../philosophy/correctness.md) most needs to verify.

### On testing

A test framework's real product is its failure output. "Expected X but found Y", printed as two
long strings, leaves the reader diffing by eye; and in a codebase whose invariants live in the type
system, half of what needs verifying is precisely that certain code is impossible to write — which
an ordinary test cannot express at all, since it would have to compile first.

Soundness addresses both: failures render as structured comparisons, and compile errors are
first-class test subjects. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
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
val results = List(0.01, -0.02, 0.015)

test(m"the mean converges"):
  results.total/results.size
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
enum Codec:
  case Binary, Textual, Compressed

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

Comparison alone can only say *same* or *different*, so a raw diff of two sequences reports a
changed element as a deletion beside an unrelated insertion, and stops there. The report goes
further by asking, within each region of differences, whether an element removed and an element
added are *similar* — a board member with the same role but a different name, a record with the
same identifier and a changed field. Where they are, the two are shown as one substitution broken
down to the field that changed, rather than as two unrelated entries the reader must pair up.

### Asserting that code does not compile

`demilitarize` compiles the code it encloses and, instead of failing the build, delivers the
compile errors as values:

```scala
import classloaders.threadContextClassloader

test(m"a malformed version literal is rejected"):
  demilitarize:
    v"1.2"
  . map(_.message)
. assert(_.nonEmpty)
```

This is how Soundness's own guarantees are tested — that a wrong-currency addition, an invalid
literal, an undeclared relation genuinely fails to compile — and any project building invariants
into types needs the same: the *impossibility* is the feature, and this makes it testable.

It works through a compiler plugin, and knowing roughly how is useful when it misbehaves. The
plugin runs after parsing but before typechecking. Finding a `demilitarize` block in the untyped
tree, it starts a *separate* compilation of the same file, on the same classpath but without the
plugin, and expects it to fail; the errors falling inside the block are captured and replaced, in
the main compilation, by code constructing the corresponding `CompileError` values. Typechecking
then sees only those constructions, never the erroneous code, so the build succeeds as long as
nothing outside a `demilitarize` block is wrong.

One consequence is worth knowing. An error in an early phase can stop later phases running, so
errors that would have come from those phases go uncaptured on the first pass. The plugin
re-runs the compilation as many times as needed, removing more of the offending code each time,
until the later phases are reached — which is why a file with several `demilitarize` blocks
compiles more slowly than its size suggests.

### Measuring, not just asserting

A test need not end in a verdict. Three further kinds record measurements, and all four land in
the same report: a name, its axes, and a sparse map of cells whose figures come from one closed
set, so every kind renders the same way and charts the same way.

A `Bench` measures speed. Its body is a quoted expression, [staged](staging.md) and dispatched in
its own JVM, so the measurement is not distorted by the rest of the suite:

<!-- doccheck: skip -->
```scala
val bench = Bench()

bench(m"decode directly")(target = 1*Second, operationSize = 1000):
  '{ Benchmarks.decodeUsersDirect() }
```

A `Stress` measures what a throughput figure hides: allocation per operation, peak heap, and the
live set retained under sustained concurrency. It runs for a wall-clock target at a stated
concurrency, and may be given a constrained heap or a CPU limit, so a design can be shown to hold
up where it matters:

<!-- doccheck: skip -->
```scala
val stress = Stress()
val constrained = Stress(heap = t"128m")

stress(m"cross-thread hand-off")(target = 2*Second, concurrency = 16):
  '{ Benchmarks.handOff() }
```

A `Profile` answers where the time went, rendering a histogram of the hottest methods by self
time from JFR execution samples, colored by package:

<!-- doccheck: skip -->
```scala
val profile = Profile()

profile(m"pipeline hotspots")(target = 5*Second):
  '{ Benchmarks.pipeline() }
```

Benchmarks spread over axes as tests do, staging one program per distinct implementation and
dispatching each cell in its own JVM; a baseline anchors one axis value and the rest render
relative to it. A stress *sweep* records its steps as coordinates on an emergent axis of a single
test, rather than as a family of differently-named tests.

### Running and selecting

A `Suite` is not a main class. The beneficence compiler plugin records every `Suite` object in
the `META-INF/services/probably.Suite` index of the jar it compiles, and the
[fume](https://github.com/propensive/fume) runner discovers suites from that index on a built
classpath, runs each one in-process, and renders the stream of `TestEvent`s a suite emits: live
progress on a terminal, plain output on CI, each failure with its contrast, and a nonzero exit if
any test failed — all a build needs to gate on. A project names its classpath once in
`.fume/config.tel`, so a bare `fume run` runs everything.

fume also accepts selection terms, so a subset can be run without editing the code. Hashes,
monikers and name globs identify tests and union with each other; the `--test`, `--bench`,
`--stress` and `--profile` switches and axis constraints intersect with that union:

```sh
fume run square 'parser*'         # by moniker, then by name glob
fume run --bench                  # only the benchmarks
fume run parser=jacinta 'N<32'    # only cells matching these axis constraints
fume list                         # enumerate what matches, and run nothing
```

Unselected assertions never execute, and unselected benchmark cells are skipped, so a selection
costs only what it runs.
