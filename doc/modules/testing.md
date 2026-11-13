## Testing

### About

A test is a value. It names what it verifies, and produces a value, and an assertion states
what must hold of that value. Around that core sit two capabilities which most frameworks
lack.

When an assertion of equality fails, the report is not two `toString`s. It is a structural
*contrast*: a diff of the expected value and the observed one, aligned field by field and
element by element, with the actual differences highlighted.

And tests can assert that code *does not compile*. So many checks are made at [compiletime]
that this matters: a block which should be rejected is captured, and its compile errors
become values to inspect. Testing that something is impossible is what a library built on
[correctness](../philosophy/correctness.md) most needs to do.

### On testing

The real product of a test framework is its output on failure. "Expected X but found Y",
printed as two long strings, leaves the reader to find the difference by eye.

And in a codebase whose invariants live in the type system, half of what needs to be
verified is precisely that certain code cannot be written. An ordinary test cannot express
that at all, since it would have to compile first.

Both are addressed here. Failures are shown as structured comparisons, and compile errors
are subjects for tests in their own right. Everything comes from the `soundness` package:

```scala
import soundness.*
import environments.javaBaseEnvironment
import strategies.throwUnsafely
```

The samples on this page declare tests, which a test runner would run, so they are compiled
but not run here.

### Suites and tests

A suite implements `run()`. A test wraps the expression under scrutiny, and asserts on its
value, and `suite` groups related tests:

```scala
object Tests extends Suite(m"Parser tests"):
  def run(): Unit =
    suite(m"Numbers"):
      test(m"a decimal integer parses"):
        t"42".as[Int]
      . assert(_ == 42)
```

A test ends in one of three assertions:

```tabular
column  Assertion
column  Asserts
column  Returns
row  `assert`  a predicate holds of the value  nothing
row  `check`  a predicate holds of the value  the value, for chaining further work on it
row  `matches`  the value fits a pattern  nothing
```

A test which *should* pass, but is known not to yet, is written like any other, inside an
`aspirationally` block. Every assertion within it is recorded as an aspiration. It neither
fails the build, nor is forgotten, and it is reported distinctly once it starts to pass.

```scala
aspirationally:
  test(m"a hexadecimal integer parses"):
    t"0x2a".as[Int]
  . assert(_ == 42)
```

Approximate comparison is built in, for the numeric cases where exact equality is wrong:

```scala
import dysasymptotics.linearSize

val results = List(0.01, -0.02, 0.015)

test(m"the mean converges"):
  results.total/results.size
. assert(_ === 0.0 +/- 0.02)
```

A test may also have a *moniker*: an identifier, checked at compiletime, which names it
stably. A selection, or a chart, can then address the test even after its description has
been reworded:

```scala
import soundness.nominative

test(n"square", m"square a number")(3*3).assert(_ == 9)
```

### Where a test belongs

A suite has a title and an *id*. The id is what a selection, and a path, know the suite by.
It is either given, as a string literal before the title, or derived from the title, as its
letters and digits in lower case, with a `-` for each run of anything else. The first of
these suites is run with `fume run parser`, and the second with `fume run parser-tests`:

```scala fuel
typecheck
##
object Tests extends Suite("parser", m"Parser tests")
object ParserTests extends Suite(m"Parser tests")
```

Soundness's own suites always give an id, which names what the library is for, and not the
library itself, as [the testing standard](../standards/testing.md) sets out.

An id which is given is also a *type*. Within the body of
`Suite("parser", m"Parser tests")`, the [contextual value] which `test` and `suite` take is
a `Testable of "parser"`, and each `suite` block passes the same type on. The compiler
therefore knows, of every test which it compiles, which suite it is declared for, and it
refuses a `test` which is declared for none.

Tests which are written in a method of their own name the suite, by its id, in the method's
signature, as the second version shows:

```scala fuel
alternative Inline
  code
      object Tests extends Suite(m"Parser tests"):
        def run(): Unit =
          suite(m"Numbers"):
            test(m"a decimal integer parses"):
              t"42".as[Int]
            . assert(_ == 42)
alternative In a method
  code
      object Tests extends Suite("parser", m"Parser tests"):
        def run(): Unit =
          suite(m"Numbers")(numbers())

        def numbers()(using Testable of "parser"): Unit =
          test(m"a decimal integer parses"):
            t"42".as[Int]
          . assert(_ == 42)
##
```

A suite whose id is derived has no literal to name it by. Its tests are written in its
body, or in a method which is generic in the suite, with
`[topic <: Label](using Testable of topic)`.

A compiler plugin reads this from the typed code, and writes an index of the tests beside
the index of suites, in `META-INF/probably/tests`: every test's place in the hierarchy, its
kind, its moniker, its tags and where it is declared. A tool can therefore list the tests on
a classpath, complete their names, and arrange them into suites, without running anything. A
listing which is made by running a suite, by contrast, executes every statement around its
tests.

Some tests cannot be known in that way: one for each file of a corpus, say, or tests which
are run on a `Runner` of the program's own, and belong to no suite at all. They are declared
inside an `impromptu` block. It admits `test` and `suite` as a `Suite` does, at the position
of the surrounding suite, if there is one, but leaves them out of the index. A tool learns
of them as they run.

```scala
impromptu:
  List(t"1", t"22", t"333").each: digits =>
    test(m"$digits parses")(digits.as[Int]).assert(_ > 0)
```

A test with a computed name which is *not* in an `impromptu` block is still indexed, by the
fixed parts of its name.

### Spreading a test over axes

A test is a name, and zero or more *axes*. Given an axis, the body runs once for each value,
and each verdict is recorded at that coordinate. One test therefore covers a family of
cases, without becoming a loop whose failures all report the same name:

```scala
test(m"double every value").over(Axis(t"n")(1, 2, 3, 4)): n =>
  n*2
. assert((n, result) => result == n*2)
```

The companion of an [enumeration] is an axis in its own right, and needs no list of values:

```scala
enum Codec:
  case Binary, Textual, Compressed

test(m"enum companions form axes").over(Codec): codec =>
  codec.ordinal
. assert((codec, ordinal) => ordinal >= 0 && ordinal < 3)
```

Two axes spread a test over a grid, which is shown as a table with one axis along each
side. A body which is partial leaves gaps, which appear as empty positions, not as failures.
That is an honest way to say that a combination does not apply:

```scala
test(m"biaxial spread with a gap").over(Axis(t"x")(1, 2, 3), Axis(t"y")(10, 20)):
  case (x, y) if x + y != 23 => x*y
. assert((x, y, result) => result == x*y)
```

The assertion sees the values of the axes beside the result, so a predicate can depend on
where it is in the grid.

### Failure as a contrast

When an assertion of equality fails, the two values are *decomposed*, with case classes
broken into fields, and collections into aligned elements, and are set side by side. The
report shows the tree, with the differing leaves marked: an age which differs by two, a list
with one element substituted, a text with the changed characters highlighted. Sequences are
aligned through the same [diffing](diffing.md) which is used elsewhere, so an insertion
shows as an insertion, and not as every later element "differing".

Comparison alone can say only *same* or *different*. A raw diff of two sequences therefore
reports a changed element as a deletion beside an unrelated insertion, and stops there. The
report goes further. Within each region of differences, it asks whether an element which was
removed and an element which was added are *similar*: a board member with the same role but
a different name, or a record with the same identifier and a changed field. Where they are,
the two are shown as one substitution, broken down to the field which changed, and not as
two unrelated entries which the reader must pair up.

### Diagnosing a failure

Nothing about how a failure will be shown is decided when a test is compiled. Most
assertions pass, so working out how to render each one's value, on every compilation, would
be paid for thousands of times to serve the few which fail. An assertion records only where
its predicate is written.

When a predicate fails, the runner reads it back from the compiled suite, and evaluates it
once more on the value which failed, recording each intermediate value on the way. The
test's body is not run again. For a predicate such as `x => x*2 < 9 && x*y > 4`, with `x`
and `y` both 2, the report is the tree of its subexpressions:

```
x*2 < 9 && x*y > 4   false
x*2 < 9              true
x*2                  4
x                    2
           x*y > 4   false
           x*y       4
           x         2
             y       2
```

This is what makes a failing property legible: the counterexample is shown together with
everything that was computed from it. A predicate which is an equality is also shown as a
contrast of its two sides.

Each value is rendered by whichever instance is found for its type at that moment, an
`Inspectable` or a `Showable`. A suite supplies one in the type's companion, by an import
above the assertion, or as a given of the object which the suite is written in, and need
not know whether a runner will ask for it. A suite is asked for a diagnosis with the
selection term `--diagnose=dissect,contrast,inspect`.

### Asserting that code does not compile

`demilitarize` compiles the code which it encloses and, instead of failing the build,
delivers the compile errors as values:

```scala fuel
typecheck
##
import classloaders.threadContextClassloader

test(m"a malformed version literal is rejected"):
  demilitarize:
    v"1.2"
  . map(_.message)
. assert(!_.nil)
```

This is how Soundness's own guarantees are tested: that adding amounts in the wrong
[currency](money.md#arithmetic), an invalid literal, or an undeclared relation, really does
fail to compile. Any project which builds invariants into types needs the same. The
*impossibility* is the feature, and this makes it testable.

It works through a compiler plugin, and knowing roughly how is useful when it misbehaves.
The plugin runs after parsing, but before typechecking. When it finds a `demilitarize` block
in the untyped code, it starts a *separate* compilation of the same file, on the same
classpath, but without the plugin, and expects it to fail. The errors which fall inside the
block are captured. In the main compilation, they are replaced by code which constructs the
corresponding `CompileError` values. Typechecking then sees only those constructions, and
never the erroneous code, so the build succeeds, so long as nothing outside a `demilitarize`
block is wrong.

One consequence is worth knowing. An error in an early phase of the compiler can stop later
phases from running, so errors which would have come from those phases go uncaptured on the
first pass. The plugin runs the compilation again as many times as are needed, removing more
of the offending code each time, until the later phases are reached. That is why a file with
several `demilitarize` blocks compiles more slowly than its size suggests.

### Measuring, not just asserting

A test need not end in a verdict. Three further kinds record measurements:

```tabular
column  Kind
column  Records
row  `test`  a verdict
row  `Bench`  speed
row  `Stress`  allocation per operation, peak heap, and the live set under sustained concurrency
row  `Profile`  the hottest methods by self time, from JFR execution samples
```

All four land in the same report: a name, its axes, and a sparse map of cells, whose figures
come from one closed set. Every kind is therefore displayed in the same way, and charted in
the same way.

Every measurement is compiled and run on a `BenchmarkDevice`, which is the local machine
here. It needs a system, a temporary directory to work in, and an embedding for the values
which it carries, all in scope. A `Decimalizer` formats its sizes and rates:

```scala
import embeddings.automaticEmbedding
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory

given device: BenchmarkDevice = LocalhostDevice
given decimalizer: Decimalizer = Decimalizer(2)
```

A `Bench` measures speed. Its body is a quoted expression, which is [staged](staging.md),
and dispatched in a JVM of its own, so that the measurement is not distorted by the rest of
the suite:

```scala fuel
typecheck
##
val bench = Bench()

bench(m"decode directly")(target = 1*Second, operationSize = 1000*Byte):
  '{ Benchmarks.decodeUsersDirect() }
```

A `Stress` measures what a figure for throughput hides. It runs for a target of elapsed
time, at a stated concurrency, and may be given a constrained heap, or a limit on CPU, so a
design can be shown to hold up where it matters:

```scala fuel
typecheck
##
import threads.virtualThreads

val stress = Stress()
val constrained = Stress(heap = t"128m")

stress(m"cross-thread hand-off")(target = 2*Second, concurrency = 16):
  '{ Benchmarks.handOff() }
```

A `Profile` answers where the time went. It gives a histogram of the hottest methods,
colored by package:

```scala fuel
typecheck
##
val profile = Profile()

profile(m"pipeline hotspots")(target = 5*Second):
  '{ Benchmarks.pipeline() }
```

Benchmarks spread over axes as tests do. One program is staged for each distinct
implementation, and each cell is dispatched in a JVM of its own. A baseline anchors one
value of an axis, and the rest are shown relative to it. A stress *sweep* records its steps
as coordinates on an axis of a single test, not as a family of tests with different names.
[Charts](charts.md) draws the results.

### Running and selecting

A `Suite` is not a main class. A compiler plugin records every `Suite` object in the index
`META-INF/services/probably.Suite` of the jar which it compiles. The runner,
[fume](https://github.com/propensive/fume), discovers suites from that index on a built
classpath, runs each one in its own process, and displays the stream of `TestEvent`s which a
suite emits: live progress on a terminal, plain output in CI, each failure with its
contrast, and a nonzero exit status if any test failed. That is all that a build needs to
gate on.

A project names its classpath once, in `.pyrocosm/fume/config.tel`, so a bare `fume run`
runs everything. `fume list`, and the completion of test names in a shell, read the index of
tests, and so run nothing.

fume also accepts terms of selection, so a subset can be run without editing the code:

```tabular
column  Term
column  Selects
column  Combines by
row  a hash  the test with that hash  union
row  a moniker, such as `square`  the test with that moniker  union
row  a name glob, such as `'parser*'`  the tests whose names match  union
row  `--test`, `--bench`, `--stress`, `--profile`  the tests of that kind  intersection
row  an axis constraint, such as `parser=jacinta` or `'N<32'`  the cells matching it  intersection
```

Terms which identify tests are united with each other, and the rest are intersected with
that union:

```sh
fume run square 'parser*'
fume run --bench
fume run parser=jacinta 'N<32'
fume list
```

The first selects by moniker and then by a glob on names, the second runs only the
benchmarks, the third runs only the cells which match the constraints on the axes, and the
last lists what matches, and runs nothing. Assertions which are not selected never execute,
and benchmark cells which are not selected are skipped, so a selection costs only what it
runs.

A suite is not a main class, but an entry point for plain `java`, for CI, or for a host
which is too old to read the jar's schema of events, is one line.
`Suite.main(arguments, Tests)` runs the suites named, prints a line for each completed test,
its details and a summary, and exits with their status:

```tabular
column  Status
column  Means
row  `0`  every test passed
row  `1`  a test failed
row  `2`  a suite threw
```

`Suite.run` is the same, without exiting, for a host of its own.

<!-- doccheck: skip -->
```scala
object Tests extends Suite("parser", m"Parser tests"):
  def main(arguments: scala.Array[String]): Unit = Suite.main(arguments, Tests)
  def run(): Unit = ()
```

### Related

- [Errors](errors.md#inspecting-a-failure-directly) describes `capture`, which a test uses to
  assert that code fails in the right way.
- [Diffing](diffing.md) computes the differences which a contrast shows.
- [Randomness](randomness.md#reproducing-a-run) generates reproducible test data.
- [Terminal emulation](terminal-emulator.md) tests what a terminal program displays.
- [Environment](environment.md#overriding-for-a-block) gives a test a fixed environment.
- [Browser DOM](dom.md), [web automation](web-automation.md), [time](time.md) and [stack traces](stack-traces.md) describe what tests drive, control and report.
