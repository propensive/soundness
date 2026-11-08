# Soundness Benchmarking Standards

This document defines how benchmarks are written and organised in the Soundness
libraries. Benchmarks build on [Probably](https://github.com/propensive/probably/)
and, for staged compiletime work, on [Sedentary](https://github.com/propensive/sedentary/).
It complements `testing.md`. Examples are drawn from `lib/*/src/bench/*.scala`.

## 1. Location and naming

Benchmarks live in a module's `src/bench` directory, in a file named
`<module>.Benchmarks.scala`. The object extends `Suite` and is conventionally
named `Benchmarks`:

```scala fuel
substitute ???  ...
##
object Benchmarks extends Suite("paths-bench", m"Serpentine benchmarks"):
  def run(): Unit = ???
```

The suite's id is the id of the module's tests with `-bench` after it.

## 2. Structure

A benchmark is declared with a `Bench` rig, which measures a body in place of the
assertion a test would make. The rig needs a `BenchmarkDevice` to dispatch to, and,
in scope where it is applied, a classloader, an environment, a system, a temporary
directory to stage in, and an error strategy for a body that fails to compile or run. Group related benchmarks with `suite`:

```scala fuel
typecheck
##
import classloaders.threadContextClassloader
import environments.javaBaseEnvironment
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory

object PathBenchmarks extends Suite("paths-bench", m"Serpentine benchmarks"):
  given device: BenchmarkDevice = LocalhostDevice

  def run(): Unit =
    val bench = Bench()

    suite(m"Conjunctions"):
      bench(m"find the conjunction of two paths")(target = 500*Milli(Second)):
        '{ (% / "foo" / "bar").conjunction(% / "foo" / "baz") }
```

The rig first runs the body untimed so that the JIT can reach a steady state, then
calibrates and times it in batches, as these parameters say:

```tabular
column  Parameter
column  Default
column  Meaning
row  `target`  none; required  how long the measured phase runs, all timed batches together
row  `warmups`  the value of `iterations`  calibration batches, which settle how many operations fit in one batch
row  `iterations`  `5`  timed batches
```

Choose a target large enough for a stable figure.

## 3. Measure a result, not a side effect

The body must produce a value that the benchmark consumes; otherwise the JIT may
prove the work unused and eliminate it, measuring nothing. Return the result of
the operation under test rather than discarding it, and never benchmark a body
whose value is thrown away.

## 4. Staged benchmarks

A benchmark's body is a quote, compiled again in the measuring JVM. Write the work
being measured in full inside the quote, so that each body reads as a self-contained
fragment, as Honeycomb's benchmarks do:

```scala fuel
parse
##
bench(m"Parse with jsoup")(target = 1*Second, operationSize = size):
  '{ org.jsoup.Jsoup.parse(honeycomb.Benchmarks.htmlText1).nn }
```

A body may refer to anything reachable by a static path, so it names members of the
benchmark object by their fully-qualified names (`honeycomb.Benchmarks.htmlText1`),
and values chosen in `run()` reach it only through a splice (`$size`). It cannot
refer to a `private` member. Reserve such members for:

- the data a body works on: corpora, caches and their builders;
- instances derived once, such as a codec from a derivation macro, so that a body
  measures their use rather than their expansion or allocation;
- instruments that many bodies share without being what they measure, such as a fixed
  `Buffering` or a rival's run adapter;
- arms that `run()` also calls, to check before timing that the rivals agree. These
  stay as methods, so that the code checked and the code timed are the same.

The quote is typed where it is written, but an inline method that resolves an instance
as it expands (through `summonFrom` or `summonInline`) only does so when the staged
tree is compiled, where none of the file's imports are in scope. An `import` written
inside the quote travels with it, so import there whatever such a method needs:

```scala fuel
parse
##
'{
    import htmlDoms.whatwg
    unsafely(honeycomb.Benchmarks.html1.load[Html])
}
```

A transparent inline macro given cannot be expanded inside a quote at all, so a call
that needs one (Panopticon's `lens` resolves an optic per path segment this way) must
be made in a method of the benchmark object, which the body then calls.

The harness binds the body as a method of its own, so a body written inline is
JIT-compiled exactly as one which delegates to a method.

## 5. Keep benchmarks out of the test suite

Benchmarks are not run as part of `make test` or the attested CI build; they are
run deliberately when measuring performance. Keep them in `src/bench` so they do
not slow the ordinary test cycle.
