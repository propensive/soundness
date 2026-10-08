# Soundness Benchmarking Standards

This document defines how benchmarks are written and organised in the Soundness
libraries. Benchmarks build on [Probably](https://github.com/propensive/probably/)
and, for staged compiletime work, on [Sedentary](https://github.com/propensive/sedentary/).
It complements `testing.md`. Examples are drawn from `lib/*/src/bench/*.scala`.

## 1. Location and naming

Benchmarks live in a module's `src/bench` directory, in a file named
`<module>.Benchmarks.scala`. The object extends `Suite` and is conventionally
named `Benchmarks`:

```scala
object Benchmarks extends Suite(m"internal Benchmarks"):
  def run(): Unit = ...
```

## 2. Structure

A benchmark is a `test` block whose result is measured, with `.benchmark`
applied to it in place of an assertion. Group related benchmarks with `suite`:

```scala
suite(m"Show performance"):
  test(m"render a quantity"):
    (7.567*Metre).show
  . benchmark(warmup = 30000L, duration = 30000L)
```

`warmup` and `duration` are durations in milliseconds: `warmup` lets the JIT reach
a steady state before measurement, and `duration` is how long the measured phase
runs. Choose values large enough for a stable figure.

## 3. Measure a result, not a side effect

The body must produce a value that the benchmark consumes; otherwise the JIT may
prove the work unused and eliminate it, measuring nothing. Return the result of
the operation under test rather than discarding it, and never benchmark a body
whose value is thrown away.

## 4. Compiletime benchmarks

To measure compilation cost rather than runtime cost, wrap the body in
`deferCompilation`, so the benchmark times the compiler resolving and elaborating
the code:

```scala
test(m"resolve a Show instance"):
  deferCompilation:
    Group(List(Person("Jack", 30))).show
. benchmark(warmup = 30000L, duration = 30000L)
```

## 5. Staged benchmarks

For benchmarks that run staged code through Sedentary's `Bench` rig, the benchmark
body is a quote, compiled again in the measuring JVM. Write the work being measured
in full inside the quote, so that each body reads as a self-contained fragment:

```scala
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

```scala
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

## 6. Keep benchmarks out of the test suite

Benchmarks are not run as part of `make test` or the attested CI build; they are
run deliberately when measuring performance. Keep them in `src/bench` so they do
not slow the ordinary test cycle.
