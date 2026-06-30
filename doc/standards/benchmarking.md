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
body is a quote elaborated in a fresh staging context that does not see the file's
contextual givens (such as `Tactic`s). Define any helpers the body needs as
top-level functions, so they are available in the staged context rather than
captured from the enclosing scope.

## 6. Keep benchmarks out of the test suite

Benchmarks are not run as part of `make test` or the attested CI build; they are
run deliberately when measuring performance. Keep them in `src/bench` so they do
not slow the ordinary test cycle.
