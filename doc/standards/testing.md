# Soundness Testing Standards

This document defines how tests are written and organised in the Soundness
libraries. Tests use [Probably](https://github.com/propensive/probably/). It
complements `syntax.md` (whitespace and formatting) and `benchmarking.md`
(performance measurement). Examples are drawn from `lib/*/src/test/*.scala`.

## 1. Location and naming

Tests live in a module's `src/test` directory. The test object extends `Suite`
and implements `run()`:

```scala
object Tests extends Suite(m"Eucalyptus tests"):
  def run(): Unit = ...
```

The suite name is a `Message`, naming the module under test. Where a module has
several distinct areas, each may have its own file (`<module>.<Area>Tests.scala`)
with its own `Suite`.

## 2. Structure

Group related tests with `suite`, and write each individual test with `test`,
both taking a `Message`:

```scala
suite(m"Parsing"):
  test(m"an empty document parses to an empty value"):
    t"".read[Json]
  . assert(_ == Json.empty)
```

A `test` block evaluates to the value under examination; the `assert` (or `check`)
that follows states what must hold of it. Keep one logical assertion per test, so
a failure names exactly what broke.

## 3. Test names

A test name is a short, descriptive `m"..."` phrase, **under 70 characters**,
stating the property being verified rather than the method being called. Prefer
`m"a malformed pointer fails to parse"` over `m"test parse"`.

## 4. Assertions

Assertions are written in the fluent style, with the predicate applied to the
block's result:

```scala
test(m"addition combines quantities of the same unit"):
  Metre + Metre*2.0
. assert(_ == 3*Metre)
```

Compare against an expected value with `_ == expected`; use a predicate for
looser checks. Do not throw or use `require`/`assert` from the standard library.

## 5. Error strategy

A test that exercises fallible code chooses an error strategy explicitly, usually
by importing `strategies.throwUnsafely`, so that an unexpected failure surfaces as
a failed test rather than a swallowed error:

```scala
import strategies.throwUnsafely
```

To test that a failure *does* occur, capture it and assert on its reason rather
than letting it propagate.

## 6. Compiletime testing

To assert that code is *rejected* by the compiler — that an impossible state truly
cannot be written — use [Larceny](https://github.com/propensive/larceny/), which
captures compile errors as values a test can inspect. A `// does not compile`
example in documentation should have a corresponding Larceny test.

## 7. What to test

Test the behaviour a user relies on: the result of each public operation, the
boundary cases (empty, absent, maximal), and the failures that should be raised.
Test that invalid constructions do not compile. Do not test private internals
directly; reach them through the public surface.
