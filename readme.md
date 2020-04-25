<a href="https://furore.dev/propensive/probably"><img src="/doc/images/furore.png" style="vertical-align:middle" valign="middle"></a>&nbsp;<a href="https://furore.dev/propensive/probably">__Develop Probably with Fury__ </a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://riot.im/app/#/room/#propensive.probably:matrix.org"><img src="/doc/images/riot.png" style="vertical-arign:middle" valign="middle"></a>&nbsp;<a href="https://riot.im/app/#/room/#propensive.probably:matrix.org">__Discuss Probably on Riot__</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://search.maven.org/search?q=g:com.propensive%20AND%20a:probably_2.12"><img src="/doc/images/mavencentral.png" style="vertical-arign:middle" valign="middle"></a>&nbsp;<a href="https://search.maven.org/search?q=g:com.propensive%20AND%20a:probably_2.12">__Get Probably from Maven Central__</a>

<img src="/doc/images/github.png" valign="middle">

# Probably

__Probably__ is a testing library designed to unintrusively provide test recording and reporting capabilities to any codebase, regardless of the users&rsquo; choices of libraries or programming paradigms. __Probably__ can define and run unit tests and property tests. Its syntax is simple and unexciting, and its execution model has zero magic: it&rsquo;s the same as for any other program.

## Features

- no framework, reflection or control flow to understand
- ScalaCheck-style property testing
- tests may be run multiple times, with results aggregated
- automatic derivation of arbitrary instances
- functional API where it matters; impure calls where it's safe and practical


## Getting Started

_Probably_ defines only two primary types: a mutable `Runner` for recording test results and reporting back on
them, and `Test` definitions, whose instances are created by the `Runner`.

Although it is possible to construct and use different `Runner`s, the most typical usage is to use the global
singleton `Runner` called `test`, because for most purposes only one will be required. Defining a test is
simple. For example,
```scala
import probably._

test("the sum of two identical integers is divisible by two") {
  val x: Int = 7
  x + x
}.assert(_%2 == 0)
```

Note that the assertion takes a predicate lambda, which operates on the result from evaluating the body of the
test. It does not operate on the value directly. This clearly separates the process of running the test from the
check which is performed upon it.

When a test definition like this is encountered in running code, its body will be evaluated, and a predicate
(defined as a parameter to `assert`) will be evaluated on the result of the body. The outcome of this will be
one of four possibilities:
- the predicate returns true, and the test passes
- it returns false, and the test fails
- an exception is thrown while the body is being evaluated
- an exception is thrown while the assertion is being evaluated

These different cases will be distinguished in the test report.

It is important to note that a test can be defined anywhere, such as,
- in the `main` method of an application
- in a `lazy val`
- inside a `Future`
- in a parameterized method, testing a property of that parameter
- in the request handler on a web server
- in a pattern extractor (`unapply` method)
- inside an actor
- in the Scala REPL
- as one branch of a case clause
- nested inside another test

Regardless of where the test is defined, the behavior is always the same: it will be evaluated, checked, and the
result will be recorded in the `Runner`, as a side-effect. Tests may be run more than once (in which case they
are recorded more than once, and aggregated) or not at all if, by virtue of some runtime criterion, they are
simply not executed. The question of whether the test is executed is the same for 

The decision to make the `Runner` mutable reflects the power of Scala's hybrid nature. The state of the `Runner`
is write-only while the tests are being run, so many of the common concurrency problems which arise with mutable
state do not apply. The `Runner` has one read-only method, `report()`, which will produce a summary report of
the recorded test results. Reports may be produced many times, but normally `report()` is called just once, at
the end. This conscious and careful compromise in functional purity buys convenience: integration of tests does
not impose constraints on new code, or require non-local changes to existing code.

### Parameterized tests

As tests may appear anywhere, they are easy to parameterize. We could, for example, rewrite the test above like
so,
```
import probably._

def runTest(x: Int): Unit =
  test("the sum of three identical integers is divisible by 3") {
    x + x + x
  }.assert(_%3 == 0)

runTest(2)
runTest(50)
runTest(Int.MaxValue)
```

However, if the test were to fail, it would be useful to know what input caused it to fail. Any number of inputs
can be logged by including them as additional named parameters after the test name, like this:
```
import probably._

def runTest(x: Int): Unit =
  test("the sum of three identical integers is divisible by 3", input = x) {
    x + x + x
  }.assert(_%3 == 0)
```

The choice of the parameter name `input` is the user&rsquo;s choice: any name that is a valid identifier may be
chosen.

### Property-based testing

The ability to run the same test multiple times with different parameters suggests an obvious approach to
property-based testing: to run the same test over and over again with a stream of different inputs. _Probably_
also provides the means to generate such streams of increasingly-obscure instances for a variety of primitive
types, and will derive generators on-demand for case-class and sealed-trait types for which generators exist for
each of the parameters.

```scala
import probably._

case class Person(name: String, age: Int)

Generate.stream[Person](1000).foreach { person =>
  test("all persons have realistic ages", v = person) {
    person.age
  }.assert { a => a >= 0 && a < 100 }
}
```

For a given `Seed`, the pseudorandom data generated will always be deterministic and hence repeatable.

### Command-line Interface

_Probably_ comes with a simple CLI runner for running tests through the standard shell interface.


### Test Expression

_Probably_ provides a second way of defining a test: as an expression. For example,
```scala
import probably._

test("check the backup exists") {
  new File("data.bak")
}.check(_.exists).setReadOnly()
```

This style should look familiar, apart from one superficial difference: the test predicate is applied to a
method called `check` instead of `assert`. This transforms the test from a statement into an expression, which
means that it returns the result of its body, instead of `Unit`. Note that it returns the value, regardless of
whether the test passes or fails, and execution continues.

This confers a few further differences with assertion tests:
- exceptions thrown inside the body are not caught (but are recorded); exceptions in the check are still caught
- test expressions cannot be skipped; their return value is necessary for execution to continue


## Availability

Probably&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/probably`.
```
fury layer clone -i propensive/probably
```
or imported into an existing layer with,
```
fury layer import -i propensive/probably
```
A binary will be made available on Maven Central.

## Contributing

Contributors to Probably are welcome and encouraged. New contributors may like to look for issues marked <a href="https://github.com/propensive/probably/labels/good%20first%20issue"><img alt="label: good first issue" src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>

Contributors who follow the instructions in the
The [Contributing Guide](/contributing.md) exists to make this process easier.

## Author

Probably was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Probably is made available under the [Apache 2.0 License](/license.md).
