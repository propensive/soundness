### Background

#### Quotes and Splices

A Scala 3 macro, written using quotes and splices syntax, typically looks something like this:
```scala
import scala.quoted.*

def say(user: String)(using Quotes): Expr[Unit] =
  val name: Expr[String] = Expr(user)
  '{Out.println("Hello "+${name})}
```

The usage of `'{...}` (quotes) and `${...}` (splices) are indicative of _phase
shifts_.  Code inside quotes will be executed one phase later than the
surrounding code, and code inside a splice will be executed one phase earlier
than the code surrounding it. In the example above, the definition of `name`
and the usage of `name` occur in the same phase (and *must* occur in the same
phase, due to the _Phase Consistency Principle_), while `Out.println` and
`"Hello "` are in the next phase. An instance of `Expr[String]` or `Expr[Unit]`
is an abstract notion of an expression that will become a `String` or a `Unit`
in the next phase.

For a macro, that "next phase" will be a later compilation, when a new instance
of the compiler is run, and all code from prior phases exists as compiled
bytecode, and can be run.

A similar effect could be achieved just by writing the code in a separate file,
and compiling it later, but the clever part is that quotes and splices can be
interleaved at a fine-grained level with multiple levelse of nesting; as
expressions. And furthermore, those expressions are typechecked for
_consistency across phases_.

But quotes and splices and the concept of phases can be applied more generally
than in plain macros. The "next phase" does not have to be "the next
compilation"; a quoted block can represent any code which runs "elsewhere".
There are a world of possibilities for where "elsewhere" could be: it could be
in another JVM, on a cloud-based system, or inside a browser using ScalaJS.

_Superlunary_ provides the wiring to make it easy to exploit the powerful
syntax and consistency-checking of quoted code, to make it possible to write
code with seamless syntax which can be dispatched to run in an environment of
your choosing, with very little effort.

### Usage

_Superlunary_ provides the most general mechanism to make it possible to
remotely run code which is written in an inline style. The library could be
employed in a wide variety of different projects.

Other libraries may use _Superlunary_ to provide wiring for different remote
environments. (Note that we will generally use "remote" to mean "external to
the current JVM". It may return to a different physical computer accessed
across a network, or may not.) These would typically be its direct dependents,
and would implement the logic necessary to package and deploy arbitrary code to
the environment in which it would be run, spawn the environment, provide its
input parameters, launch it and capture its result. These implementations would
be very different for code being deployed to a web browser, compared to code
deployed to a docker container or cloud virtual machine.

These libraries will be called _dispatch providers_.

Further libraries (or applications) may make use of one or more dispatch
providers as a convenient way to run their code in a different environment, and
could be maintained by entirely different developers. Libraries like this will
need to implement that code using _quotes and splices_ syntax, but will present
it to dependents as ordinary methods, like any other API.

These libraries will be called _dispatch clients_.

Any downstream libraries may call these methods, blissfully unaware that
_Superlunary_ is involved.

The two "interesting" users of _Superlunary_ are dispatch providers and
dispatch clients.

#### Writing a Dispatch Client

All terms and types are defined in the `superlunary` package:
```scala
import superlunary.*
```

Writing a dispatch client presumes a dispatch provider, which may come from a
third-party library, or may be defined locally. For now, we will assume that we
have a dispatch provider, an instance of `Dispatcher`, which will dispatch some
code _somewhere else_ to run. `Dispatcher` is designed so that _somewhere else_
can remain abstract.

Thus, given a `Dispatcher` object, called `Remote`, we can call its 'dispatch'
method, passing in a quoted block of code, like so:
```scala
def run(): Unit =
  Remote.dispatch:
    '{ println("Hello world") }
```

Invoking `run()` will dispatch the code `println("Hello world")` to its remote
environment, run it, and return the result. The result is just the `Unit` value,
so it's not very interesting. And unless there's a console connected to the
remote environment, we won't be able to see the words, `Hello world`.

But if it returns without an exception, then we can assume it executed
successfully.

A more interesting example could return a value. The
[Inimitable](https://github.com/propensive/inimitable/) library provides a
method to return a UUID corresponding to the currently running JVM instance,
which remains static for the lifetime of the JVM. We can use this to check that
the code is running on a different JVM:
```scala
import inimitable.*

def run(): Unit =
  val remoteUuid: Uuid = Remote.dispatch('{jvmInstanceId})
  val localUuid: Uuid = jvmInstanceId
  println(t"$remoteUuid vs $localUuid")
```

Here, the same invocation is called twice: once remotely, and once locally, and
we print both for comparison. We should see two different values.

Note a significant change in the `Remote.dispatch` call: it is now returning a
`Uuid`, rather than `Unit`. This allows us to get a value back from the remote
JVM. Note also how simple the remote code is—a single expression—and that we
are able to access it without a prefix, because the `inimitable` import outside
of the quotes is sufficient for its name to be resolved.

That is only true for _static prefixes_: those names which can be resolved
statically. It would not make sense to be able to access heap references which
exist within the memory of the local JVM, but have no meaning on the remote.
And this is exactly the protection that the _phase consistency principle_ gives
us.

#### Marshalling

The remote code defined inside the `dispatch` call returns a `Uuid`, which (at
that point) will exist only as a heap value in the remote JVM. In order for it
to exist within the local JVM, it must be transmitted by some means. There are
many ways this _could_ happen, but currently _Superlunary_ uses
[Jacinta](https://github.com/propensive/jacinta/) to serialize and deserialize
values to and from JSON strings.

This is possible for any type that has both a `JsonEncoder` and `JsonDecoder`
typeclass instance, and it happens transparently, entirely _behind the scenes_.
As long as the necessary typeclass instances exist (or can be automatically
constructed by generic derivation), _Superlunary_ will apply the encoding and
decoding wherever necessary.

#### Providing inputs

It's possible to pass local values into the remote code, much as values are
substituted into an interpolated string, or (even moreso) spliced into a macro.

The only requirement is that values be spliced as `Expr`s. That is, if `x` is
an `Int`, we must splice it as an `Expr[Int]`, which is trivially possible by
calling the `put` extension method on it.

For example, we can check the lag between local and remote execution with a
`lag` method:
```scala
def lag(currentTime: Long): Long = remote.dispatch:
  '{System.currentTimeMillis - ${currentTime.put}}
```
or even more directly:
```scala
def lag(): Long = remote.dispatch:
  '{System.currentTimeMillis - ${System.currentTimeMillis.put}}
```

As with return parameters, all marshalling to and from JSON is handled
automatically by _Superlunary_.

