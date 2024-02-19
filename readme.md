[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/superlunary/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/superlunary/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Superlunary

__Exploiting lightweight modular staging__

When we write a program in Scala, _usually_ all the code that is compiled
together will be run together (along with dependencies), in the _same
environment_: a single instance of the JVM, or in a browser with Scala.JS.
(Macros are a notable exception, since they are run during a later compilation,
but they generally appear in _library_ rather than _application_ code.)
Conversely, in distributed applications, code which is intended to run in
_different environments_ would be compiled separately, and would remain
separate from source code to its execution in separate JVMs, web browsers, and
maybe multiple machines or docker instances.

Examples include client-server communications between an HTTP server and a web
browser, and microservice-based architectures. For a distributed application,
there is an inherent contract between any two distinct environments in the
system.

However, our usual approach to development means that this contract is not
enforced by the compiler. Even though other tools may be employed to ensure
contractual consistency, this must happen externally to the compiler, and
rarely offers the same strong guarantees that Scala can. So contractual
consistency can be compromised, and lead to runtime failures.

_Superlunary_'s model compiles source code to be run in different environments
_together_, using _quotes and splices_ to precisely and safely delimit local
from remote code. This allows code which runs in a remote environment to be
written alongside local code; to be fully checked by the compiler; to be
marshalled and unmarshalled transparently; and to be maintained in lockstep.

_Superlunary_ makes it possible to develop a distributed application with the
versatility, simplicity and self-consistency as an application which runs
within a single runtime environment.

## Features

- write remote code in-place using quotes and splices
- embed local values seamlessly into remote code
- lightweight, yet clearly-delimited code
- compile code using a custom compiler, to be run remotely
- ensure contractual consistency between local and remote code
- applications in microservice architectures and client-server scenarios



## Availability Plan

Superlunary has not yet been published. The medium-term plan is to build Superlunary
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Superlunary.

Subsequently, Superlunary will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

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



## Status

Superlunary is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Superlunary is designed to be _small_. Its entire source code currently consists
of 141 lines of code.

## Building

Superlunary will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Superlunary?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Superlunary's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Superlunary and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `superlunary`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Superlunary's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Superlunary are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/superlunary/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Superlunary
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Superlunary was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

That which is _superlunary_ (by contrast to _sublunary_), is literally "beyond the moon", and hence "otherworldly", and describes the code in quotes which Superlunary enables to run elsewhere.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a moon, reflected in water.

## License

Superlunary is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

