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

### Quotes and Splices

A Scala 3 macro, written using quotes and splices syntax, typically looks something like this:
```scala
import scala.quoted.*

def say(user: String)(using Quotes): Expr[Unit] =
  val name: Expr[String] = Expr(user)
  '{Out.println("Hello "+${name})}
```

The usage of `'{...}` (quotes) and `${...}` (splices) are indicative of _phase shifts_.
Code inside quotes will be executed one phase later than the surrounding code, and code
inside a splice will be executed one phase earlier than the code surrounding it. In the
example above, the definition of `name` and the usage of `name` occur in the same phase
(and *must* occur in the same phase, due to the _Phase Consistency Principle_),
while `Out.println` and `"Hello "` are in the next phase. An instance of `Expr[String]`
or `Expr[Unit]` is an abstract notion of an expression that will become a `String` or a
`Unit` in the next phase.

For a macro, that "next phase" will be a later compilation, when a new instance of the
compiler is run, and all code from prior phases exists as compiled bytecode, and can be
run.

A similar effect could be achieved just by writing the code in a separate file, and
compiling it later, but the clever part is that quotes and splices can be interleaved
at a fine-grained level; as expressions. And furthermore, those expressions are
typechecked.

But quotes and splices and the concept of phases can be applied more generally than in
plain macros. The "next phase" does not have to be "the next compilation"; a quoted
block can represent any code which runs "elsewhere". There are a world of
possibilities for where "elsewhere" could be, but it could be inside another JVM,
on a cloud-based system, or inside a browser using ScalaJS.

_Superlunary_ provides the wiring to make it easy to exploit the powerful syntax and
consistency-checking of quoted code, to make it possible to deploy the code inside
quotes to an environment of your choosing.




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
of 96 lines of code.

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

