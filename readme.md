[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/superlunary/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/superlunary/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Superlunary

____



## Features



## Availability

Superlunary has not yet been published as a binary.

## Getting Started

## Quotes and Splices

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

Superlunary is classified as ____. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Superlunary is designed to be _small_. Its entire source code currently consists
of 86 lines of code.

## Building

Superlunary can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Superlunary are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/superlunary/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Superlunary easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Superlunary was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name



In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo



## License

Superlunary is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
