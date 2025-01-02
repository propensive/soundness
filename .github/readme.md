[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/metamorphose/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/metamorphose/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Metamorphose

__Efficient permutation representations__

A permutation is a rearrangement of the elements in an ordered collection. It
is a one-to-one mapping—a bijection—and therefore reversible.
[Lehmer codes](https://en.wikipedia.org/wiki/Lehmer_code) provide a mapping
between positive integers and permutations, which make it possible to describe
a permutation of _n_ elements in _O(nlog(n))_ space. __Metamorphose__  provides
an implementation of this encoding.

## Features

- efficient representation of permutations of collection elements
- bijection between positive integers and permutations
- provides a binary serialization of permutations


## Availability





## Getting Started

Metamorphose primarily provides the `Permutation` class, which is in the
`metamorphose` package and exported to the `soundness` package. You can import
it with,
```scala
import metamorphose.*
```
or,
```scala
import soundness.*
```

There are two ways to construct a new `Permutation`. Firstly, from an ordered
sequence of distinct indexes, for example,
```scala
val permutation = Permutation(Vector(1, 4, 2, 3, 0))
```
which interprets the ordering of the indexes to yield the permutation with
factoradic number 45, i.e. `Permutation(Factoradic(45))`.

Or, if we already know the factoradic number of the permutation, we can pass
that in directly, using the `Factoradic` costructor, like so:
```scala
val permutation = Permutation(Factoradic(45))
```

To check that this is the same permutation, we can expand it with,
```scala
val order: List[Int] = permutation.expansion
```
which gives the original sequence back: `List(1, 4, 2, 3, 0)`.

It's also possible to see the Lehmer code derived from this permutation with,
```scala
permutation.lehmer
```
which produces, `List(1, 3, 1, 1, 0)`.

We can apply this permutation to a sufficiently long sequence just by applying
it to the sequence, i.e. `permutation(list)`.

Permutations are always reversible, and `Permutation#inverse` will construct a
new permutation which will permute a sequence permuted by the original
permutation back to the original list.

That is, for all lists and permutations of the right size it is true that,
```scala
permutation.inverse(permutation(list)) == list
```


## Status

Metamorphose is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Metamorphose is designed to be _small_. Its entire source code currently consists
of 223 lines of code.

## Building

Metamorphose will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Metamorphose?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Metamorphose's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Metamorphose and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `metamorphose`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Metamorphose's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Metamorphose are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/metamorphose/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Metamorphose
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Metamorphose was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

When an insect metamorphoses, it permutes the elements of its body from one
form into another.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a butterfly, which has undergone metamorphosis from a
caterpillar.

## License

Metamorphose is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

