[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/ulysses/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/ulysses/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Ulysses

__An implementation of Bloom filters for Scala__

Bloom filters are useful when a _probabilistic_ answer for whether a set
contains a value is sufficient, particularly when the cost of storing every set
element is prohibitive. _Ulysses_ provides an immutable representation of a
Bloom filter, with flexibility to tune its properties according to need.

## Features

- Provides a generic, immutable implementation of a Bloom filter
- Can be configured to use a variety of different hash functions
- Automatically determines Bloom filter parameters for target number of elements and error rate
- Uses Gastronomy to derive hashes for primitive, product and sum types


## Availability Plan

Ulysses has not yet been published. The medium-term plan is to build Ulysses
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Ulysses.

Subsequently, Ulysses will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### About Bloom Filters

A Bloom filter provides a way to test if a value is _probably_ in a set, or if
it is _certainly_ not in the set. That is, false positives for set membership
are permitted, while false negatives are not. The memory required by a Bloom
filter is dependent on the number of elements it should hold, and the
probability of false positives when testing for set membership, and these
parameters must be specified when the Bloom filter is created.

As well as determining the amount of memory the Bloom filter should use, the
approximate number of elements and the acceptable false-positive rate determine
the number of different hashes that will be used for each operation, with
optimal values chosen for each, transparently.

The Bloom filter only has two core operations: adding a value to the set, and
testing if a value is contained within the set. Since the Bloom filter does not
actually store any values, there are no methods for retrieval.

### Constructing a Bloom Filter

A new Bloom filter can be constructed with, for example:
```scala
import gastronomy.hashFunctions.crc32
val bloom = BloomFilter[Element](1000, 0.01)
```

This creates a new `BloomFilter` instance for storing elements of type
`Element`, optimized for 1000 elements, with a target error rate of 1%
(`0.01`). Additionally, the presence of the contextual value,
`hashFunctions.crc32` menas that the CRC32 hash function, defined in
[Gastronomy](https://github.com/propensive/gastronomy/), will be used to
calculate the hashes for addition and membership checks.

For creation, the Bloom filter additionally requires that its element types are
digestible, that is, a contextual `Digestible` instance exists. Gastronomy
provides `Digestible` instances for primitive types and will derive instances for
product and coproduct types.

A newly-created `BloomFilter` is empty, but an element can be added with the
`+` operator, or multiple elements with the `++` operator. Since `BloomFilter`s
are immutable, these will construct new `BloomFilter` instances.

### Using a Bloom filter

`BloomFilter` also provides the method, `mayContain`, which takes an instance of
the Bloom filter's type, and returns a `Boolean`. The interpretation of the
result should not be mistaken: `false` means that the value is guaranteed not
to be a member of the set represented by this Bloom filter, while `true` means
that the value is _probably_ a member of the set, but may not be.

For the Bloom filter above, constructed for approximately 1000 elements with an
error rate of 1%, if it has, indeed, had 1000 elements added to it, then there
is an estimated 1% chance that the `mayContain` method will return `true` for
an element which has not been added. That false-positive probably will increase
if significantly more elements are added to the Bloom filter, and would be
smaller had significantly fewer elements been added.




## Status

Ulysses is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Ulysses is designed to be _small_. Its entire source code currently consists
of 75 lines of code.

## Building

Ulysses will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Ulysses?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Ulysses's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Ulysses and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `ulysses`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Ulysses's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Ulysses are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/ulysses/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Ulysses
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Ulysses was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Ulysses_ is named after the novel by James Joyce, whose principal character is Leopold Bloom, a namesake of the creator of Bloom Filters, Burton H. Bloom.

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

The logo shows a blooming lotus flower, alluding to the _Bloom_ filters that Ulysses provides.

## License

Ulysses is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

