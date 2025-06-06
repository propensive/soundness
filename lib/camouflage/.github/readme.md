[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/camouflage/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/camouflage/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Camouflage

__An LRU cache implementation for Scala__

Caching provides a useful tradeoff between the costs of _time_ and _space_; that is, a cache can reduce the
amount of computation time at the expense of using more memory. _Camouflage_ provides one implementation of a
cache which will be appropriate for some (but not all) applications: a least-recently-used cache, or LRU cache.

## Features

- provides an implementation of a least-recently-used (LRU) cache
- cache requires only a _size_ parameter to initialize
- serves as a simple drop-in memoization wrapper around expensive method calls

## Availability







## Getting Started

All Camouflage terms and types are defined in the `camouflage` package:
```scala
import camouflage.*
```

### LRU Caches

An LRU cache is for storing a limited number of values which are expensive to compute. Values are added to the
cache when they are computed, and can be accessed in constant time by referencing them with a key. If the cache
already contains its limit of values, an existing value must be "evicted" to make space for the new value. The
value chosen is whichever has been in the cache the longest time without being accessed; in other words, the
_least-recently-used_ value.

### `LruCache`

To construct an LRU cache, specify a key and value type, and maximum size to the `LruCache` constructor:
```scala
import galilei.Path
import anticipation.Text

val cache = LruCache[Path, Text](100)
```

This `LruCache` will behave like a `Map[Path, Text]`: its values are `Text`s, indexed by `Path`s.

A single `apply` method is used to both retrieve and update a value in the cache, much like `getOrElseUpdate` in
a `Map`. For `LruCache`, the parameters are curried, since the second _ought_ to represent a costly
computation—since that is the purpose of caching.

Here is an example of using the `LruCache` to cache reading two files from disk:
```scala
import galilei.File
import galilei.filesystemOptions.{dereferenceSymlinks, doNotCreateNonexistent}
import contingency.strategies.throwUnsafely
import hieroglyph.charDecoders.utf8
import hieroglyph.badEncodingHandlers.skip
import serpentine.{%, p, Slash}
import serpentine.hierarchies.unix
import turbulence.readAs

val dataDir = % / "home" / "data"

def read(): Unit =
  cache(dataDir / "intro.txt"):
    (dataDir / "intro.txt").as[File].readAs[Text]

  cache(dataDir / "chapter1.txt"):
    (dataDir / "chapter1.txt").as[File].readAs[Text]
```

A subsequent call to,
```scala
def readAgain(): Text = cache(dataDir / "chapter1.txt"):
  (dataDir / "chapter1.txt").as[File].readAs[Text]
```
would retrieve the stored `Text` value from the cache, and the code which reads the file from disk would not be
executed.

This is true _unless_ the cache has filled up in the meantime and the text of `chapter1.txt` has been evicted.
And for an `LruCache` of size `100`, `chapter1.txt` would only be evicted if 100 distinct additions or accesses
of other keys were made since the addition or last access of `chapter1.txt`.

Here is a complete example of a tiny `LruCache`:
```scala
import turbulence.Out
import turbulence.stdioSources.virtualMachine
import gossamer.t

def numbers(): Unit =
  val cache: LruCache[Int, Text] = LruCache(4)

  Out.println(cache(1)(t"one"))
  Out.println(cache(2)(t"two"))
  Out.println(cache(3)(t"three"))
  Out.println(cache(4)(t"four"))
  Out.println(cache(1)(t"uno"))  // (1)
  Out.println(cache(5)(t"five")) // (2)
  Out.println(cache(1)(t"ein"))  // (3)
  Out.println(cache(2)(t"dos"))
```

The output would be: `one`, `two`, `three`, `four`, `one`, `five`, `one`, `dos`. Note that values retrieved on
the line marked _1_ is not `uno`, since at this point, the cache already contains the value `one` and the second
parameter (which produces `uno`) is not evaluated.

On the line marked _2_, a fifth value, `five`, is inserted. This would exceed the maximum size of the cache, so
one element must be evicted. That value is `two` because it is the least-recently-used value. Were it not for
the previous line (_1_), `one` would be the least-recently-used value, and the line marked _3_ would result in
the evaluation of `ein` and its insertion into the cache; but since `one` remains in the cache at the same
index, it is retrieved instead.


## Status

Camouflage is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Camouflage is designed to be _small_. Its entire source code currently consists
of 94 lines of code.

## Building

Camouflage will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Camouflage?".

1. *Copy the sources into your own project*

   Read the `fury` file in the repository root to understand Camouflage's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Camouflage and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.

   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `camouflage`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Camouflage's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Camouflage are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/camouflage/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Camouflage
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Camouflage was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Camouflage is used for hiding something, which is the original meaning of _caching_.

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

The logo shows a swatch of fabric in typical military camouflage colors.

## License

Camouflage is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).
