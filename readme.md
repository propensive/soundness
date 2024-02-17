[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/chiaroscuro/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/chiaroscuro/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Chiaroscuro

__Compare and contrast similar objects and highlight their differences__

A simple equality test can determine whether two values are identical or not,
but it offers no explanation for _how_ two unequal values are different.
_Chiaroscuro_ provides a means to compare two objects and see their structural
differences.

## Features

- structurally compare different values of the same type
- provides a recursive structural breakdown of differences between product types
- provides a diff (using Myers' algorithm) between sequence types with different elements
- presents results as an immutable datatype which can be rendered as a tabular tree of differences
- diff sequences can be customized for more meaningful output


## Availability Plan

Chiaroscuro has not yet been published. The medium-term plan is to build Chiaroscuro
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Chiaroscuro.

Subsequently, Chiaroscuro will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All Chiaroscuro terms and types are defined in the `chiaroscuro` package:
```scala
import chiaroscuro.*
```

Two values of the same type can be compared with the `contrastWith` method,
provided by Chiaroscuro. This will return an instance of `Semblance`,
describing the similarity of one value with the other, and will be one of three
cases:
- `Identical`, if the two values are the same
- `Different`, if the two values are different, without any further detail
- `Breakdown`, if the two values are different, with a breakdown of their
  similarities and differences

The last of these three cases is the most interesting, though it is only a
possible result for values of certain types, namely types which can be
destructured into components which can themselves be compared, recursively.
Simple types like `Text` (or `String`) and primitive types like `Int` or
`Double` can only ever be `Identical` or `Different`, but product types, like
case class instances, sum types, like enums or sealed traits, and sequence
types, like `List` or `IArray` can result in semblances which are neither
`Identical` nor `Different` but a `Breakdown`.

The three cases of `Semblance` are defined as follows:
- `Identical(value: Text)`
- `Different(left: Text, right: Text)`
- `Breakdown(comparison: IArray[(Text, Semblance)], left: Text, right: Text)`

`Identical` includes just a textual representation of the two identical values.
`Different` includes textual representations of _both_ values, since they will
not be the same. `Breakdown` also includes textual representations of the left
and right values, but additionally includes a sequence (an `IArray`) of
labelled `Semblance`s, each of which may be `Identical`, `Different` or another
`Breakdown`. This sequence represents a breakdown of the different components of
the two objects, comparing like-for-like, however the breakdown depends on the
type of objects being contrasted.

Typically, for product types such as case classes, the comparison sequence will
contain an entry for each parameter of the case class, showing whether that
parameter is the same or differs between the two values. In the case where it
differs, and the parameter is another product type, a nested `Breakdown` instance
may recursively show 

For sequence types, such as `List`, a diff between the elements of the left
sequence and the elements of the right sequence will yield a comparison
sequence, labeled for the index of the left and/or right sequence, where each
entry represents a one-to-one comparison between two elements, an addition (to
the right side) or a deletion (from the left side). Each one-to-one comparison
may be `Identical`, `Different` or a recursive `Breakdown` value.

This format provides a convenient and concise way of describing the structural
differences between two values. A contrast between two deep structures with few
differences will yield a tree structure where identical branches are pruned,
and only differing branches are expanded.

### _Similar_ elements

When performing a diff between two sequences of elements, whatever their type,
a comparison between any two elements will judge them to be either the same or
different, however small the difference. The non-appearance of an element in
one sequence and its appearance in the other would be considered a deletion or
insertion. In general, the result of a diff could be presented as an
alternating series of blocks of identical elements and blocks of deletions
(from the left side) and insertions (on the right side).

Often this is the best that can be achieved, and Chiaroscuro would present each
deletion and insertion as `Different` nodes: absent on one side, and present on
the other, with no further breakdown possible.

But what if further analysis on the blocks of differences (those between the
blocks of identical elements) were possible, and elements appearing on both
sides deemed _similar_ could be compared to each other? With a definition for
what it means for two elements to be _similar_ (for a given element type)
Chiaroscuro makes this possible, and provides a breakdown of the differences
between similar elements.

For example, consider the board members of an imaginary company,
```scala
import anticipation.Text
import gossamer.t

enum Role:
  case Ceo, Cto, Coo, Cmo, Cfo

case class Member(role: Role, name: Text)

val boardMembers: List[Member] = List(
  Member(Role.Ceo, t"Jane"),
  Member(Role.Cto, t"Leopold"),
  Member(Role.Coo, t"Simon"),
  Member(Role.Cmo, t"Helen")
)
```
being compared to the board members a year later:
```scala
val boardMembers2: List[Member] = List(
  Member(Role.Ceo, t"Leopold"),
  Member(Role.Cto, t"Linda"),
  Member(Role.Coo, t"Simon"),
  Member(Role.Cfo, t"Anna"),
  Member(Role.Cmo, t"Helen")
)
```

A simplistic diff would identify that the COO (Simon) and CMO (Helen) remained
unchanged, and two _sets_ of changes:
 - deletion of Jane (CEO), deletion of Leopold (CTO), insertion of Leopold
   (CEO) and insertion of Linda (CTO), and
 - insertion of Anna (CFO)

However, the first set of changes could be presented more simply if we were to
provide an indication of _similarity_ between two `Member` instances. Two
possibilities suggest themselves:
 1. two `Member`s with the same name are similar (regardless of role)
 2. two `Member`s with the same role are similar (regardless of name)

The presence of a `Similarity` typeclass instance can be used to specify this similarity. Defining,
```scala
given Similarity[Member] = _.role == _.role
```
```
will transform the `Semblance` output  comparing these two sequences to
directly compare `Member(Role.Ceo, t"Jane")` and
`Member(Role.Ceo, t"Leopold")`, resulting in a `Breakdown` indicating
the name having changed, and a similar name change for the corresponding
members with the `Role.Cto` role.

The insertion of a `Member` with the `Role.Cfo` role would remain as an
insertion.

Likewise, providing the alternative `given` definition,
```scala
given Similarity[Member] = _.name == _.name
```
would show Jane as a deletion, Leopold as a role change from CTO to CEO, and
Linda and Anna as insertions.

The implementation of `Similarity` can be any function comparing two elements,
and a good implementation can dramatically improve the readability of the
`Semblance` output. Some ideas for similarity implementations include considering:
- `Text` elements with a minimum edit distance less than 4 to be similar
- `Double` elements which differ by less than 5% to be similar
- `Json` objects with equal `"id"` fields to be similar
- a choice the same case of an enum (regardless of parameters) to indicate similarity






## Status

Chiaroscuro is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Chiaroscuro is designed to be _small_. Its entire source code currently consists
of 177 lines of code.

## Building

Chiaroscuro will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Chiaroscuro?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Chiaroscuro's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Chiaroscuro and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `chiaroscuro`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Chiaroscuro's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Chiaroscuro are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/chiaroscuro/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Chiaroscuro
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Chiaroscuro was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Chiaroscuro_ is a Renaissance painting technique of expressing strong contrasts between light and dark, while Chiaroscuro provides the means to highlight the contrasts between two values.

### Pronunciation

`/kɪˌɑːɹəˈskʊəɹəʊ/`

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

The logo shows a stylized crescent moon, illustrating the contrast of light against darkness.

## License

Chiaroscuro is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

