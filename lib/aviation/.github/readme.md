[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/aviation/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/aviation/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Aviation

__Representations of time, in its various forms__

The representation of time is complex, in part because the domain is
intrinsically complicated, and in part because we mean by "time" is
different in different contexts. _Aviation_ is an attempt to rationalize this
complexity by providing immutable representations of a variety concepts
relating to time and operations between them, taking advantage of the
opportunities Scala offers to make these APIs as intuitive as possible.

## Features

- representations points in time and lengths of time, in both exact/universal and civil forms
- types are all immutable and typesafe
- intuitive constructors for civil date and time values
- customisable rules for adding civil time units
- supports different calendar systems
- use and convert between different timezones and calendar systems


## Availability







## Getting Started

_Aviation_ provides a variety of types representing and working with different time-related concepts.

### On Time

_Time_ is a complicated concept in several ways. On a physical abstraction, we
can pinpoint an instant in time and order it as happening _before_ or _after_
another instant, provided they happen in the same place. We can consider the
difference between two instants, and measure it by comparison to reference
events that occur with a known regularly. We can relate _time_ to _distance_
using the speed of light, to [partially
order](https://en.wikipedia.org/wiki/Special_relativity) events in different
locations.

In a geological and heliocentric context, we talk about instants and durations
with reference to the rotation of the Earth around the sun, the rotation of
Earth on its axis, and the phase of the moon, as well as with arbitrary
subdivisions based on historical convention and [highly composite
numbers](https://en.wikipedia.org/wiki/Highly_composite_number), and sometimes
religious convention.

The measurements we use may have different meanings in different locations on
Earth (or conversely, the same measurement may be represented differently in
different locations), and different systems for representing the measurements
have been in use at different times in different places. The relationships
between different units of measurement are [not usually
decimal](https://en.wikipedia.org/wiki/12-hour_clock), [not always
constant](https://en.wikipedia.org/wiki/Thirty_Days_Hath_September), not always
exact subdivisions, and [may not even be known in
advance](https://en.wikipedia.org/wiki/Leap_second). The same representation
may refer to distinct events in the same place, and some representations may
not represent valid events on a handful of occasions. The system of
representation may depend on the prevailing political authority at the time,
which can depend on war.

_Aviation_ makes an ambitious effort to rationalize, systematize and encode
as much of this complexity as possible.

### Instants and Durations

We can represent a moment in time, without reference to any other macroscopic
entity (such as the Earth, and units derived from it), as an `Instant`. An
`Instant` is represented, internally, as a 64-bit integer corresponding to the
number of non-leap milliseconds since the UNIX epoch.

The difference between two `Instant`s is represented as a `Duration`, and
represented internally as a 64-bit integral number of milliseconds.

#### International Atomic Time (TAI)

In addition to `Instant`s (which do not distinguish leap seconds from the
second immediately preceding it), `TaiInstant` implements an instant in
International Atomic Time (TAI) which has no discontinuity at each leap second.

Although the concept of an "instant" and, in particular, the "duration" between
any two instants _ought_ to be unambiguous, it is not. This is due to the
existence of leap seconds, each of which increments the difference between the
numbers we use to represent the time in UNIX time (or UTC) and TAI. Since 1972
(when UTC and UNIX time already differed from TAI by ten seconds), twenty-seven
leap seconds have been added, either at the end or the middle of the year.
These were corrections, based on precise observations of the tiny deviations in
the Earth's movement which change its rate of rotation over the previous
months, to try to maintain the invariant that the sun's maximum elevation above
the Greenwich Meridian (Solar Noon) should be within one second of Clock Noon
(i.e. the time at which the clock digits read 12:00:00).





## Status

Aviation is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Aviation is designed to be _small_. Its entire source code currently consists
of 997 lines of code.

## Building

Aviation will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Aviation?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Aviation's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Aviation and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `aviation`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Aviation's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Aviation are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/aviation/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Aviation
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Aviation was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Tempus fugit_, or, _time flies_.

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

The logo shows a bird, the archetype of _aviation_, ascending in front of a setting sun.

## License

Aviation is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

