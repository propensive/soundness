[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/turbulence/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/turbulence/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Turbulence

__Simple tools for working with data streams in `LazyList`s__

_Turbulence_ provides a few useful methods for working with `LazyList`s for streaming

## Features

- provides several stream-related operations on `LazyList`s
- can multiplex several streams into a single stream
- can cluster together short sequences of events which happen within a predefined period of time


## Availability Plan

Turbulence has not yet been published. The medium-term plan is to build Turbulence
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Turbulence.

Subsequently, Turbulence will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Operations on `LazyList`s

#### `Funnel`s

A `Funnel` receives asynchronous events, potentially from multiple threads, and puts them into a
`LazyList`

For example,
```scala
val funnel: Funnel[Int] = Funnel[Int]()
funnel.put(2)
Future(funnel.put(6))
val events: LazyList[Int] = funnel.stream
```

Note that evaluation of the `Funnel#stream` method constructs a `LazyList` which consumes events,
and should be called exactly once. Later releases of Turbulence will change the API to avoid this
trap.

#### Clustering

An event stream provided by a `LazyList[T]` may yield events irregularly, often with several events
happening at the same time. A simple event-handling loop, which performs a slow operation, such as,
```scala
stream.foreach: event =>
  slowOperation(event)
```
will incur a time cost for every event in the stream; so if the operation takes one second and ten
events arrive at around the same time, it will take about ten seconds from the first event arriving
until the last event is processed.

Sometimes it can be quicker to process events in a batch, or the results of processing earlier
events can be invalidated by the arrival of later events. In these cases, clustering the events on
a stream can be useful.

The `LazyList#cluster` extension method can transform a `LazyList[T]` into a `LazyList[List[T]]`. It
will group together sequences of events arriving with less than a specified gap in time between
them.

For example,
```scala
stream.cluster(1000).foreach: events =>
  slowOperation(events.last)
```
will effectively ignore all but the last event, but will not start processing an event until 1000ms
has passed without any new events.

As a more complete example, consider the event stream, `stream`, which produces events `0`-`9` at
the times shown in the "Time" column. 

Event   | Time    | Gap      | `stream`    | `stream.cluster(10)`   | `stream.cluster(100)`     |
-------:|--------:|---------:|------------:|-----------------------:|--------------------------:|
`0`     | `4ms`   |          | `0 @ 4ms`   |                        |                           |
`1`     | `8ms`   | `4ms`    | `1 @ 8ms`   | `{0,1} @ 18ms`         |                           |
`2`     | `15ms`  | `7ms`    | `2 @ 15ms`  | `{2} @ 25ms`           |                           |
`3`     | `26ms`  | `11ms`   | `3 @ 26ms`  | `{3} @ 36ms`           |                           |
`4`     | `75ms`  | `49ms`   | `4 @ 75ms`  |                        |                           |
`5`     | `80ms`  | `5ms`    | `5 @ 80ms`  |                        |                           |
`6`     | `85ms`  | `5ms`    | `6 @ 85ms`  |                        |                           |
`7`     | `90ms`  | `5ms`    | `7 @ 90ms`  | `{4,5,6,7} @ 100ms`    | `{1,2,3,4,5,6,7} @ 190ms` |
`8`     | `203ms` | `113ms`  | `8 @ 203ms` | `{8} @ 213ms`          | `{8} @ 303ms`             |
`9`     | `304ms` | `101ms`  | `9 @ 304ms` | `{8} @ 308ms`          | `{9} @ 308ms`             |
`END`   | `308ms` | `4ms`    |             |                        |                           |

The event streams `stream.cluster(10)` and `stream.cluster(100)` will produce results at different
times. Note that event `0` is not received on `stream.cluster(100)` until `190ms` after it is
produced, and likewise event `4` is not received on `stream.cluster(10)` until `25ms` after it
fires.

In the worst-case scenario, a stream steadily producing events with a gap slightly shorter than the
cluster interval will never produce a value! To mitigate this possibility, an optional second
parameter can be provided which specifies the maximum number of events to include in a single
clustered event, for example,
```scala
stream.cluster(100, 10)
```

The `LazyList#cluster` extension method expects a parameter of the contextual `Timekeeping` type.

#### Multiplexing

Multiple `LazyList` streams may be combined into a single stream by multiplexing them. The
extension method `LazyList.multiplex` takes a variable number of `LazyList` arguments to construct
a new `LazyList` from two or more existing `LazyList`s, for example:
```scala
val combinedStream = LazyList.multiplex(source1, source2, source3)
```

The type parameter of the resultant `LazyList` will be the least upper-bound of that of the input
streams.

#### Rate-limiting

Often a stream will produce results faster than desired if it is actively consumed. The
`LazyList#rate` method will guarantee a minimum amount of time passes between consecutive values.
If the elapsed time since the previous element already exceeds the minimum, it will be yielded
immediately.

For example,
```scala
LazyList.from(1).rate(100)
```
will count from `1`, yielding approximately ten numbers per second.

Note that a rate-limited `LazyList` which has already been partially or completely evaluated will
evaluate without any delay on subsequent reads.

#### Mutable Multiplexing

It may be desirable to add or remove streams from the set being multiplexed. This is possible with
a `Multiplexer` instance, which takes two type parameters: `K`, the type of the keys with which
streams will be associated, and `T`, the type of the elements in the resultant stream.

New streams may be added to the `Multiplexer` with the `Multiplexer#add` method, which takes a key
and a stream, and removed with the `Multiplexer#remove` method, taking just the key. For example,
```scala
val multiplexer = Multiplexer[Text, Int]()
multiplexer.add(t"Fibonacci", fib(0, 1).rate(500))
multiplexer.add(t"Naturals", LazyList.from(1).rate(350))
multiplexer.stream.take(10).foreach(println(_))
multiplexer.remove(t"Fibonacci")
multiplexer.stream.take(10).foreach(println(_))
multiplexer.close()
```

#### Tap

Sometimes it's useful to have direct control over when a `LazyList` is yielding values and when
it is "paused", using an external trigger. This functionality is provided by a `Tap`, a mutable
object which defines two methods, `open()` and `close()`, and holds the tap's current state.

Given a `LazyList[T]`, `stream`, the `regulate` extension method may be used to specify a `Tap`
which can control it.

For example,
```scala
val tap = Tap()
def regulatedStream = stream.regulate(tap)
```

Elsewhere, perhaps in another thread, `tap.close()` and `tap.open()` may be called to pause or
resume output on the `LazyList`. Any events which arise while the `Tap` is closed will be buffered,
and emitted when it is re-opened. Accessing `isEmpty`, `head` or `tail` on the `LazyList` will, of
course, block while the tap is closed.

#### Pulsar

A `Pulsar` provides a regular stream of `Unit` values and a predefined rate. It may be created
simply with the `pulsar` extension method on the `LazyList` object, taking a time duration as its
only parameter, for example,
```scala
LazyList.pulsar(1000L).foreach:
  unit => println("Hello")
```
will print `Hello` once per second, forever.






## Status

Turbulence is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Turbulence is designed to be _small_. Its entire source code currently consists
of 871 lines of code.

## Building

Turbulence will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Turbulence?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Turbulence's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Turbulence and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `turbulence`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Turbulence's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Turbulence are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/turbulence/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Turbulence
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Turbulence was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Turbulence_ describes multiple interacting flows, or streams, of fluids; this library makes it easier to streamline interacting streams.

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

The logo shows a turbulent (and colorful) vortex.

## License

Turbulence is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

