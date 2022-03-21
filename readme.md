[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/turbulence/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/turbulence/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/turbulence-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/turbulence-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Turbulence

_Turbulence_ provides a few useful methods for working with `LazyList`s for streaming

## Features

- provides several stream-related operations on `LazyList`s
- can multiplex several streams into a single stream
- can cluster together short sequences of events which happen within a predefined period of time


## Availability

The current latest release of Turbulence is __0.4.0__.

## Getting Started

## Operations on `LazyList`s

### Clustering

An event stream provided by a `LazyList[T]` may yield events irregularly, often with several events
happening at the same time. A simple event-handling loop, which performs a slow operation, such as,
```scala
stream.foreach:
  event => slowOperation(event)
```
will incur a time cost for every event in the stream; so the operation takes one second and ten
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
stream.cluster(1000).foreach:
  events => slowOperation(events.last)
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
clustered event.

### Multiplexing

TBC

## Related Projects

The following _Scala One_ libraries are dependencies of _Turbulence_:

[![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

The following _Scala One_ libraries are dependents of _Turbulence_:

[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

## Status

Turbulence is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Turbulence is designed to be _small_. Its entire source code currently consists of 235 lines of code.

## Building

Turbulence can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Turbulence are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/turbulence/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Turbulence easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Turbulence was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Turbulence_ describes multiple interacting flows, or streams, of fluids; this library makes it easier to streamline interacting streams.

## License

Turbulence is copyright &copy; 2022 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
