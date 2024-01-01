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



