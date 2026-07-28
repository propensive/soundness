## Streams

### About

Data that arrives or departs over time — a file too large to hold in memory, a network
connection, the output of a process — is handled as a stream. One set of polymorphic operations
moves data between sources and sinks: `read` pulls a source in as a chosen type, `writeTo` sends
a value to a destination, and `stream` exposes a value as a stream of pieces.

What can be read, and what can be written, is decided by typeclasses. A type that describes how
it becomes a stream can be read; a type that describes how it consumes one can be written to; so
a new source or sink joins the same `read` and `writeTo` as everything else, with no bespoke
plumbing.

Underneath those operations is a streaming *kernel* whose central type is a `Stream`: not a lazy
sequence of chunks, but a pull endpoint over a buffer whose readable window is handed to exactly
one consumer, without copying. Backpressure is intrinsic to it, and the compiler enforces the
single ownership its zero-copy window depends on.

### On streaming

The JVM's streaming is a pair of mutable objects — an `InputStream` and an `OutputStream` — that
are read and written by side effect, one imperative call at a time, with the reader responsible
for buffering, decoding, and closing. Nothing in a method's type says whether it streams, what
element it streams, or how the bytes become text.

The obvious functional answer — a lazy list of chunks — fixes the typing and loses the
performance: every chunk is an allocation, every stage copies, and the memoization that makes a
lazy list replayable is exactly what keeps a large source alive in memory when nothing wanted it
kept.

Soundness takes a third route. A `Stream` is a *pull endpoint*: a consumer calls `refill` with
the amount it is prepared to take, and gets back a window into a buffer, which it reads and then
skips past. Nothing is copied and nothing is retained. A consumer that does not pull demands
nothing, so backpressure is not a mechanism bolted on but the absence of a call. Stages compose
as nested calls on the consumer's thread, with no queues and no synchronization, until a stage
explicitly crosses a thread boundary.

That design has one requirement: a window belongs to one consumer, and a stream must not be read
twice or shared. This is checked, not merely documented. Streams are exclusive capabilities, and
the streaming modules compile with [capture and separation checking](../philosophy/capture-checking.md)
enabled, so aliasing a stream, or letting one escape the scope that owns its source, is a
compile error. Where replay really is wanted, `memoize` drains the stream once into an immutable
value which may then be shared freely — the explicit, bounded replacement for a lazy list's
implicit caching.

Everything comes from the `soundness` package, with a character encoding and decoding in scope
for the text/byte boundary:

```scala
import soundness.*
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
```

### Reading a source

`read` pulls a source in as the type named. A source that yields bytes reads equally as `Text`,
as raw `Data`, or as a stream of either — the encoding in scope bridges bytes and text:

```scala
val source = t"The quick brown fox"

source.read[Text]            // t"The quick brown fox"
source.read[Data]            // the UTF-8 bytes
```

The same `read` works on any source with a `Readable` instance — a file, a network socket, a
process's output — so reading a URL as text and reading a file as bytes are the one operation.

### Writing to a sink

`writeTo` sends a value to a destination that knows how to consume it. The source may be text,
bytes, or a stream of either, and the destination's `Writable` instance receives it:

```scala
source.writeTo(destination)
```

### Making a value readable or writable

A type becomes a source by describing how it turns into a stream, and a sink by describing how
it consumes one. Each is a single-method typeclass:

```scala
case class Record(fields: List[Text])

given Record is Streamable by Text = record => Stream(record.fields.join(t","))
```

With that instance in scope a `Record` can be `read`, `writeTo` a destination, or `stream`ed,
without any further definitions.

### Streaming a value

`stream` turns a value into a pull endpoint, and `source` does the same through a `Streamable`
instance, naming the element type:

```scala
val bytes = t"The quick brown fox".in[Data].stream   // Stream[Data]
val text = document.source[Text]                     // Stream[Text]
```

### Composing a pipeline

A stage transforms a stream into a differently-typed stream. `via` attaches one on the pull
side, and the whole chain runs on the consumer's thread as nested refills — no threads, no
queues, no intermediate collections:

```scala
file.stream.via(decompressor).via(decoder)
```

The stage may be a `Duct` — the kernel's transformation type — or any descriptor value with a
`Ductile` instance, which is how compression formats, cipher modes and character codings all
present themselves as stages.

A pipeline ends in a *terminal* operation, which drains the endpoint and closes it:

```scala
stream.memoize                       // drain into one immutable value
stream.sweep((storage, start, n) => …)  // drain, seeing each raw window
stream.fold(0L)((total, storage, start, n) => …)  // window-level fold
```

`sweep` and `fold` expose the raw window rather than boxed elements, so a byte-level reduction
runs over the array with no per-element cost. `take` and `drop` bound a stream without draining
it, releasing the remainder of the upstream unread.

Where a pipeline ends in a *push* chain rather than a value, `pump` is the single point at which
data crosses from the pull side to the push side:

```scala
stream.pump(intake)
```

### Records

Between raw windows and materialized collections sits record-granularity streaming: rows,
events, frames, messages. A `Records[record]` is a stream whose windows are chunks of records,
so credit counts records rather than bytes. `delineate` splits a character source into lines,
and `records` iterates the parsed records of such a stream:

```scala
import lineSeparation.adaptiveLinefeedLineSeparation

t"1\n2\n3".source[Text].delineate.records.map(_.as[Int]).to(List)   // List(1, 2, 3)
```

Records are immutable values, so — unlike windows — they cross stage and thread boundaries by
reference.

### Standard streams

Standard output and error are reached through `Out` and `Err`, which print through the `Stdio`
capability in scope. Because standard output is a capability rather than a global, a test or a
daemon can redirect it:

```scala
Out.println(t"started")
Err.println(t"a warning")
```

### Producing a stream over time

Where the elements of a stream are produced by one part of a program and consumed by another, a
`Relay` bridges the two: any number of producers `put` records into it as they arise, one of
them eventually calls `stop`, and a single reader owns the resulting stream:

```scala
val relay = Relay[Text]()
relay.put(t"first")
relay.put(t"second")
relay.stop()
relay.stream
```

A relay's refill blocks for the first record and then drains whatever else has already arrived
into the same window, so records batch across the thread boundary instead of paying a hand-off
each. For byte and character traffic between two threads, a `Conduit` is the strictly
single-producer, single-consumer, block-structured counterpart: data crosses in blocks through a
bounded queue, so a writer that outpaces its reader parks — cross-thread backpressure — and the
free capacity is exactly the credit the reader reports as demand.

### Combining and replicating streams

`Confluence` merges several streams into one, in arrival order, running one pump per input; a
slow consumer backpressures every input, and the merged stream ends when all of them have. Both
draw on concurrency, so they run inside a supervised scope:

```scala
import threading.platformThreading

supervise(Confluence(first, second, third))
```

`Divergence` is the opposite: one source is delivered to several subscribers, each chunk
materialized once and shared immutably between them. A full subscriber queue parks the pump, so
the slowest subscriber gates the source — the correct behaviour for replication.

### Compression

A byte stream compresses and decompresses with a named scheme, as a stage in a pipeline or over
a whole value; see [compression](compression.md) for the formats available:

```scala
Data(1, 2, 3, 5, 8).compress[Gzip].decompress[Gzip]   // the original bytes
file.stream.decompress[Gzip].read[Text]
```
