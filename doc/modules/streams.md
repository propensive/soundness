## Streams

### About

Some data arrives or departs over time: a file too large to hold in memory, a network
connection, the output of a process. It is handled as a stream. One set of operations moves
data between sources and sinks:

```tabular
column  Operation
column  Does
row  `read`  pulls a source in, as a chosen type
row  `writeTo`  sends a value to a destination
row  `stream`  exposes a value as a stream of pieces
```

What can be read, and what can be written, is decided by [typeclasses][typeclass]. A type
which describes how it becomes a stream can be read. A type which describes how it consumes
one can be written to. A new source or sink therefore joins the same `read` and `writeTo` as
everything else, with no plumbing of its own. One verb for each operation, ranging over every
type which supports it, is the [small APIs](../philosophy/small-apis.md) discipline.

Underneath those operations is a streaming *kernel*, whose central type is a `Stream`. It is
not a lazy sequence of chunks. It is an endpoint which is pulled from, over a buffer whose
readable window is handed to exactly one consumer, without copying.

### On streaming

The JVM's streaming is a pair of mutable objects, an `InputStream` and an `OutputStream`. They
are read and written by side effect, one imperative call at a time, and the reader is
responsible for buffering, decoding and closing. Nothing in a method's type says whether it
streams, what kind of element it streams, or how the bytes become text.

The obvious functional answer is a lazy list of chunks. That fixes the typing, and loses the
performance. Every chunk is an allocation, and every stage copies. The memoization which makes
a lazy list replayable is exactly what keeps a large source alive in memory when nothing
wanted it kept.

Here a third route is taken. A `Stream` is a *pull endpoint*. A consumer calls `refill` with
the amount it is prepared to take, and gets back a window into a buffer, which it reads and
then skips past. Nothing is copied, and nothing is retained.

Two properties follow from that design:

- **Backpressure needs no mechanism.** A consumer which does not pull demands nothing, so
  backpressure is simply the absence of a call.
- **Stages compose without queues.** They run as nested calls on the consumer's thread, with
  no synchronization, until a stage explicitly crosses to another thread.

The design has one requirement. A window belongs to one consumer, so a stream must not be read
twice, or shared. This is checked, and not merely documented. A stream is an exclusive
[capability], and the streaming libraries are compiled with
[capture and separation checking](../philosophy/capture-checking.md) enabled. Aliasing a
stream, or letting one escape the scope which owns its source, is a compile error. Where
replay really is wanted, `memoize` drains the stream once into an [immutable] value, which may
then be shared freely. That is the explicit, bounded replacement for a lazy list's implicit
caching.

Everything comes from the `soundness` package. An encoding and a decoding of characters are in
scope, for the boundary between text and bytes, and the last two imports let the samples print
their results:

```scala
import soundness.*
import codepages.utf8Codepage
import charsets.utf8Charset
import strategies.throwUnsafely
import pathInterfaces.pathOnLinux
import filesystemOptions.overwritePreexisting
import filesystemOptions.createNonexistentParents
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
```

### Reading a source

`read` pulls a source in, as the type named. A source of bytes reads equally as `Text`, as raw
`Data`, or as a stream of either, and the [encoding](characters.md) in scope bridges bytes and
text:

```scala
val source = t"The quick brown fox"
```

```scala fuel
run
##
Out.println(source.read[Text])
Out.println(source.read[Data].length.show)
```

The same `read` works on any source with a `Readable` instance, such as a
[file](filesystem.md#reading-and-writing), a [network socket](sockets.md) or the output of a
[process](processes.md). Reading a [URL](http-client.md) as text, and reading a file as bytes,
are the one operation.

### Writing to a sink

`writeTo` sends a value to a destination which knows how to consume it. The value may be text,
bytes, or a stream of either, and the destination's `Writable` instance receives it:

```scala fuel
typecheck
##
val destination = t"/tmp/streams-tutorial.txt".as[Path on Linux]

source.writeTo(destination)
```

### Making a value readable or writable

A type becomes a source by describing how it turns into a stream, and a sink by describing how
it consumes one. Each is a typeclass with a single method:

```scala
case class Row(name: Text, count: Int)

given rowStreamable: (Row is Streamable by Text over Credit) =
  row => Stream(t"${row.name},${row.count}")
```

With that instance in scope, a `Row` can be `read`, sent with `writeTo`, or exposed with
`stream`, without any further definitions:

```scala fuel
run
##
Out.println(Row(t"apples", 3).read[Text])
```

### Streaming a value

`stream` turns a value into a pull endpoint. `source` does the same through a `Streamable`
instance, and names the type of element:

```scala
val bytes = t"The quick brown fox".in[Data].stream
val text = Row(t"apples", 3).source[Text]
```

The first is a `Stream[Data]`, and the second a `Stream[Text]`.

### Composing a pipeline

A stage transforms a stream into a stream of a different type. `via` attaches one on the pull
side. The whole chain runs on the consumer's thread, as nested refills, with no threads, no
queues and no intermediate collections. Each stage takes what the last one produced, which is
[composability](../philosophy/composability.md) in its most literal form:

```scala fuel
typecheck
##
import textSanitizers.skipSanitizer

val compressed = t"The quick brown fox".in[Data].compress[Gzip]
val decompressor = summon[Gzip is Compression].decompressor()
val decoder = summon[Charset]

compressed.stream.viaDuct(decompressor).via(decoder)
```

A `Duct` is the kernel's type for a transformation, and is what the `decompressor()` of a
[compression format](compression.md) returns. It is attached with `viaDuct`. Any other value
which describes a transformation, and has a `Ductile` instance, such as a character encoding
or a [base encoding](base-encoding.md#encoding-a-stream), is attached with `via`.

A pipeline ends in a *terminal* operation, which drains the endpoint and closes it:

```scala fuel
typecheck
##
var count = 0

compressed.stream.memoize
compressed.stream.drain: region =>
  window => count += (window: Interval).size
```

`memoize` drains the stream into one immutable value. `drain` hands each raw window to a
function, and `gather` is its accumulating counterpart. Both expose the raw window, not boxed
elements, so a reduction over bytes runs over the array with no cost for each element.

`truncate` and `discard` bound a stream without draining it, and release the rest of the
upstream unread. They count bytes or records, not the elements of a collection, which is why
they do not borrow the names `take` and `drop`.

Where a pipeline ends in a chain which is *pushed* to, instead of in a value, `pump` is the
single point at which data crosses from the pull side to the push side:

```scala fuel
typecheck
##
def forward(stream: Stream[Data] over Credit, intake: Intake[Data] over Credit): Unit =
  stream.pump(intake)
```

In summary:

```tabular
column  Operation
column  Drains the stream
column  Gives
row  `memoize`  yes  one immutable value
row  `drain`  yes  each raw window, to a function
row  `gather`  yes  an accumulator threaded through each raw window
row  `pump`  yes  every window, pushed into an `Intake`
row  `truncate`  no  the first records or bytes, releasing the remainder unread
row  `discard`  no  everything after the first records or bytes
```

### Parsing with a cursor

A parser does not consume a stream in bulk. It takes one element at a time, and looks at the
next element before deciding what to do with it. A `Cursor` is that view: a position over a
sequence of `Text` or `Data` blocks, built from an iterator, a `Chain` or a `Stream`. It
refills from its source as the position advances, so the parser sees one continuous run of
characters or bytes, however the source was divided into chunks:

```tabular
column  Method
column  Does
row  `peek`  shows the current element without consuming it, as a `Datum`
row  `advance()`  moves past the current element
row  `next()`  moves past it, and reports whether anything remains
row  `position`, `line`, `column`  say where the cursor has reached
```

A `Datum` compares directly with a `Char`, and is `Datum.End` once the source is exhausted:

```scala
val cursor = Cursor(Iterator(t"hello", t"world"))
```

```scala fuel
run
##
Out.println((cursor.peek == 'h').inspect)
cursor.advance()
Out.println(cursor.position.inspect)
```

A `mark` records a point to return to, or to copy from. `grab` gives everything between two
marks as one value, even where it straddles the blocks which the source was supplied in. A
mark may be taken only within `hold`, which keeps the marked region in the buffer until the
block ends. Outside `hold`, the cursor is free to discard what it has already passed:

```scala fuel
run
##
val grabbed = cursor.hold:
  val start = cursor.mark
  for i <- 1 to 5 do cursor.next()
  cursor.grab(start, cursor.mark)

Out.println(grabbed)
```

The grabbed text runs from the end of the first block into the second. The parsers for
[JSON](json.md), [XML](xml.md) and the other formats are built on cursors, which is how they
read a document from a stream without holding it whole.

### Records

Between raw windows and whole collections sits streaming by the record: rows, events, frames,
messages. A `Records[record]` is a stream whose windows are chunks of records, so its credit
counts records, not bytes. `delineate` splits a source of characters into lines, and `records`
iterates the parsed records of such a stream:

```scala
import lineSeparation.adaptiveLinefeedLineSeparation

val numbers = t"1\n2\n3".source[Text].delineate.records.map(_.as[Int]).to[List]
```

```scala fuel
run
##
Out.println(numbers.inspect)
```

Records are immutable values, so, unlike windows, they cross between stages and between
threads by reference. [CSV](csv.md#reading-rows) reads rows in this way, and
[JSON](json.md#ndjson) reads newline-delimited documents.

### Standard streams

Standard output and standard error are reached through `Out` and `Err`, which print through
the `Stdio` capability in scope. Standard output is a capability, not a global, so a test or a
[daemon](daemons.md) can redirect it:

```scala fuel
run
##
Out.println(t"started")
Err.println(t"a warning")
```

### Producing a stream over time

Sometimes the elements of a stream are produced by one part of a program, and consumed by
another. A `Relay` bridges the two. Any number of producers `put` records into it as they
arise, one of them eventually calls `stop`, and a single reader owns the resulting stream:

```scala
val relay = Relay[Text]()
relay.put(t"first")
relay.put(t"second")
relay.stop()
val relayed = relay.stream
```

A relay's refill blocks for the first record. It then drains whatever else has already arrived
into the same window, so records are batched across the boundary between threads, and do not
each pay for a hand-off.

For traffic in bytes or characters between two threads, a `Conduit` is the counterpart which
has strictly one producer and one consumer, and is structured in blocks. Data crosses in
blocks, through a bounded queue. A writer which outpaces its reader therefore parks, which is
backpressure across threads, and the free capacity is exactly the credit which the reader
reports as demand.

### Combining and replicating streams

`Confluence` merges several streams into one, in order of arrival, with one pump running for
each input. A slow consumer applies backpressure to every input, and the merged stream ends
when all of them have. It draws on [concurrency](concurrency.md), so it runs inside a
supervised scope:

```scala
import threads.platformThreads
import probates.cancelProbate

val first = Stream(t"one")
val second = Stream(t"two")
val third = Stream(t"three")

val merged = supervise(Confluence(first, second, third).memoize)
```

`Divergence` is the opposite. One source is delivered to several subscribers, and each chunk
is materialized once, and shared immutably between them. A subscriber whose queue is full
parks the pump, so the slowest subscriber sets the pace of the source, which is the correct
behavior for replication.

### Tuning backpressure

Every stage which allocates a buffer consults the `Buffering` policy in scope, so one given
tunes a whole pipeline. Its members are:

```tabular
column  Member
column  Meaning
column  Default
row  `capacity(substrate)`  the staging block size  4096 bytes, 2048 characters or 256 records
row  `transfer(substrate)`  the size of a block crossing an asynchronous boundary  sixteen staging blocks
row  `depth`  the transfer blocks of headroom a `Conduit` holds between writer and reader  16
row  `recycle`  whether drained transfer blocks return to the writer for reuse  on
```

The sizes are chosen for two reasons. Staging blocks want to stay in the processor's cache.
And every transfer block costs a synchronized hand-off, so the blocks which cross a boundary
want to be much larger than staging blocks.

The data in flight in a conduit is bounded by `(depth + 1)` transfer blocks. The throughput of
hand-offs collapses at very shallow depths: a depth of 2 reaches less than a tenth of the
default's throughput in the benchmarks. It levels off around the default, for a producer and
consumer in step with each other. A pipeline which works in bursts still gains from a depth
which covers the size of its bursts, in transfer blocks. Recycled blocks go to a shared pool
of capped size, which spreads the cost of allocating blocks across conduits.

Backpressure applies wherever data crosses between threads:

```tabular
column  Structure
column  Carries
column  Under backpressure
row  `Conduit`  bytes or characters, from one writer to one reader  the writer parks on a full ring
row  `Divergence`  one source, to several subscribers  the pump parks on the slowest subscriber's ring, until that stream is closed
row  `Confluence`  several streams, merged into one  the pumps park uniformly on the shared queue
row  HTTP/2  a body, in both directions  the peer waits for credit: the receive window, replenished only as the body is drained
row  `Relay`  records, from many producers to one reader  producers never block
```

The receive window which HTTP/2 advertises is 1 MiB by default, and can be tuned for each
connection. It bounds how far a peer can run ahead of a consumer which has stopped reading.
[HTTP server](http-server.md#http2) describes it.

One structure deliberately opts out. `Relay` is the bus for records from many producers, and
its contract is that producers never block. HTTP/2's multiplexer for outbound frames depends
on that, since a blocking `put` there would risk a deadlock between streams. A relay's
discipline is therefore its single consumer, and not a bound.

### Compression

A stream of bytes is compressed and decompressed with a named scheme, as a stage of a pipeline
or over a whole value. [Compression](compression.md) describes the formats:

```scala fuel
run
##
Out.println(t"The quick brown fox".in[Data].compress[Gzip].decompress[Gzip].utf8)
```
