## Streams

### About

Data that arrives or departs over time — a file too large to hold in memory, a network
connection, the output of a process — is handled as a stream. A `Stream` is a lazy sequence,
usually of bytes or of text, and one set of polymorphic operations moves data between sources
and sinks: `read` pulls a source in as a chosen type, `writeTo` sends a value to a
destination, and `stream` exposes a value as a stream of pieces.

What can be read, and what can be written, is decided by typeclasses. A type that describes how
it becomes a stream can be read; a type that describes how it consumes one can be written to; so
a new source or sink joins the same `read` and `writeTo` as everything else, with no bespoke
plumbing.

### On streaming

The JVM's streaming is a pair of mutable objects — an `InputStream` and an `OutputStream` — that
are read and written by side effect, one imperative call at a time, with the reader responsible
for buffering, decoding, and closing. Nothing in a method's type says whether it streams, what
element it streams, or how the bytes become text.

A stream here is an immutable lazy list, so it is a value like any other: it can be mapped,
filtered and combined, and it is consumed only as far as it is needed, which is what keeps a
large source from being pulled wholly into memory. Reading and writing are polymorphic over the
element type and the participating types, so the same `read` yields text, bytes, or a stream of
either, according to the type asked for. Everything comes from the `soundness` package, with a
character encoding and decoding in scope for the text/byte boundary:

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
source.read[Stream[Text]]    // the text in chunks
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
`Spool` bridges the two: values are `put` into it as they arise, and its `stream` yields them in
order until it is stopped:

```scala
val spool = Spool[Text]()
spool.put(t"first")
spool.put(t"second")
spool.stop()
spool.stream   // Stream(t"first", t"second")
```

### Combining streams

Several streams merge into one, interleaving their elements as they arrive, with `multiplex`.
Because the merge draws from all sources concurrently, it runs inside a supervised scope:

```scala
import threading.platformThreading

val evens = Stream(2, 4, 6)
val odds = Stream(1, 3, 5)
supervise(evens.multiplex(odds).to(Set))   // Set(1, 2, 3, 4, 5, 6)
```

### Compression

A byte stream compresses and decompresses with a named scheme — [Gzip](https://en.wikipedia.org/wiki/Gzip),
`Zlib` or `Deflate` — and a single block of bytes has eager `gzip` and `gunzip`:

```scala
Stream(Data(1, 2, 3, 5, 8)).compress[Gzip].decompress[Gzip]   // the original bytes
```

### Framing

A stream of bytes carrying length-prefixed frames — a common shape in network protocols —
splits into its frames with `framed`, naming the width of the length prefix. A truncated frame
raises a `StreamError`:

```scala
Stream(Data(0, 0, 0, 3, 10, 20, 30)).framed[U32]()   // Stream(Data(10, 20, 30))
```
