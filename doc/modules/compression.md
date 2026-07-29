## Compression

### About

The formats that carry the world's bytes — [DEFLATE](https://en.wikipedia.org/wiki/Deflate) and
its `gzip` and `zlib` wrappers, [Brotli](https://en.wikipedia.org/wiki/Brotli),
[XZ](https://en.wikipedia.org/wiki/XZ_Utils) and raw LZMA2, and
[LZW](https://en.wikipedia.org/wiki/Lempel–Ziv–Welch) — compress and decompress data through one
pair of methods. The format is a type, and `compress` and `decompress` serve every one of them,
over a whole value or as a stage in a [streaming](streams.md) pipeline.

Every format is implemented in pure Scala, so it works on every platform Soundness targets. On
the JVM, DEFLATE and its wrappers are additionally served by the platform's native zlib, chosen
when the code is built rather than at each call site, so the JVM pays nothing for the
portability.

### On compression

Compression libraries usually present as codecs to be constructed, fed and finished: an object
with a buffer, a `setInput`, a `deflate`, a `finish`, and an easily-mistaken loop around them.
The format is a string or an integer, the framing is a flag, and whether a given call has
produced everything it will produce is a question the caller has to answer.

Here the format is a type — `Gzip`, `Brotli`, `Xz` — and the operation is a method on the data.
Because the same instance describes both the whole-value and the streaming form, the two agree
byte for byte, and a value compressed one way decompresses the other. Everything comes from the
`soundness` package:

```scala
import soundness.*
```

### Compressing a value

A block of bytes compresses and decompresses by naming the format:

```scala
val payload = t"the quick brown fox".in[Data]

val compressed = payload.compress[Gzip]
compressed.decompress[Gzip]   // the original bytes
```

### Compressing a stream

The same names attach to a stream, where they become pipeline stages: the data is transformed as
it flows, and nothing larger than the pipeline's buffers is held:

```scala
file.stream.compress[Gzip].writeTo(destination)
archive.stream.decompress[Gzip].read[Text]
```

Whole-value and streaming forms interoperate freely: a value compressed as a whole decompresses
through a stream, and a stream's output decompresses as a value.

### The formats

`Deflate` is the raw algorithm; `Zlib` adds its two-byte header and checksum; `Gzip` adds the
header, timestamp and CRC that `.gz` files carry. These are the formats of HTTP content
encoding, ZIP entries and PNG chunks, and they are the fastest of the set.

`Brotli` compresses smaller than DEFLATE at comparable speed, and is the modern web's content
encoding. Both directions need the whole value before producing output — the decoder because
backward references may reach across the entire window, the encoder because it chooses its
framing from the total length — so a Brotli stage buffers where a DEFLATE stage does not.

`Xz` is the high-ratio codec: LZMA2 inside the `.xz` container, with a CRC-64 check, matching
what the `xz` command-line tool produces. `Lzma2` is the same codec without the container
framing, standing to `Xz` as `Deflate` stands to `Gzip`. Both default to preset 6; presets 0 to
3 favour speed and 4 to 9 favour ratio, and an explicit preset is selected with
`Xz.compress(stream, preset)` or `Xz.compressor(preset)`.

`Lzw` is the compression of TIFF and PDF streams. The JDK offers no implementation of it at all,
so this one is written from the specification. Its `earlyChange` parameter — both sides widening their
codes one table entry sooner — is on by default, which is what TIFF, PDF and every known encoder
produce; the parameterized `Lzw.compressor` and `Lzw.decompressor` serve formats that state it
explicitly.

### Where compression appears

Compression is rarely used alone. It is how [archives](archives.md) store their entries and how
tar archives travel; how [HTTP](http-client.md) bodies are encoded on the wire; how
[container](docker.md) image layers are addressed; and how [PDF](pdf.md) content streams are
stored. In each case the format is the same type named here, so a layer, an entry or a body
decompresses with the operation described above.
