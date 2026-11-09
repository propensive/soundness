## Compression

### About

A handful of formats carry most of the world's compressed bytes:
[DEFLATE](https://en.wikipedia.org/wiki/Deflate) and its wrappers `gzip` and `zlib`,
[Brotli](https://en.wikipedia.org/wiki/Brotli), [XZ](https://en.wikipedia.org/wiki/XZ_Utils)
and raw LZMA2, and [LZW](https://en.wikipedia.org/wiki/Lempel–Ziv–Welch). All of them compress
and decompress data through one pair of methods. The format is a type, and `compress` and
`decompress` serve every format, over a whole value or as a stage of a
[stream](streams.md).

Every format is implemented in pure Scala, so it works on every platform. On the JVM, DEFLATE
and its wrappers are also served by the platform's native zlib. That choice is made when the
code is built, not at each use, so the JVM pays nothing for the portability.

### On compression

A compression library usually presents a codec which must be constructed, fed and finished: an
object with a buffer, a `setInput`, a `deflate` and a `finish`, and a loop around them which
is easy to get wrong. The format is a string or an integer, the framing is a flag, and whether
a given call has produced everything it will produce is a question the caller has to answer.

Here the format is a type, such as `Gzip`, `Brotli` or `Xz`, and the operation is a method on
the data. The same instance describes both the whole-value form and the streaming form, so the
two agree byte for byte, and a value compressed in one way decompresses in the other. A
compression scheme is a stage of a pipeline, not a wrapper around a stream, which is
[composability](../philosophy/composability.md) in its most literal form.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*

import charsets.utf8Charset
import textSanitizers.skipSanitizer
import codepages.utf8Codepage
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Compressing a value

A block of bytes is compressed, and decompressed, by naming the format:

```scala
val text = t"the quick brown fox jumps over the lazy dog. "
val payload = (text + text + text + text).in[Data]

val compressed = payload.compress[Gzip]
```

```scala fuel
run
##
Out.println(payload.length.show)
Out.println(compressed.length.show)
Out.println((compressed.decompress[Gzip] == payload).inspect)
```

### Compressing a stream

The same names attach to a stream, where they become stages of a pipeline. The data is
transformed as it flows, and nothing larger than the pipeline's buffers is held.

Writing a compressed [file](filesystem.md), and reading it back, is a matter of placing the
compressor and the decompressor on either side of the disk. `pump` delivers the compressed
stream into the file's intake:

```scala fuel
typecheck
##
import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val archive = temporaryDirectory[Path on Linux] / "compression" / Uuid().show / "fox.txt.gz"

archive.create[File](CreateFlag.Parents): handle ?=>
  payload.stream.compress[Gzip].pump(handle.intake())

val restored: Text = archive.open[File]()(file.stream.decompress[Gzip].read[Text])
```

The whole-value and streaming forms interoperate freely. A value which was compressed as a
whole decompresses through a stream, and the output of a stream decompresses as a value.

### The formats

Seven formats are provided, in four families:

```tabular
column  Format
column  Algorithm
column  Framing
column  Carried by
row  `Deflate`  DEFLATE  none  ZIP entries
row  `Zlib`  DEFLATE  a two-byte header and a checksum  PNG chunks, HTTP `deflate` encoding
row  `Gzip`  DEFLATE  a header, timestamp and CRC  `*.gz` files, HTTP content encoding
row  `Brotli`  Brotli  the Brotli stream format  HTTP content encoding
row  `Xz`  LZMA2  the `*.xz` container, with a CRC-64 check  `*.xz` files
row  `Lzma2`  LZMA2  none
row  `Lzw`  LZW  none  TIFF and PDF streams
```

#### DEFLATE

`Deflate` is the raw algorithm. `Zlib` adds a two-byte header and a checksum. `Gzip` adds the
header, timestamp and CRC which `*.gz` files carry. These are the formats of HTTP content
encoding, of ZIP entries and of PNG chunks, and they are the fastest of the set.

#### Brotli

`Brotli` compresses to a smaller size than DEFLATE, at a comparable speed, and is the content
encoding of the modern web. Both directions need the whole value before they produce any
output. The decoder needs it because backward references may reach across the entire window,
and the encoder because it chooses its framing from the total length. A Brotli stage therefore
buffers, where a DEFLATE stage does not.

Brotli can also encode a value *relative to* another which the receiver already holds. A
`continuation` is the tail of a Brotli stream whose window has been loaded with a base
beforehand. It carries only the new value's blocks, and no stream header, and its backward
references reach into the base. A successor which differs from its predecessor by a few edits
therefore costs a few bytes:

```scala
val base: Data = t"the quick brown fox jumps over the lazy dog".in[Data]
val next: Data = t"the quick brown fox jumped over the lazy dog".in[Data]
val continuation = Brotli.continuation(base, next)
```

The receiver rebuilds the window from its own copy of the base, with `prefix`, and decodes the
prefix and the continuation as one stream. What comes out is the base, followed by the
successor:

```scala
val decoded = Chain(Brotli.prefix(base), continuation).decompress[Brotli].read[Data]
```

```scala fuel
run
##
Out.println(next.length.show)
Out.println(continuation.length.show)
Out.println((decoded.length == base.length + next.length).inspect)
```

The bytes of a `prefix` are fixed by RFC 7932, together with the window and block parameters.
Both methods take the window size as WBITS, from 10 to 24, and `prefix` takes the maximum
length of an uncompressed block. Their defaults are `Brotli.Window` and `Brotli.Block`, the
largest which the format allows. Both sides must use the same values.

#### XZ and LZMA2

`Xz` is the codec with the highest ratio. It is LZMA2 inside the `*.xz` container, with a
CRC-64 check, and matches what the `xz` command-line tool produces. `Lzma2` is the same codec
without the container's framing, and stands to `Xz` as `Deflate` stands to `Gzip`.

Both default to preset 6. Presets 0 to 3 favor speed, and 4 to 9 favor ratio. An explicit
preset is selected with `Xz.compress(stream, preset)` or `Xz.compressor(preset)`.

#### LZW

`Lzw` is the compression of TIFF and PDF streams. The JDK offers no implementation of it at
all, so this one is written from the specification. Its `earlyChange` parameter makes both
sides widen their codes one table entry sooner. It is on by default, which is what TIFF, PDF
and every known encoder produce. The parameterized `Lzw.compressor` and `Lzw.decompressor`
serve formats which state it explicitly.

### Where compression appears

Compression is rarely used alone:

- [Archives](archives.md) store their entries compressed, and tar archives travel compressed.
- [HTTP](http-client.md) bodies are encoded with it on the wire.
- [Container](docker.md) image layers are compressed, and addressed by their
  [digest](hashing.md#hashing-a-stream).
- [PDF](pdf.md) content streams are stored compressed.

In each case the format is one of the types named here, so a layer, an entry or a body
decompresses with the operation described above.
