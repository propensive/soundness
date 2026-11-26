# A stream is read once

A stream hands its consumer a window onto its own buffer, with no copy, which is sound only
while there is one consumer. Every terminal operation therefore consumes the stream, and the
compiler's separation checking rejects a second use of it. Where the data is wanted twice,
`memoize` drains the stream once into an immutable value, which may be shared.

```scala fuel
compile
feature experimental.captureChecking
feature experimental.separationChecking
frame
  label One stream, drained twice
  errors show
  highlight stream.memoize
    line 6
  code
      import codepages.utf8Codepage

      def prologue(): Data =
        val stream = t"Two households, both alike in dignity".in[Data].stream
        val length = stream.memoize.length
        stream.memoize
frame
  label Drained once, and the value used twice
  highlight stream.memoize
  code
      import codepages.utf8Codepage

      def prologue(): Data =
        val stream = t"Two households, both alike in dignity".in[Data].stream
        val data = stream.memoize
        val length = data.length
        data
##
```

[On streaming](../../soundness/modules/streams.md#on-streaming) explains why a window has one
owner, and [capture checking](../../philosophy/capture-checking.md#separation-checking) how
the compiler knows.
