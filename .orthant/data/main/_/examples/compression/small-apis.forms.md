# One name for a value, a chain and a stream

Whether the data is held whole, arrives as a chain of chunks or flows through a stream,
compressing it is the same method with the same type argument. Only the result differs: it has
the shape of what was compressed.

```scala fuel
compile
alternative A value
  highlight compress[Gzip]  bytes in, bytes out
  code
      import codepages.utf8Codepage

      val sonnet = t"Shall I compare thee to a summer's day?".in[Data]

      val packed: Data = sonnet.compress[Gzip]
alternative A chain of chunks
  highlight compress[Gzip]  each chunk, as it is demanded
  code
      import codepages.utf8Codepage

      val sonnet = Chain(t"Shall I compare thee ".in[Data], t"to a summer's day?".in[Data])

      val packed: Chain[Data] = sonnet.compress[Gzip]
alternative A stream
  highlight compress[Gzip]  a stage of a pipeline
  code
      import codepages.utf8Codepage

      val sonnet = t"Shall I compare thee to a summer's day?".in[Data]

      def packed = sonnet.stream.compress[Gzip]
##
```

[Compressing a stream](../../soundness/modules/compression.md#compressing-a-stream) says how
the forms interoperate.
