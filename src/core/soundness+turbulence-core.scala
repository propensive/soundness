package soundness

export turbulence.{Aggregable, Appendable, Compression, CompressionAlgorithm, Err, Funnel, Gzip, In, Io,
    LazyListInputStream, LazyListOutputStream, Line, LineSeparation, Multiplexer, Out, Pistol, Pulsar, Readable,
    SimpleAppendable, SimpleWritable, Stdio, StreamError, Tap, Writable, Zlib, stream, readAs, writeTo, appendTo,
    by, deduplicate, rate, multiplexWith, regulate, cluster, parallelMap, multiplex, multiplexer, defer, pulsar,
    gzip, gunzip, skip, compress, decompress, shred, chunked, take, funnel}

package stdioSources:
  package virtualMachine:
    export turbulence.stdioSources.virtualMachine.{ansi, textOnly}

package lineSeparation:
  export turbulence.lineSeparation.{carriageReturn, strictCarriageReturn, linefeed, strictLinefeeds,
      carriageReturnLinefeed, adaptiveLinefeed, virtualMachine}
