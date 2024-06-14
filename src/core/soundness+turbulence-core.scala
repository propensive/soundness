/*
    Turbulence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package soundness

export turbulence.{Aggregable, Appendable, Compression, CompressionAlgorithm, Err, Funnel, Gzip, In, Io,
    LazyListInputStream, LazyListOutputStream, Line, LineSeparation, Multiplexer, Out, Pistol, Pulsar, Readable,
    SimpleAppendable, SimpleWritable, Stdio, StreamError, Tap, Writable, Zlib, stream, readAs, writeTo, appendTo,
    deduplicate, rate, multiplexWith, regulate, cluster, parallelMap, multiplex, multiplexer, defer, pulsar,
    gzip, gunzip, skip, compress, decompress, shred, chunked, take, funnel}

package stdioSources:
  package virtualMachine:
    export turbulence.stdioSources.virtualMachine.{ansi, textOnly}

package lineSeparation:
  export turbulence.lineSeparation.{carriageReturn, strictCarriageReturn, linefeed, strictLinefeeds,
      carriageReturnLinefeed, adaptiveLinefeed, virtualMachine}
