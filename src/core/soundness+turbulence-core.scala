/*
    Turbulence, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

export turbulence.{Aggregable, Compression, CompressionAlgorithm, Eof, Err, Spool, Gzip, In, Io,
    Line, LineSeparation, Multiplexer, Out, Pistol, Pulsar, Readable,
    SimpleWritable, Stdio, StreamError, Tap, Writable, Zlib, stream, read, writeTo,
    deduplicate, rate, multiplexWith, regulate, cluster, parallelMap, multiplex, multiplexer, defer,
    pulsar, gzip, gunzip, discard, compress, decompress, shred, chunked, take, spool, strict,
    Conduit, inputStream, outputStream}

package stdioSources:
  export turbulence.stdioSources.mute
  package virtualMachine:
    export turbulence.stdioSources.virtualMachine.{ansi, textOnly}

package lineSeparation:
  export turbulence.lineSeparation.{carriageReturn, strictCarriageReturn, linefeed, strictLinefeeds,
      carriageReturnLinefeed, adaptiveLinefeed, virtualMachine}
