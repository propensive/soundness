                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package gesticulate

import java.io.{InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.US_ASCII

import org.apache.commons.fileupload2.core.MultipartInput
import org.eclipse.jetty.http.MultiPart
import org.eclipse.jetty.io.Content

// The rival multipart decoders, written as their own users would write them. Each is fed the
// same wire bytes, pre-split into the same blocks as the `Chain[Data]` that `Multipart.parse`
// reads, and returns the total number of body bytes it decoded, so that `Benchmarks.run()` can
// check every arm agrees before anything is timed.
//
// This file is the rivals' world: stdlib arrays and Java I/O. Nothing of Soundness is used here
// except the package clause.
object Rivals:

  // Jetty 12's push parser: each block is handed over as a `Content.Chunk`, and the listener is
  // called back with slices of it as the part boundaries are found.
  object Jetty:
    private final class Counter extends MultiPart.Parser.Listener:
      var total: Long = 0L
      var complete: Boolean = false

      override def onPartContent(chunk: Content.Chunk): Unit = total += chunk.remaining
      override def onComplete(): Unit = complete = true
      override def onFailure(failure: Throwable): Unit = throw failure

    def parse(boundary: String, blocks: Array[Array[Byte]]): Long =
      val counter = Counter()
      val parser = MultiPart.Parser(boundary, counter)
      var index = 0

      while index < blocks.length do
        val last = index == blocks.length - 1
        parser.parse(Content.Chunk.from(ByteBuffer.wrap(blocks(index)), last))
        index += 1

      if !counter.complete then throw IllegalStateException("Jetty did not complete the multipart")
      counter.total

  // commons-fileupload's pull parser, reading from an `InputStream` which yields one block per
  // `read` call, as a socket would. Bodies are drained to a null sink: `readBodyData` still copies
  // each through the parser's buffer.
  object FileUpload:
    private final class Blocks(blocks: Array[Array[Byte]]) extends InputStream:
      private var index = 0
      private var offset = 0

      override def read(): Int =
        val buffer = new Array[Byte](1)
        if read(buffer, 0, 1) == -1 then -1 else buffer(0) & 0xff

      override def read(buffer: Array[Byte], start: Int, length: Int): Int =
        while index < blocks.length && offset == blocks(index).length do
          index += 1
          offset = 0

        if index == blocks.length then -1 else
          val count = math.min(length, blocks(index).length - offset)
          System.arraycopy(blocks(index), offset, buffer, start, count)
          offset += count
          count

    def parse(boundary: String, blocks: Array[Array[Byte]]): Long =
      val input =
        MultipartInput.builder()
          .setInputStream(Blocks(blocks))
          .setBoundary(boundary.getBytes(US_ASCII))
          .get()

      val sink = OutputStream.nullOutputStream()
      var total = 0L
      var more = input.skipPreamble()

      while more do
        input.readHeaders()
        total += input.readBodyData(sink)
        more = input.readBoundary()

      total
