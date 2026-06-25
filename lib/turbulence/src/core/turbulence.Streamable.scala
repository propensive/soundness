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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package turbulence

import java.io as ji
import java.nio as jn

import anticipation.*
import contingency.*
import hieroglyph.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

object Streamable:
  given bytes: Data is Streamable by Data = Stream(_)
  given text: [textual <: Text] => textual is Streamable by Text = Stream(_)

  // The encoder-bridged counterpart of `Aggregable`'s `bytesText` (`Text is Aggregable by Data`),
  // so a `Text` can be streamed as `Data` directly — e.g. written to a file with `path.write`.
  given textData: (encoder: CharEncoder) => Text is Streamable by Data =
    text => encoder.encoded(Stream(text))

  given stream: [element] => Stream[element] is Streamable by element = identity(_)

  given inCharReader: (stdio: Stdio) => In.type is Streamable by Char = in =>
    def recur(count: Bytes): Stream[Char] =
      stdio.reader.read() match
        case -1  => Stream()
        case int => int.toChar #:: recur(count + 1.b)

    Stream.defer(recur(0L.b))

  given inByteReader: (stdio: Stdio) => In.type is Streamable by Byte = in =>
    def recur(count: Bytes): Stream[Byte] =
      stdio.in.read() match
        case -1  => Stream()
        case int => int.toByte #:: recur(count + 1.b)

    Stream.defer(recur(0L.b))

  given reader: [input <: ji.Reader] => (Tactic[StreamError]^) => input is Streamable by Char =
    reader =>
      def recur(count: Bytes): Stream[Char] =
        try reader.read() match
          case -1  => Stream()
          case int => int.toChar #:: recur(count + 1.b)
        catch case error: ji.IOException =>
          reader.close()
          abort(StreamError(count))

      Stream.defer(recur(0L.b))


  given bufferedReader: [input <: ji.BufferedReader] => (Tactic[StreamError]^)
  =>  input is Streamable by Line =

    reader =>
      def recur(count: Bytes): Stream[Line] =
        try reader.readLine() match
          case null         => Stream()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case error: ji.IOException =>
          reader.close()
          abort(StreamError(count))

      Stream.defer(recur(0L.b))


  given inputStream: [input <: ji.InputStream] => (Tactic[StreamError]^)
  =>  input is Streamable by Data =

    channel.contramap(jn.channels.Channels.newChannel(_).nn)


  given channel: (Tactic[StreamError]^) => jn.channels.ReadableByteChannel is Streamable by Data =
    channel =>
      val buffer: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](1024)).nn

      def recur(total: Long): Stream[Data] =
        try channel.read(buffer) match
          case -1 => Stream().also(try channel.close() catch case error: Exception => ())
          case 0  => recur(total)

          case count =>
            buffer.flip()
            val size: Int = count.min(1024)
            val array: Array[Byte] = new Array[Byte](size)
            buffer.get(array)
            buffer.clear()

            array.immutable(using Unsafe) #:: recur(total + count)

        catch case e: Exception => abort(StreamError(total.b))

      Stream.defer(recur(0))

trait Streamable extends Typeclass, Operable:
  def stream(value: Self): Stream[Operand]

  def contramap[self2](lambda: self2 => Self): self2 is Streamable by Operand =
    source => stream(lambda(source))
