/*
    Turbulence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
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

object Readable:
  given bytes: Bytes is Readable by Bytes = Stream(_)
  given text: [TextType <: Text] => TextType is Readable by Text = Stream(_)

  given encodingAdapter: [SourceType: Readable by Text] => (encoder: CharEncoder)
  =>    SourceType is Readable by Bytes =
    source => encoder.encode(SourceType.stream(source))

  given decodingAdapter: [SourceType: Readable by Bytes] => (decoder: CharDecoder)
  =>    SourceType is Readable by Text =

    source => decoder.decode(SourceType.stream(source))

  given stream: [ElementType] => Stream[ElementType] is Readable by ElementType = identity(_)

  given inCharReader: (stdio: Stdio) => In.type is Readable by Char = in =>
    def recur(count: Memory): Stream[Char] =
      stdio.reader.read() match
        case -1  => Stream()
        case int => int.toChar #:: recur(count + 1.b)

    Stream.defer(recur(0L.b))

  given inByteReader: (stdio: Stdio) => In.type is Readable by Byte = in =>
    def recur(count: Memory): Stream[Byte] =
      stdio.in.read() match
        case -1  => Stream()
        case int => int.toByte #:: recur(count + 1.b)

    Stream.defer(recur(0L.b))

  given reader: [InType <: ji.Reader] => Tactic[StreamError]
  =>    InType is Readable by Char = reader =>
    def recur(count: Memory): Stream[Char] =
      try reader.read() match
        case -1  => Stream()
        case int => int.toChar #:: recur(count + 1.b)
      catch case err: ji.IOException =>
        reader.close()
        raise(StreamError(count), Stream())

    Stream.defer(recur(0L.b))

  given bufferedReader: [InType <: ji.BufferedReader]
  =>    Tactic[StreamError]
  =>    InType is Readable by Line =
    reader =>
      def recur(count: Memory): Stream[Line] =
        try reader.readLine() match
          case null         => Stream()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException =>
          reader.close()
          raise(StreamError(count), Stream())

      Stream.defer(recur(0L.b))

  given inputStream: [InType <: ji.InputStream]
  =>    Tactic[StreamError]
  =>    InType is Readable by Bytes =
    channel.contramap(jn.channels.Channels.newChannel(_).nn)

  given channel: Tactic[StreamError] => jn.channels.ReadableByteChannel is Readable by Bytes =
    channel =>
      val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](1024)).nn

      def recur(total: Long): Stream[Bytes] =
        try channel.read(buf) match
          case -1 => Stream().also(try channel.close() catch case err: Exception => ())
          case 0  => recur(total)

          case count =>
            buf.flip()
            val size: Int = count.min(1024)
            val array: Array[Byte] = new Array[Byte](size)
            buf.get(array)
            buf.clear()

            array.immutable(using Unsafe) #:: recur(total + count)

        catch case e: Exception => Stream(raise(StreamError(total.b), Bytes()))

      Stream.defer(recur(0))

trait Readable:
  type Self
  type Operand
  def stream(value: Self): Stream[Operand]

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Readable by Operand =
    source => stream(lambda(source))
