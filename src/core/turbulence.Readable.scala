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

package turbulence

import java.io as ji
import java.nio as jn

import anticipation.*
import contingency.*
import hieroglyph.*
import rudiments.*
import symbolism.*
import prepositional.*
import vacuous.*

object Readable:
  given Bytes is Readable by Bytes as bytes = LazyList(_)
  given [TextType <: Text] => TextType is Readable by Text as text = LazyList(_)

  given [SourceType: Readable by Text](using encoder: CharEncoder)
      => SourceType is Readable by Bytes as encodingAdapter =
    source => encoder.encode(SourceType.stream(source))

  given [SourceType: Readable by Bytes](using decoder: CharDecoder)
      => SourceType is Readable by Text as decodingAdapter =

    source => decoder.decode(SourceType.stream(source))

  given [ElementType] => LazyList[ElementType] is Readable by ElementType as lazyList = identity(_)

  given (using stdio: Stdio) => In.type is Readable by Char as inCharReader = in =>
    def recur(count: Memory): LazyList[Char] =
      stdio.reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)

    LazyList.defer(recur(0L.b))

  given (using stdio: Stdio) => In.type is Readable by Byte as inByteReader = in =>
    def recur(count: Memory): LazyList[Byte] =
      stdio.in.read() match
        case -1  => LazyList()
        case int => int.toByte #:: recur(count + 1.b)

    LazyList.defer(recur(0L.b))

  given [InType <: ji.Reader](using Tactic[StreamError]) => InType is Readable by Char as reader = reader =>
    def recur(count: Memory): LazyList[Char] =
      try reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)
      catch case err: ji.IOException =>
        reader.close()
        raise(StreamError(count), LazyList())

    LazyList.defer(recur(0L.b))

  given [InType <: ji.BufferedReader](using Tactic[StreamError])
      => InType is Readable by Line as bufferedReader =
    reader =>
      def recur(count: Memory): LazyList[Line] =
        try reader.readLine() match
          case null         => LazyList()
          case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException =>
          reader.close()
          raise(StreamError(count), LazyList())

      LazyList.defer(recur(0L.b))

  given [InType <: ji.InputStream](using Tactic[StreamError])
      => InType is Readable by Bytes as inputStream =
    channel.contramap(jn.channels.Channels.newChannel(_).nn)

  given (using Tactic[StreamError])
      => jn.channels.ReadableByteChannel is Readable by Bytes as channel = channel =>
    val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](1024)).nn

    def recur(total: Long): LazyList[Bytes] =
      try channel.read(buf) match
        case -1 => LazyList().also(try channel.close() catch case err: Exception => ())
        case 0  => recur(total)

        case count =>
          buf.flip()
          val size: Int = count.min(1024)
          val array: Array[Byte] = new Array[Byte](size)
          buf.get(array)
          buf.clear()

          array.immutable(using Unsafe) #:: recur(total + count)

      catch case e: Exception => LazyList(raise(StreamError(total.b), Bytes()))

    LazyList.defer(recur(0))

trait Readable:
  type Self
  type Operand
  def stream(value: Self): LazyList[Operand]

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Readable by Operand =
    source => stream(lambda(source))
