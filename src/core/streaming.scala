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

import rudiments.*
import vacuous.*
import contingency.*
import hieroglyph.*
import anticipation.*

import java.io as ji
import java.nio as jn

//import language.experimental.captureChecking

object Writable:
  given [OutType <: ji.OutputStream](using streamCut: Errant[StreamError]) => OutType is Writable by Bytes as outputStreamBytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given (using streamCut: Errant[StreamError], encoder: CharEncoder)
          => ji.OutputStream is Writable by Text as outputStreamText =

    (outputStream, stream) =>
      stream.each: text =>
        outputStream.write(encoder.encode(text).mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given [TargetType: Writable by Text](using decoder: CharDecoder)
      => TargetType is Writable by Bytes as decodingAdapter =
    (target, stream) => TargetType.write(target, decoder.decode(stream))

  given [TargetType: Writable by Bytes](using encoder: CharEncoder)
      => TargetType is Writable by Text as encodingAdapter =
    (target, stream) => TargetType.write(target, encoder.encode(stream))

@capability
trait Writable:
  type Self
  type Element
  def write(target: Self, stream: LazyList[Element]): Unit

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Writable by Element =
    (target, stream) => write(lambda(target), stream)

trait SimpleWritable[TargetType, ElementType] extends Writable:
  type Element = ElementType
  type Self = TargetType
  def write(target: Self, stream: LazyList[ElementType]): Unit = stream match
    case head #:: tail => writeElement(target, head); write(target, tail)
    case _             =>

  def writeElement(target: Self, element: ElementType): Unit

object Appendable:
  given stdoutBytes(using stdio: Stdio): SimpleAppendable[Out.type, Bytes] =
    (stderr, bytes) => stdio.write(bytes)

  given stdoutText(using stdio: Stdio): SimpleAppendable[Out.type, Text] =
    (stdout, text) => stdio.print(text)

  given stderrBytes(using stdio: Stdio): SimpleAppendable[Err.type, Bytes] =
    (stderr, bytes) => stdio.writeErr(bytes)

  given stderrText(using stdio: Stdio): SimpleAppendable[Err.type, Text] =
    (stderr, text) => stdio.printErr(text)

  given [OutType <: ji.OutputStream](using streamCut: Errant[StreamError]) => OutType is Appendable by Bytes as outputStreamBytes =
    (outputStream, stream) =>
      stream.each: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()

      outputStream.close()

  given [TargetType: Appendable by Text](using decoder: CharDecoder)
      => TargetType is Appendable by Bytes as decodingAdapter =
    (target, stream) => TargetType.append(target, decoder.decode(stream))

  given [TargetType: Appendable by Bytes](using encoder: CharEncoder)
      => TargetType is Appendable by Text as encodingAdapter =
    (target, stream) => TargetType.append(target, encoder.encode(stream))

trait Appendable:
  type Self
  type Element
  def append(target: Self, stream: LazyList[Element]): Unit
  def asWritable: Self is Writable by Element = append(_, _)

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Appendable by Element =
    (target, stream) => append(lambda(target), stream)

trait SimpleAppendable[TargetType, ElementType] extends Appendable:
  type Element = ElementType
  type Self = TargetType

  def append(target: TargetType, stream: LazyList[Element]): Unit = stream match
    case head #:: tail => appendElement(target, head); append(target, tail)
    case _             => ()

  def appendElement(target: TargetType, element: Element): Unit

object Readable:
  given Bytes is Readable by Bytes as bytes = LazyList(_)
  given Text is Readable by Text as text = LazyList(_)

  given [SourceType](using readable: SourceType is Readable by Text, encoder: CharEncoder)
      => SourceType is Readable by Bytes as encodingAdapter =
    source => encoder.encode(readable.read(source))

  given [SourceType](using readable: SourceType is Readable by Bytes, decoder: CharDecoder)
      => SourceType is Readable by Text as decodingAdapter =

    source => decoder.decode(readable.read(source))

  given [ElementType] => LazyList[ElementType] is Readable by ElementType as lazyList = identity(_)

  given (using stdio: Stdio) => In.type is Readable by Char as inCharReader = in =>
    def recur(count: ByteSize): LazyList[Char] =
      stdio.reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)

    LazyList.defer(recur(0L.b))

  given (using stdio: Stdio) => In.type is Readable by Byte as inByteReader = in =>
    def recur(count: ByteSize): LazyList[Byte] =
      stdio.in.read() match
        case -1  => LazyList()
        case int => int.toByte #:: recur(count + 1.b)

    LazyList.defer(recur(0L.b))

  given [InType <: ji.Reader](using streamCut: Errant[StreamError]) => InType is Readable by Char as reader = reader =>
    def recur(count: ByteSize): LazyList[Char] =
      try reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)
      catch case err: ji.IOException =>
        reader.close()
        raise(StreamError(count))(LazyList())

    LazyList.defer(recur(0L.b))

  given [InType <: ji.BufferedReader](using streamCut: Errant[StreamError]) => InType is Readable by Line as bufferedReader =
    reader =>
      def recur(count: ByteSize): LazyList[Line] =
        try reader.readLine() match
          case result if result eq null => LazyList()
          case line: String             => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
        catch case err: ji.IOException =>
          reader.close()
          raise(StreamError(count))(LazyList())

      LazyList.defer(recur(0L.b))

  given [InType <: ji.InputStream] => InType is Readable by Bytes as reliableInputStream = in =>
    val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
    val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](1024)).nn

    def recur(): LazyList[Bytes] =
      try channel.read(buf) match
        case -1 => LazyList().also(try channel.close() catch case err: Exception => ())
        case 0  => recur()

        case count =>
          buf.flip()
          val size: Int = count.min(1024)
          val array: Array[Byte] = new Array[Byte](size)
          buf.get(array)
          buf.clear()

          array.immutable(using Unsafe) #:: recur()

      catch case e: Exception => LazyList()

    LazyList.defer(recur())

  given [InType <: ji.InputStream](using streamCut: Errant[StreamError]) => InType is Readable by Bytes as inputStream = in =>
    val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
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

      catch case e: Exception => LazyList(raise(StreamError(total.b))(Bytes()))

    LazyList.defer(recur(0))

@capability
trait Readable:
  type Self
  type Element
  def read(value: Self): LazyList[Element]

  def contramap[SelfType2](lambda: SelfType2 => Self): SelfType2 is Readable by Element =
    source => read(lambda(source))

object Aggregable:
  given bytesBytes: Aggregable[Bytes, Bytes] = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes = source match
      case head #:: tail => buf.write(head.mutable(using Unsafe)); recur(buf, tail)
      case _             => buf.toByteArray().nn.immutable(using Unsafe)

    recur(ji.ByteArrayOutputStream(), source)

  given bytesText(using decoder: CharDecoder): Aggregable[Bytes, Text] =
    bytesBytes.map(decoder.decode)

  given lazyList[ElementType, ElementType2](using aggregable: Aggregable[ElementType, ElementType2])
          : Aggregable[ElementType, LazyList[ElementType2]] =

    element => LazyList(aggregable.aggregate(element))

  given functor[ElementType]: Functor[[ValueType] =>> Aggregable[ElementType, ValueType]] = new Functor:
    def map[ResultType, ResultType2]
        (aggregable: Aggregable[ElementType, ResultType], lambda: ResultType => ResultType2)
            : Aggregable[ElementType, ResultType2] =

      new Aggregable:
        def aggregate(value: LazyList[ElementType]): ResultType2 = lambda(aggregable.aggregate(value))

@capability
trait Aggregable[-ElementType, +ResultType]:
  def aggregate(source: LazyList[ElementType]): ResultType

extension [ValueType](value: ValueType)
  def stream[ElementType](using readable: ValueType is Readable by ElementType): LazyList[ElementType] =
    readable.read(value)

  def readAs[ResultType]
      (using readable: ValueType is Readable by Bytes, aggregable: Aggregable[Bytes, ResultType])
          : ResultType =

    aggregable.aggregate(readable.read(value))

  def writeTo[TargetType](target: TargetType)[ElementType]
      (using readable: ValueType is Readable by ElementType, writable: TargetType is Writable by ElementType)
          : Unit =

    writable.write(target, readable.read(value))

  def appendTo[TargetType](target: TargetType)[ElementType]
      (using readable:   ValueType is Readable by ElementType,
             appendable: TargetType is Appendable by ElementType)
          : Unit =

    appendable.append(target, readable.read(value))

infix type by [ElementaryType, ElementType] = ElementaryType { type Element = ElementType }
