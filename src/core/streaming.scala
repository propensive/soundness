/*
    Turbulence, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import perforate.*
import hieroglyph.*
import anticipation.*

import java.io as ji
import java.nio as jn

//import language.experimental.captureChecking

object Writable:
  given outputStreamBytes
      (using streamCut: Raises[StreamError])
      : Writable[ji.OutputStream, Bytes] =
    (outputStream, stream) =>
      stream.foreach: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()
      outputStream.close()
  
  given outputStreamText
      (using streamCut: Raises[StreamError], encoder: CharEncoder)
      : Writable[ji.OutputStream, Text] =
    (outputStream, stream) =>
      stream.foreach: text =>
        outputStream.write(encoder.encode(text).mutable(using Unsafe))
        outputStream.flush()
      outputStream.close()

  given decodingAdapter
      [TargetType]
      (using writable: Writable[TargetType, Text], decoder: CharDecoder)
      : Writable[TargetType, Bytes] =
    (target, stream) => writable.write(target, decoder.decode(stream))
  
  given encodingAdapter
      [TargetType]
      (using writable: Writable[TargetType, Bytes], encoder: CharEncoder)
      : Writable[TargetType, Text] =
    (target, stream) => writable.write(target, encoder.encode(stream))

@capability
trait Writable[-TargetType, -ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit

  def contraMap[TargetType2](fn: TargetType2 => TargetType): Writable[TargetType2, ChunkType] =
    (target, stream) => write(fn(target), stream)

trait SimpleWritable[-TargetType, -ChunkType] extends Writable[TargetType, ChunkType]:
  def write(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => writeChunk(target, head); write(target, tail)
    case _             => 

  def writeChunk(target: TargetType, chunk: ChunkType): Unit

object Appendable:
  given stdoutBytes(using stdio: Stdio): SimpleAppendable[Out.type, Bytes] =
    (stderr, bytes) => stdio.write(bytes)
  
  given stdoutText(using stdio: Stdio): SimpleAppendable[Out.type, Text] =
    (stdout, text) => stdio.print(text)

  given stderrBytes(using stdio: Stdio): SimpleAppendable[Err.type, Bytes] =
    (stderr, bytes) => stdio.writeErr(bytes)
  
  given stderrText(using stdio: Stdio): SimpleAppendable[Err.type, Text] =
    (stderr, text) => stdio.printErr(text)

  given outputStreamBytes
      (using streamCut: Raises[StreamError])
      : Appendable[ji.OutputStream, Bytes] =
    (outputStream, stream) =>
      stream.foreach: bytes =>
        outputStream.write(bytes.mutable(using Unsafe))
        outputStream.flush()
      outputStream.close()
  
  given decodingAdapter
      [TargetType]
      (using appendable: Appendable[TargetType, Text], decoder: CharDecoder)
      : Appendable[TargetType, Bytes] =
    (target, stream) => appendable.append(target, decoder.decode(stream))
  
  given encodingAdapter
      [TargetType]
      (using appendable: Appendable[TargetType, Bytes], encoder: CharEncoder)
      : Appendable[TargetType, Text] =
    (target, stream) => appendable.append(target, encoder.encode(stream))

trait Appendable[-TargetType, -ChunkType]:
  def append(target: TargetType, stream: LazyList[ChunkType]): Unit
  def asWritable: Writable[TargetType, ChunkType] = append(_, _)
  
  def contraMap[TargetType2](fn: TargetType2 => TargetType): Appendable[TargetType2, ChunkType] =
    (target, stream) => append(fn(target), stream)

trait SimpleAppendable[-TargetType, -ChunkType] extends Appendable[TargetType, ChunkType]:

  def append(target: TargetType, stream: LazyList[ChunkType]): Unit = stream match
    case head #:: tail => appendChunk(target, head); append(target, tail)
    case _             => ()

  def appendChunk(target: TargetType, chunk: ChunkType): Unit

object Readable:
  given bytes: Readable[Bytes, Bytes] = LazyList(_)
  given text: Readable[Text, Text] = LazyList(_)
  
  given encodingAdapter
      [SourceType]
      (using readable: Readable[SourceType, Text], encoder: CharEncoder)
      : Readable[SourceType, Bytes] = source =>
    encoder.encode(readable.read(source))
  
  given decodingAdapter
      [SourceType]
      (using readable: Readable[SourceType, Bytes], decoder: CharDecoder)
      : Readable[SourceType, Text] = source =>
    decoder.decode(readable.read(source))

  given lazyList[ChunkType]: Readable[LazyList[ChunkType], ChunkType] = identity(_)

  given inCharReader(using stdio: Stdio): Readable[In.type, Char] = in =>
    def recur(count: ByteSize): LazyList[Char] =
      stdio.reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)
    
    LazyList.defer(recur(0L.b))

  given inByteReader(using stdio: Stdio): Readable[In.type, Byte] = in =>
    def recur(count: ByteSize): LazyList[Byte] =
      stdio.in.read() match
        case -1  => LazyList()
        case int => int.toByte #:: recur(count + 1.b)
    
    LazyList.defer(recur(0L.b))

  given reader(using streamCut: Raises[StreamError]): Readable[ji.Reader, Char] = reader =>
    def recur(count: ByteSize): LazyList[Char] =
      try reader.read() match
        case -1  => LazyList()
        case int => int.toChar #:: recur(count + 1.b)
      catch case err: ji.IOException =>
        reader.close()
        raise(StreamError(count))(LazyList())
    
    LazyList.defer(recur(0L.b))

  given bufferedReader(using streamCut: Raises[StreamError]): Readable[ji.BufferedReader, Line] = reader =>
    def recur(count: ByteSize): LazyList[Line] =
      try reader.readLine match
        case null         => LazyList()
        case line: String => Line(Text(line)) #:: recur(count + line.length.b + 1.b)
      catch case err: ji.IOException =>
        reader.close()
        raise(StreamError(count))(LazyList())
    
    LazyList.defer(recur(0L.b))

  given reliableInputStream: Readable[ji.InputStream, Bytes] = in =>
    val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
    val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn

    def recur(): LazyList[Bytes] =
      try channel.read(buf) match
        case -1 => LazyList().also(try channel.close() catch case err: Exception => ())
        case 0  => recur()
        
        case count =>
          buf.flip()
          val size: Int = count.min(65536)
          val array: Array[Byte] = new Array[Byte](size)
          buf.get(array)
          buf.clear()

          array.immutable(using Unsafe) #:: recur()
          
      catch case e: Exception => LazyList()
      
    LazyList.defer(recur())

  given inputStream(using streamCut: Raises[StreamError]): Readable[ji.InputStream, Bytes] = in =>
    val channel: jn.channels.ReadableByteChannel = jn.channels.Channels.newChannel(in).nn
    val buf: jn.ByteBuffer = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn

    def recur(total: Long): LazyList[Bytes] =
      try channel.read(buf) match
        case -1 => LazyList().also(try channel.close() catch case err: Exception => ())
        case 0  => recur(total)
        
        case count =>
          buf.flip()
          val size: Int = count.min(65536)
          val array: Array[Byte] = new Array[Byte](size)
          buf.get(array)
          buf.clear()

          array.immutable(using Unsafe) #:: recur(total + count)
          
      catch case e: Exception => LazyList(raise(StreamError(total.b))(Bytes()))
      
    LazyList.defer(recur(0))

@capability
trait Readable[-SourceType, +ChunkType]:
  def read(value: SourceType): LazyList[ChunkType]
  
  def contraMap[SourceType2](fn: SourceType2 => SourceType): Readable[SourceType2, ChunkType] = source =>
    read(fn(source))

object Aggregable:
  given bytesBytes: Aggregable[Bytes, Bytes] = source =>
    def recur(buf: ji.ByteArrayOutputStream, source: LazyList[Bytes]): Bytes = source match
      case head #:: tail => buf.write(head.mutable(using Unsafe)); recur(buf, tail)
      case _             => buf.toByteArray().nn.immutable(using Unsafe)
    
    recur(ji.ByteArrayOutputStream(), source)
  
  given bytesText(using decoder: CharDecoder): Aggregable[Bytes, Text] = bytesBytes.map(decoder.decode)

  given lazyList
      [ChunkType, ChunkType2]
      (using aggregable: Aggregable[ChunkType, ChunkType2])
      : Aggregable[ChunkType, LazyList[ChunkType2]] = chunk =>

    LazyList(aggregable.aggregate(chunk))

  given functor[ChunkType]: Functor[[ValueType] =>> Aggregable[ChunkType, ValueType]] = new Functor:
    def map
        [ResultType, ResultType2]
        (aggregable: Aggregable[ChunkType, ResultType], fn: ResultType => ResultType2)
        : Aggregable[ChunkType, ResultType2] =

      new Aggregable:
        def aggregate(value: LazyList[ChunkType]): ResultType2 = fn(aggregable.aggregate(value))
      
@capability
trait Aggregable[-ChunkType, +ResultType]:
  def aggregate(source: LazyList[ChunkType]): ResultType

extension [ValueType](value: ValueType)
  def stream[ChunkType](using readable: Readable[ValueType, ChunkType]): LazyList[ChunkType] =
    readable.read(value)
  
  def readAs
      [ResultType]
      (using readable: Readable[ValueType, Bytes], aggregable: Aggregable[Bytes, ResultType])
      : ResultType =

    aggregable.aggregate(readable.read(value))
  
  def writeTo
      [TargetType]
      (target: TargetType)
      [ChunkType]
      (using readable: Readable[ValueType, ChunkType], writable: Writable[TargetType, ChunkType])
      : Unit =

    writable.write(target, readable.read(value))
  
  def appendTo
      [TargetType]
      (target: TargetType)
      [ChunkType]
      (using readable: Readable[ValueType, ChunkType], appendable: Appendable[TargetType, ChunkType])
      : Unit =

    appendable.append(target, readable.read(value))
