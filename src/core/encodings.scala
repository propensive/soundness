package turbulence

import rudiments.*
import deviation.*

import scala.collection.mutable as scm
import java.nio.charset as jnc

import language.experimental.captureChecking

object Encoding:
  import scala.jdk.CollectionConverters.SetHasAsScala
  final val empty: Array[Byte] = Array.empty[Byte]

  val all: Set[Text] =
    jnc.Charset.availableCharsets.nn.asScala.to(Map).values.to(Set).flatMap: cs =>
      cs.aliases.nn.asScala.to(Set) + cs.displayName.nn
    .map(_.toLowerCase.nn).map(Text(_))
  
  def unapply(name: Text): Option[Encoding] = if !all.contains(Text(name.s.toLowerCase.nn)) then None else Some:
    val charset = jnc.Charset.forName(name.s).nn.displayName.nn
    charset match
      case "UTF-8"      => characterEncodings.utf8
      case "US-ASCII"   => characterEncodings.ascii
      case "ISO-8859-1" => characterEncodings.iso88591
      case other        => ???

trait Encoding:
  def name: Text
  def trimLength(buf: Bytes): Int = 0
  def runLength(byte: Byte): Int = 1
  def convertStream(stream: {*} LazyList[Bytes])(using handler: {*} BadEncodingHandler)
                   : {stream, handler} LazyList[Text]
  
  def readBytes(bytes: Bytes): Text = Text(String(bytes.mutable(using Unsafe), name.s))
  def getBytes(text: Text): Bytes = text.s.getBytes(name.s).nn.immutable(using Unsafe)

trait VariableLengthEncoding extends Encoding:
  def convertStream(stream: {*} LazyList[Bytes])(using handler: {*} BadEncodingHandler)
                   : {stream, handler} LazyList[Text] =
    def read(stream: LazyList[Bytes], carried: Array[Byte] = Encoding.empty, skip: Int = 0): LazyList[Text] =
      stream match
        case head #:: tail =>
          val array = head.mutable(using Unsafe)
          if carried.length > 0 then
            val need = runLength(carried(0))
            val got = array.length + carried.length
            if got < need then read(tail, carried ++ array)
            else if got == need then Text(String(carried ++ array, name.s)) #:: read(tail, Encoding.empty)
            else
              Text(String(carried ++ array.take(need - carried.length), name.s)) #::
                  read(stream, Encoding.empty, need - carried.length)
          else
            val carry = trimLength(array.immutable(using Unsafe))
            Text(String(array, skip, array.length - carry - skip, name.s)) #::
                read(tail, array.takeRight(carry))
        
        case _ =>
          LazyList()
      
    read(stream)
  
package characterEncodings:
  given utf8: Encoding = new VariableLengthEncoding:
    def name: Text = Text("UTF-8")
    
    override def trimLength(arr: Bytes): Int =
      val len = arr.length
      def last = arr(len - 1)
      def last2 = arr(len - 2)
      def last3 = arr(len - 3)
      
      if len > 0 && ((last & -32) == -64 || (last & -16) == -32 || (last & -8) == -16) then 1
      else if len > 1 && ((last2 & -16) == -32 || (last2 & -8) == -16) then 2
      else if len > 2 && ((last3 & -8) == -16) then 3
      else 0
    
    override def runLength(byte: Byte): Int =
      if (byte & -32) == -64 then 2 else if (byte & -16) == -32 then 3 else if (byte & -8) == -16 then 4 else 1
    
  given ascii: Encoding with
    def name: Text = Text("ASCII")
    
    def convertStream(stream: {*} LazyList[Bytes])(using handler: {*} BadEncodingHandler)
                     : {stream, handler} LazyList[Text] =
      val builder: StringBuilder = StringBuilder()
      def recur(stream: {*} LazyList[Bytes], count: Int): {stream} LazyList[Text] = stream match
        case head #:: tail =>
          head.indices.foreach: index =>
            val char: Int = head(index)
            if char > 127
            then handler.handle(count + index, IArray(head(index)), '?').mm(builder.append(_))
            else builder.append(char.toChar)
          
          Text(builder.toString).tap(builder.clear().waive) #:: recur(tail, count + head.length)

        case _=>
          LazyList()
          
      recur(stream, 0)
          
  given iso88591: Encoding with
    def name: Text = Text("ISO-8859-1")
    
    def convertStream(stream: {*} LazyList[Bytes])(using handler: {*} BadEncodingHandler)
                     : {stream, handler} LazyList[Text] =
      val builder: StringBuilder = StringBuilder()
      def recur(stream: {*} LazyList[Bytes], count: Int): {stream} LazyList[Text] = stream match
        case head #:: tail =>
          head.foreach { char => builder.append(char.toChar) }
          Text(builder.toString).tap(builder.clear().waive) #:: recur(tail, count + head.length)

        case _ => LazyList()
          
      recur(stream, 0)
          
trait BadEncodingHandler:
  def handle(pos: Int, bytes: Bytes, suggestion: Maybe[Char]): Maybe[Char]
  def complete(): Unit

package badEncodingHandlers:
  given strict(using badEncoding: CanThrow[BadEncodingError]): ({badEncoding} BadEncodingHandler) =
    new BadEncodingHandler:
      def handle(pos: Int, bytes: Bytes, suggestion: Maybe[Char]): Char = throw BadEncodingError(pos, bytes)
      def complete(): Unit = ()
  
  given skip: BadEncodingHandler with
    def handle(pos: Int, bytes: Bytes, suggestion: Maybe[Char]): Maybe[Char] = Unset
    def complete(): Unit = ()
  
  given substitute: BadEncodingHandler with
    def handle(pos: Int, bytes: Bytes, suggestion: Maybe[Char]): Maybe[Char] = '?'
    def complete(): Unit = ()
  
  given collect
      (using aggregate: CanThrow[AggregateError[BadEncodingError]])
      : ({aggregate} BadEncodingHandler) =
    new BadEncodingHandler:
      private val mistakes: scm.ArrayBuffer[BadEncodingError] = scm.ArrayBuffer()
      def handle(pos: Int, bytes: Bytes, suggestion: Maybe[Char]): Maybe[Char] = Unset
      def complete(): Unit = if !mistakes.isEmpty then throw AggregateError(mistakes.to(List))

case class BadEncodingError(pos: Int, bytes: Bytes)
extends Error(err"The byte sequence $bytes at position $pos is not valid")
