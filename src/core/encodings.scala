/*
    Hieroglyph, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hieroglyph

import rudiments.*
import digression.*

import scala.collection.mutable as scm
import scala.jdk.CollectionConverters.SetHasAsScala

import java.nio as jn, jn.charset as jnc

import language.experimental.captureChecking


object CharDecoder:
  def system(using BadEncodingHandler): CharDecoder =
    unapply(Text(jnc.Charset.defaultCharset.nn.displayName.nn)).get

  def unapply(name: Text)(using BadEncodingHandler): Option[CharDecoder] =
    if !CharEncoder.all.contains(Text(name.s.toLowerCase.nn)) then None
    else Some(CharDecoder(Text(jnc.Charset.forName(name.s).nn.displayName.nn)))

object CharEncoder:
  private[hieroglyph] val all: Set[Text] =
    jnc.Charset.availableCharsets.nn.asScala.to(Map).values.to(Set).flatMap: cs =>
      cs.aliases.nn.asScala.to(Set) + cs.displayName.nn
    .map(_.toLowerCase.nn).map(Text(_))
  
  def system: CharEncoder = unapply(Text(jnc.Charset.defaultCharset.nn.displayName.nn)).get
  
  def unapply(name: Text): Option[CharEncoder] =
    if !all.contains(Text(name.s.toLowerCase.nn)) then None
    else Some(CharEncoder(Text(jnc.Charset.forName(name.s).nn.displayName.nn)))

class CharEncoder(val name: Text):
  def encode(text: Text): Bytes = text.s.getBytes(name.s).nn.immutable(using Unsafe)
  def encode(stream: LazyList[Text]): LazyList[Bytes] = stream.map(encode)

class SafeCharDecoder(name: Text) extends CharDecoder(name)(using badEncodingHandlers.skip)

class CharDecoder(val name: Text)(using handler: BadEncodingHandler):
  def decode(bytes: Bytes): Text =
    val buf: StringBuilder = StringBuilder()
    decode(LazyList(bytes)).foreach { text => buf.append(text.s) }
    Text(buf.toString)
  
  def decode(stream: LazyList[Bytes]): LazyList[Text] =
    val charset = jnc.Charset.forName(name.s).nn
    val decoder = charset.newDecoder().nn
    val out = jn.CharBuffer.allocate(4096).nn
    val in = jn.ByteBuffer.allocate(4096).nn

    def recur(todo: LazyList[Array[Byte]], offset: Int = 0, total: Int = 0): LazyList[Text] =
      val count = in.remaining
      val start = in.position

      if !todo.isEmpty then in.put(todo.head, offset, in.remaining.min(todo.head.length - offset))
      in.flip()
      
      def decode(): jnc.CoderResult =
        val result = decoder.decode(in, out, false).nn
        
        if !result.isMalformed then result else
          handler.handle(total + in.position, Unset).mm(out.put(_))
          in.position(in.position + result.length)
          decode()
      
      val status = decode()
      val text = Text(out.flip().nn.toString)
      in.compact()
      out.clear()
      
      def continue =
        if todo.isEmpty && !status.isOverflow then LazyList()
        else if !todo.isEmpty && count >= todo.head.length - offset
        then recur(todo.tail, 0, total + todo.head.length - offset)
        else recur(todo, offset + count, total + count)
      
      if text.s.isEmpty then continue else text #:: continue
    
    recur(stream.map(_.mutable(using Unsafe)))

package charDecoders:
  given utf8(using BadEncodingHandler): CharDecoder = CharDecoder(Text("UTF-8"))
  given ascii(using BadEncodingHandler): CharDecoder = CharDecoder(Text("ASCII"))
  given iso88591: CharDecoder = SafeCharDecoder(Text("ISO-8859-1"))

package charEncoders:
  given utf8: CharEncoder = CharEncoder(Text("UTF-8"))
  given ascii: CharEncoder = CharEncoder(Text("ASCII"))
  given iso88591: CharEncoder = CharEncoder(Text("ISO-8859-1"))

trait BadEncodingHandler:
  def handle(pos: Int, suggestion: Maybe[Char]): Maybe[Char]
  def complete(): Unit

package badEncodingHandlers:
  given strict
      (using badEncoding: CanThrow[UndecodableCharError])
      : BadEncodingHandler^{badEncoding} =
    new BadEncodingHandler:
      def handle(pos: Int, suggestion: Maybe[Char]): Char = throw UndecodableCharError(pos)
      def complete(): Unit = ()
  
  given skip: BadEncodingHandler with
    def handle(pos: Int, suggestion: Maybe[Char]): Maybe[Char] = Unset
    def complete(): Unit = ()
  
  given substitute: BadEncodingHandler with
    def handle(pos: Int, suggestion: Maybe[Char]): Maybe[Char] = '?'
    def complete(): Unit = ()
  
  given collect
      (using aggregate: CanThrow[AggregateError[UndecodableCharError]])
      : BadEncodingHandler^{aggregate} =
    new BadEncodingHandler:
      private val mistakes: scm.ArrayBuffer[UndecodableCharError] = scm.ArrayBuffer()
      def handle(pos: Int, suggestion: Maybe[Char]): Maybe[Char] = Unset
      def complete(): Unit = if !mistakes.isEmpty then throw AggregateError(mistakes.to(List))

case class UndecodableCharError(pos: Int)
extends Error(err"The byte sequence at position $pos could not be decoded")

// FIXME: This code was copied from Gossamer, which depends on Contextual, while Hieroglyph does not.
// extension (inline ctx: StringContext)
//   transparent inline def enc(inline parts: Any*): Encoding = ${EncodingPrefix.expand('ctx, 'parts)

// object EncodingPrefix extends Verifier[Encoding]:
//   def verify(enc: Text): Encoding = enc match
//     case Encoding(enc) => enc
//     case _             => throw InterpolationError(Text(s"$enc is not a valid character encoding"))
