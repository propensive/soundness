/*
    Hieroglyph, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import rudiments.*
import vacuous.*

import scala.collection.mutable as scm

import java.nio as jn, jn.charset as jnc

import language.experimental.captureChecking

object CharDecoder:
  given (using Quickstart) => CharDecoder as default = charDecoders.utf8

  def system(using TextSanitizer): CharDecoder =
    unapply(jnc.Charset.defaultCharset.nn.displayName.nn.tt).get

  def unapply(name: Text)(using TextSanitizer): Option[CharDecoder] =
    Encoding.unapply(name).map(CharDecoder(_))

class CharDecoder(val encoding: Encoding)(using sanitizer: TextSanitizer)
extends Decodable:
  type Self = Text
  type Format = Bytes

  def decode(bytes: Bytes, omit: Boolean): Text =
    val buf: StringBuilder = StringBuilder()
    decode(LazyList(bytes)).each { text => buf.append(text.s) }
    buf.toString.tt

  def decode(bytes: Bytes): Text = decode(bytes, false)

  def decode(stream: LazyList[Bytes]): LazyList[Text] =
    val decoder = encoding.charset.newDecoder().nn
    val out = jn.CharBuffer.allocate(4096).nn
    val in = jn.ByteBuffer.allocate(4096).nn

    def recur(todo: LazyList[Array[Byte]], offset: Int = 0, total: Int = 0): LazyList[Text] =
      val count = in.remaining

      if !todo.isEmpty then in.put(todo.head, offset, in.remaining.min(todo.head.length - offset))
      in.flip()

      def decode(): jnc.CoderResult =
        val result = decoder.decode(in, out, todo.isEmpty).nn

        if !result.isMalformed then result else
          sanitizer.sanitize(total + in.position, encoding).let(out.put(_))
          in.position(in.position + result.length)
          decode()

      val status = decode()
      val text = out.flip().nn.toString.tt
      in.compact()
      out.clear()

      def continue =
        if todo.isEmpty && !status.isOverflow then LazyList()
        else if !todo.isEmpty && count >= todo.head.length - offset
        then recur(todo.tail, 0, total + todo.head.length - offset)
        else recur(todo, offset + count, total + count)

      if text.s.isEmpty then continue else text #:: continue

    recur(stream.map(_.mutable(using Unsafe)))
