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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package hieroglyph

import language.experimental.pureFunctions

import java.nio as jn, jn.charset as jnc

import anticipation.*
import beneficence.*
import denominative.*
import rudiments.*
import vacuous.*

object CharDecoder:
  case class Focus(position: Int) derives CanEqual

  def system(using sanitizer: TextSanitizer): CharDecoder =
    unapply(jnc.Charset.defaultCharset.nn.displayName.nn.tt).get

  def unapply(name: Text)(using sanitizer: TextSanitizer): Option[CharDecoder] =
    Encoding.unapply(name).map(CharDecoder(_))

class CharDecoder(val encoding: Encoding)(using val sanitizer: TextSanitizer) extends Findable:
  type Self = Text
  type Form = Data

  def decoded(bytes: Data, omit: Boolean): Text =
    val buffer: StringBuilder = StringBuilder()
    decoded(LazyList(bytes)).each: text => buffer.append(text.s)
    buffer.toString.tt

  def decoded(bytes: Data): Text = decoded(bytes, false)

  def decoded(stream: LazyList[Data]): LazyList[Text] =
    val decoder = encoding.charset.newDecoder().nn
    val out = jn.CharBuffer.allocate(4096).nn
    val in = jn.ByteBuffer.allocate(4096).nn

    def recur(todo: LazyList[Array[Byte]], offset: Int = 0, total: Int = 0): LazyList[Text] =
      val count = in.remaining

      if !todo.nil then in.put(todo.head, offset, in.remaining.min(todo.head.length - offset))
      in.flip()

      def decode(): jnc.CoderResult =
        val result = decoder.decode(in, out, todo.nil).nn

        if !result.isMalformed then result else
          sanitizer.sanitize(total + in.position, encoding).let(out.put(_))
          in.position(in.position + result.length)
          decode()

      val status = decode()
      val text = out.flip().nn.toString.tt
      in.compact()
      out.clear()

      def continue =
        if todo.nil && !status.isOverflow then LazyList()
        else if !todo.nil && count >= todo.head.length - offset
        then recur(todo.tail, 0, total + todo.head.length - offset)
        else recur(todo, offset + count, total + count)

      if text.nil then continue else text #:: continue

    recur(stream.map(_.mutable(using Unsafe)))
