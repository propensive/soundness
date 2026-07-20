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
package hieroglyph

import java.nio as jn, jn.charset as jnc

import anticipation.*
import beneficence.*
import denominative.*
import rudiments.*
import vacuous.*

object CharEncoder:
  def system: CharEncoder = unapply(jnc.Charset.defaultCharset.nn.displayName.nn.tt).get

  def unapply(name: Text): Option[CharEncoder] =
    Encoding.codecs.get(name.s.toLowerCase.nn.tt).map(CharEncoder(_))

class CharEncoder(val encoding: Encoding { type CanEncode = true })
extends Encodable, Findable:
  type Self = Text
  type Form = Data

  def encoded(text: Text): Data = text.s.getBytes(encoding.name.s).nn.immutable(using Unsafe)

  // Chunk boundaries are not character boundaries: a surrogate pair may be
  // split across two chunks, so encoding each chunk independently (as this
  // method formerly did) corrupts any astral character on a boundary. Chars
  // stage through a `CharBuffer` — the mirror of `CharDecoder.decoded` — so
  // pairs carry whole across chunks; malformed and unmappable input is
  // replaced, matching `getBytes` on the whole-value path above.
  def encoded(stream: LazyList[Text]): LazyList[Data] =
    val encoder =
      encoding.charset.newEncoder().nn
      . onMalformedInput(jnc.CodingErrorAction.REPLACE).nn
      . onUnmappableCharacter(jnc.CodingErrorAction.REPLACE).nn

    val in = jn.CharBuffer.allocate(4096).nn
    val out = jn.ByteBuffer.allocate(4096).nn

    def recur(todo: LazyList[Text], offset: Int = 0): LazyList[Data] =
      val count = in.remaining

      if !todo.nil then
        in.put(todo.head.s, offset, offset + count.min(todo.head.s.length - offset))

      in.flip()
      val status = encoder.encode(in, out, todo.nil).nn

      // An overflowed final round loops to drain; `flush` (significant only
      // for stateful charsets) happens on the true final round.
      if todo.nil && !status.isOverflow then encoder.flush(out)

      out.flip()
      val array = new Array[Byte](out.remaining)
      out.get(array)
      val data: Data = array.immutable(using Unsafe)
      out.clear()
      in.compact()

      def continue =
        if todo.nil && !status.isOverflow then LazyList()
        else if !todo.nil && count >= todo.head.s.length - offset then recur(todo.tail, 0)
        else recur(todo, offset + count)

      if data.length == 0 then continue else data #:: continue

    recur(stream)
