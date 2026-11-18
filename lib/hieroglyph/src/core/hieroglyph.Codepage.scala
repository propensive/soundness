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
import fulminate.*
import rudiments.*
import vacuous.*

object Codepage:
  def system: Codepage = unapply(jnc.Charset.defaultCharset.nn.displayName.nn.tt).get

  def unapply(name: Text): Option[Codepage] =
    Encoding.codecs.at(name.s.toLowerCase.nn.tt).let(Codepage(_)).option

  // CodepageError → Codepage.Error
  case class Error(char: Char, encoding: Encoding)(using Diagnostics)
  extends fulminate.Error(35, 1, 0)
    ( m"character $char cannot be encoded with the encoding $encoding" )

class Codepage
  ( val encoding:  Encoding { type CanEncode = true },
    val sanitizer: EncodingSanitizer = EncodingSanitizer.substitute )
extends Encodable, Findable:
  type Self = Text
  type Form = Data

  // The same codepage under another sanitizer; itself, if nothing would change.
  def sanitized(sanitizer: EncodingSanitizer): Codepage =
    if sanitizer.substitutes == this.sanitizer.substitutes && sanitizer.substitutes then this
    else Codepage(encoding, sanitizer)

  // The `Charset` itself, not its name: `getBytes(String)` looks the charset up
  // by name on every call, which showed as 4.7% of an HTTP pipeline's profile.
  // `encoding.charset` resolves once, being a `lazy val`. A sanitizer which wants to hear of
  // unmappable characters takes the reporting path below.
  def encoded(text: Text): Data =
    if sanitizer.substitutes then Array.unsafeFrozen(text.s.getBytes(encoding.charset).nn)
    else reported(text)

  private def reported(text: Text): Data =
    val encoder = reporting()
    val in = jn.CharBuffer.wrap(text.s).nn
    val size = (text.s.length*encoder.maxBytesPerChar.toDouble).toInt + 16
    val out = jn.ByteBuffer.allocate(size).nn

    def recur(): Unit =
      val status = encoder.encode(in, out, true).nn

      if status.isError then
        substitute(encoder, in, out, status)
        recur()

    recur()
    encoder.flush(out)
    out.flip()
    val array = Array.allocate[Byte](out.remaining)
    out.get(array.raw)
    Array.freeze(array)

  private def reporting(): jnc.CharsetEncoder =
    encoding.charset.newEncoder().nn
    . onMalformedInput(jnc.CodingErrorAction.REPORT).nn
    . onUnmappableCharacter(jnc.CodingErrorAction.REPORT).nn

  // Tells the sanitizer of the character the encoder stopped at, then, if the sanitizer
  // returns, writes the replacement and steps past it.
  private def substitute
    ( encoder: jnc.CharsetEncoder, in: jn.CharBuffer, out: jn.ByteBuffer, status: jnc.CoderResult )
  :   Unit =

    sanitizer.unmappable(in.get(in.position), encoding)
    out.put(encoder.replacement())
    in.position(in.position + status.length)

  // Chunk boundaries are not character boundaries: a surrogate pair may be
  // split across two chunks, so encoding each chunk independently (as this
  // method formerly did) corrupts any astral character on a boundary. Chars
  // stage through a `CharBuffer` — the mirror of `Charset.decoded` — so
  // pairs carry whole across chunks; malformed and unmappable input is
  // replaced, or reported to the sanitizer, matching the whole-value path above.
  def encoded(stream: Chain[Text]): Chain[Data] =
    val encoder =
      if !sanitizer.substitutes then reporting() else
        encoding.charset.newEncoder().nn
        . onMalformedInput(jnc.CodingErrorAction.REPLACE).nn
        . onUnmappableCharacter(jnc.CodingErrorAction.REPLACE).nn

    val in = jn.CharBuffer.allocate(4096).nn
    val out = jn.ByteBuffer.allocate(4096).nn

    def recur(todo: Chain[Text], offset: Int = 0): Chain[Data] =
      val count = in.remaining

      // `Chain`'s companion primitives: this loop runs once per buffer-load on the encoding
      // hot path, where the typeclass route is overkill.
      if !todo.nil then
        val head = Chain.head(todo).s
        in.put(head, offset, offset + count.min(head.length - offset))

      in.flip()
      val status = encoder.encode(in, out, todo.nil).nn
      if status.isError then substitute(encoder, in, out, status)
      val unfinished = status.isOverflow || status.isError

      // An overflowed or interrupted final round loops to drain; `flush` (significant only
      // for stateful charsets) happens on the true final round.
      if todo.nil && !unfinished then encoder.flush(out)

      out.flip()
      val array = Array.allocate[Byte](out.remaining)
      out.get(array.raw)
      val data: Data = Array.freeze(array)
      out.clear()
      in.compact()

      def continue =
        if todo.nil && !unfinished then Chain()
        else if !todo.nil && count >= Chain.head(todo).s.length - offset
        then recur(Chain.tail(todo), 0)
        else recur(todo, offset + count)

      if data.length == 0 then continue else data #:: continue

    recur(stream)
