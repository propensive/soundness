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
package stratiform

import scala.sys

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import fulminate.*

// BASE-256 binary-to-text codec. The alphabet of §4 of the spec assigns
// each byte value `b` (0..255) a Unicode character `A[b]` whose code
// point is congruent to `b` mod 256. Encoding is a single array index;
// decoding is a single modulo operation per character.

object Base256:

  // §4 alphabet, in byte-value order. The 256 code points are drawn
  // from Basic Latin, Latin Supplements A/B, Greek, Cyrillic, Latin
  // Extended Additional, and Greek Extended — all in the BMP, so each
  // is one UTF-16 code unit. The defining property
  // `codepoint(alphabet(b)) ≡ b (mod 256)` is verified at module load
  // by `verifyAlphabet` below.
  val alphabetString: String =
    "ḀḁЂЃĄąĆćȈȉЊḋЌḍĎďȐȑĒГДȕЖЗĘęȚțĜĝḞḟḠḡḢḣḤĥȦȧШḩЪЫЬЭĮį" +
      "0123456789ĺĻļĽľĿŀABCDEFGHIJKLMNOPQRSTUVWXYZṛќѝŞşŠ" +
      "abcdefghijklmnopqrstuvwxyzŻżṽžſẀẁẂẃẄẅẆẇẈẉΊẋẌẍΎƏҐґƒẓΔƕƖẗẘẙҚқƜƝΞƟ" +
      "ƠơҢңƤƥΦƧƨΩΪΫάέήίưᾱβγδεζҷᾸικλμẽξοπӁӂÃτÅÆÇψωϊϋỌύώϏ" +
      "ÐǑǒǓÔϕӖϗῘÙῚӛӜӝÞӟàῡǢǣӤåæçǨῩӪӫìíӮӯðñỲỳôỵǶỷӸùῺûǼǽþǿ"

  val alphabet: Array[Char]^{} =
    val arr = new scala.Array[Char](256)
    var i = 0

    while i < 256 do
      arr(i) = alphabetString.charAt(i)
      i += 1

    arr.asInstanceOf[Array[Char]^{}]

  private val membership: Array[Boolean]^{} =
    val table = new scala.Array[Boolean](Char.MaxValue.toInt + 1)
    var i = 0

    while i < 256 do
      table(alphabet.readable(i).toInt) = true
      i += 1

    table.asInstanceOf[Array[Boolean]^{}]

  // Exact alphabet membership, used by the pragma parser to classify a
  // phrase as a schema signature by form (§8: length and membership are
  // checked at parse time; palimpsest decodability at resolution time).
  def valid(char: Char): Boolean = membership.readable(char.toInt)

  // Self-check the alphabet's defining property — every implementation
  // MUST verify it per §4. We do it at module load so a transcription
  // error fails fast rather than surfacing as silent decode corruption.
  locally:
    if alphabetString.length != 256 then
      sys.error(s"BASE-256 alphabet has ${alphabetString.length} chars, expected 256")

    var i = 0

    while i < 256 do
      val c = alphabet.readable(i)

      if c.toInt % 256 != i then
        val hex = String.format("%04X", Integer.valueOf(c.toInt))
        sys.error(s"BASE-256 alphabet[$i] = U+$hex (mod 256 = ${c.toInt % 256}, expected $i)")

      i += 1

    val seen = new scala.Array[Boolean](Char.MaxValue.toInt + 1)
    var j = 0

    while j < 256 do
      val ord = alphabet.readable(j).toInt
      if seen(ord) then sys.error(s"BASE-256 alphabet has duplicate char at index $j")
      seen(ord) = true
      j += 1

  // Encode a byte sequence to its BASE-256 textual form. Each byte
  // becomes one Unicode character; the result has the same length in
  // characters as the input has in bytes.
  def encode(data: Data): Text =
    val sb = new java.lang.StringBuilder(data.length * 2)
    var i = 0

    while i < data.length do
      sb.append(alphabet.readable(data.readable(i) & 0xff))
      i += 1

    Text(sb.toString)

  // Permissive decode (§9). Every input character is mapped to
  // `codepoint(c) mod 256`. Characters outside the alphabet are not
  // rejected; their residue is taken as-is.
  def decode(text: Text): Data =
    val s = text.s
    val out = new scala.Array[Byte](s.length)
    var i = 0

    while i < s.length do
      out(i) = (s.charAt(i).toInt % 256).toByte
      i += 1

    out.asInstanceOf[Array[Byte]^{}]

  // Strict decode (§9). Verifies every input character is a member of
  // the alphabet; raises a `Base256.Error` listing the first offending
  // character's position and code point. Successful decoding behaves
  // identically to `decode`.
  def decodeStrict(text: Text): Data raises Base256.Error =
    val s = text.s
    val out = new scala.Array[Byte](s.length)
    var i = 0

    while i < s.length do
      val c = s.charAt(i)
      if !membership.readable(c.toInt) then abort(Base256.Error(Base256.Error.Reason.NotInAlphabet(i, c)))
      out(i) = (c.toInt % 256).toByte
      i += 1

    out.asInstanceOf[Array[Byte]^{}]

  // Base256Error → Base256.Error
  object Error:

    object Reason:
      given communicable: Reason is Communicable =
        case NotInAlphabet(position, character) =>
          val cp = String.format("U+%04X", Integer.valueOf(character.toInt)).nn
          m"the character `$character` ($cp) at position $position is not in the BASE-256 alphabet"

    enum Reason(val number: Int) extends Clarification:
      case NotInAlphabet(position: Int, character: Char) extends Reason(1)

  case class Error(reason: Base256.Error.Reason)(using Diagnostics)
  extends fulminate.Error(607, reason.number)(m"the BASE-256 string is invalid because $reason")

