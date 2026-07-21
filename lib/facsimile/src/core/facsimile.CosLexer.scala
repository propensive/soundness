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
package facsimile

import anticipation.*
import contingency.*
import distillate.*
import gossamer.*
import hieroglyph.*
import rudiments.*
import vacuous.*

private[facsimile] object CosLexer:
  // The whitespace and delimiter classes of ISO 32000-2 §7.2.3; every other byte is `regular`
  // and participates in numbers, names and keywords.
  def whitespace(byte: Int): Boolean =
    byte == 0x00 || byte == 0x09 || byte == 0x0a || byte == 0x0c || byte == 0x0d || byte == 0x20

  def delimiter(byte: Int): Boolean = byte match
    case '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '/' | '%' => true
    case _                                                         => false

  def regular(byte: Int): Boolean = byte != -1 && !whitespace(byte) && !delimiter(byte)

  def numeric(byte: Int): Boolean =
    byte >= '0' && byte <= '9' || byte == '+' || byte == '-' || byte == '.'

  def hexadecimal(byte: Int): Int =
    if byte >= '0' && byte <= '9' then byte - '0'
    else if byte >= 'a' && byte <= 'f' then byte - 'a' + 10
    else if byte >= 'A' && byte <= 'F' then byte - 'A' + 10
    else -1

// A single-owner tokenizer over a `Scan`. The lexical grammar is shared between file bodies
// and content streams; out-of-band binary consumption (stream payloads, inline images) goes
// through `payloadStart`/`skip`/`read`, bypassing tokenization.
private[facsimile] class CosLexer(scan: Scan):
  import CosLexer.*

  def offset: Long = scan.offset

  def next(): CosToken raises PdfError =
    skipInterstice()
    val start = scan.offset

    scan.take() match
      case -1  => CosToken.End
      case '[' => CosToken.ArrayStart
      case ']' => CosToken.ArrayEnd
      case '(' => literal(start)
      case '/' => name()

      case '<' =>
        if scan.peek == '<' then
          scan.take()
          CosToken.DictStart
        else
          hexadecimalChars(start)

      case '>' =>
        if scan.peek == '>' then
          scan.take()
          CosToken.DictEnd
        else
          abort(PdfError(PdfError.Reason.Unparseable(start, t"a second '>'")))

      case ')' =>
        abort(PdfError(PdfError.Reason.Unparseable(start, t"anything but an unmatched ')'")))

      case byte if numeric(byte) => number(byte, start)
      case byte                  => keyword(byte)

  // After the `stream` keyword: consume its end-of-line marker (LF or CRLF; a lone CR is
  // tolerated) and return the absolute offset of the first payload byte.
  def payloadStart(): Long =
    if scan.peek == 0x0d then scan.skip(1)
    if scan.peek == 0x0a then scan.skip(1)
    scan.offset

  def skip(count: Long): Unit = scan.skip(count)
  def read(length: Int): Data = scan.read(length)

  // The binary payload of an inline image, between `ID` and `EI` (ISO 32000-2 §8.9.7): a
  // known length (`/L`) is read exactly; otherwise the data runs to the next standalone
  // `EI`, found by byte-level scanning — the one lexical construct tokens cannot express.
  def imageData(length: Optional[Int]): Data raises PdfError =
    if whitespace(scan.peek) then scan.take() // a single whitespace byte follows `ID`

    length.let(scan.read(_)).or:
      val bytes = Array.newBuilder[Byte]

      def boundary: Boolean =
        val standalone = !regular(scan.peek(3))
        whitespace(scan.peek) && scan.peek(1) == 'E' && scan.peek(2) == 'I' && standalone

      while scan.peek != -1 && !boundary do bytes += scan.take().toByte
      if scan.peek == -1 then abort(PdfError(PdfError.Reason.Truncated))
      scan.take() // the whitespace before `EI`, which is not payload
      bytes.result().immutable(using Unsafe)

  // Comments run to the end of the line and are whitespace (ISO 32000-2 §7.2.4).
  private def skipInterstice(): Unit =
    while
      if whitespace(scan.peek) then
        scan.take()
        true
      else if scan.peek == '%' then
        while scan.peek != -1 && scan.peek != 0x0a && scan.peek != 0x0d do scan.take()
        true
      else
        false
    do ()

  private def number(first: Int, start: Long): CosToken raises PdfError =
    val text = StringBuilder()
    text.append(first.toChar)

    while numeric(scan.peek) || scan.peek == 'e' || scan.peek == 'E'
    do
      // Exponents are not spec PDF numbers, but occur in the wild; accept a sign after `e`.
      if (scan.peek == 'e' || scan.peek == 'E') && (scan.peek(1) == '+' || scan.peek(1) == '-')
      then text.append(scan.take().toChar)

      text.append(scan.take().toChar)

    val content = text.toString

    if content.indexOf('.') >= 0 || content.indexOf('e') >= 0 || content.indexOf('E') >= 0 then
      val corrected: Text = if content == "." then t"0" else content.tt

      safely(CosToken.Real(corrected.as[Double]))
      . or(abort(PdfError(PdfError.Reason.Unparseable(start, t"a numeric object"))))
    else
      safely(CosToken.Integral(content.tt.as[Long]))
      . or(abort(PdfError(PdfError.Reason.Unparseable(start, t"a numeric object"))))

  private def name(): CosToken =
    val bytes = Array.newBuilder[Byte]

    while regular(scan.peek) do
      val byte = scan.take()

      if byte == '#' && hexadecimal(scan.peek) >= 0 && hexadecimal(scan.peek(1)) >= 0
      then bytes += ((hexadecimal(scan.take()) << 4) + hexadecimal(scan.take())).toByte
      else bytes += byte.toByte

    CosToken.Name(decode(bytes.result().immutable(using Unsafe)))

  private def keyword(first: Int): CosToken =
    val bytes = Array.newBuilder[Byte]
    bytes += first.toByte
    while regular(scan.peek) do bytes += scan.take().toByte
    CosToken.Keyword(decode(bytes.result().immutable(using Unsafe)))

  private def literal(start: Long): CosToken raises PdfError =
    val bytes = Array.newBuilder[Byte]
    var depth = 1

    while depth > 0 do scan.take() match
      case -1 =>
        abort(PdfError(PdfError.Reason.Truncated))

      case '(' =>
        depth += 1
        bytes += '('

      case ')' =>
        depth -= 1
        if depth > 0 then bytes += ')'

      // An unescaped end-of-line in a literal string is a single 0x0A (ISO 32000-2 §7.3.4.2).
      case 0x0d =>
        if scan.peek == 0x0a then scan.take()
        bytes += 0x0a

      case '\\' => scan.take() match
        case 'n'  => bytes += 0x0a
        case 'r'  => bytes += 0x0d
        case 't'  => bytes += 0x09
        case 'b'  => bytes += 0x08
        case 'f'  => bytes += 0x0c
        case '('  => bytes += '('
        case ')'  => bytes += ')'
        case '\\' => bytes += '\\'

        // A reverse solidus before an end-of-line continues the line, producing nothing.
        case 0x0d => if scan.peek == 0x0a then scan.take()
        case 0x0a => ()

        case digit if digit >= '0' && digit <= '7' =>
          var value = digit - '0'
          var count = 1

          while count < 3 && scan.peek >= '0' && scan.peek <= '7' do
            value = value*8 + (scan.take() - '0')
            count += 1

          bytes += (value & 0xff).toByte

        case -1    => abort(PdfError(PdfError.Reason.Truncated))
        case other => bytes += other.toByte // an unknown escape drops the reverse solidus

      case byte =>
        bytes += byte.toByte

    CosToken.Chars(bytes.result().immutable(using Unsafe))

  private def hexadecimalChars(start: Long): CosToken raises PdfError =
    val bytes = Array.newBuilder[Byte]
    var high: Int = -1

    while scan.peek != '>' do
      val byte = scan.take()

      if byte == -1 then abort(PdfError(PdfError.Reason.Truncated))
      else if !whitespace(byte) then
        val value = hexadecimal(byte)

        if value < 0
        then abort(PdfError(PdfError.Reason.Unparseable(start, t"a hexadecimal digit")))
        else if high < 0 then high = value
        else
          bytes += ((high << 4) + value).toByte
          high = -1

    scan.take()
    if high >= 0 then bytes += (high << 4).toByte // an odd final digit implies a trailing zero
    CosToken.Chars(bytes.result().immutable(using Unsafe))

  private def decode(bytes: Data): Text = charDecoders.utf8Decoder.decoded(bytes)
