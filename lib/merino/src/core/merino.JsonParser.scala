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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package merino

import scala.annotation.*
import scala.collection.mutable.ArrayBuffer

import anticipation.*
import contingency.*
import denominative.*
import rudiments.*
import vacuous.*
import zephyrine.*

import JsonAst.AsciiByte.*
import JsonAst.{Issue, Position}

private object JsonParser:
  private[merino] type Raw =
    Long | Double | BigDecimal | String | (IArray[String], IArray[Any]) | IArray[Any] | Boolean
    | Null | Unset.type

  private inline val NumZero       = 0
  private inline val NumInt        = 1
  private inline val NumAfterDot   = 2
  private inline val NumFrac       = 3
  private inline val NumAfterE     = 4
  private inline val NumAfterESign = 5
  private inline val NumExp        = 6

  private val TenPow: Array[Double] = Array.tabulate(23): i =>
    var p = 1.0
    var n = i
    while n > 0 do { p *= 10.0; n -= 1 }
    p

  private val pool: ThreadLocal[JsonParser] =
    ThreadLocal.withInitial(() => new JsonParser).nn

  def parse(source: Data): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetData(source)
    parser.holes = false
    parser.parse()

  def parse(source: Data, holes: Boolean): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetData(source)
    parser.holes = holes
    parser.parse()

  def parse(input: Iterator[Data]): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetIterator(input)
    parser.holes = false
    parser.parse()

  def parse(input: Iterator[Data], holes: Boolean): Raw raises ParseError =
    val parser = pool.get.nn
    parser.resetIterator(input)
    parser.holes = holes
    parser.parse()

private final class JsonParser:
  import JsonParser.*
  import Lineation.untrackedData

  // Single Cursor-backed substrate. The same parser body runs whether the
  // input was supplied as an in-memory `Data` (pre-fills the cursor's buffer)
  // or as an `Iterator[Data]` (pulls chunks via the loader). Slicing is
  // uniform: `cursor.slice` exposes the buffer/offset/length triple, so
  // there's no longer a same-block fast path versus cross-block grab path.
  private var cursor:    Cursor[Data]      = null.asInstanceOf[Cursor[Data]]
  private var heldToken: Cursor.Held | Null = null

  protected[merino] var holes: Boolean = false

  protected var arraySize:           Int = 16
  protected var chars:               Array[Char] = new Array(arraySize)
  protected var stringCursor:        Int = 0
  protected var arrayBufferId:       Int = -1
  protected val arrayBuffers:        ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty
  protected var stringArrayBufferId: Int = -1
  protected val stringArrayBuffers:  ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer.empty
  protected val numberBuilder:       java.lang.StringBuilder = java.lang.StringBuilder(32)

  def resetData(input: Data): Unit =
    cursor = Cursor[Data](input)
    stringCursor = 0
    arrayBufferId = -1
    stringArrayBufferId = -1
    heldToken = null

  def resetIterator(input: Iterator[Data]): Unit =
    cursor = Cursor[Data](input)
    stringCursor = 0
    arrayBufferId = -1
    stringArrayBufferId = -1
    heldToken = null

  // ──────────────────────────────────────────────────────────────────────────
  // Substrate (now inlined directly into the parser, since there is only one).

  protected inline def more: Boolean = cursor.more

  protected inline def peek: Byte =
    cursor.unsafeBuffer(using Unsafe).asInstanceOf[Array[Byte]](cursor.unsafePos(using Unsafe))

  protected inline def advance(): Unit = cursor.next()

  protected def errorAt(issue: Issue)(using Tactic[ParseError]): Nothing =
    abort(ParseError(JsonAst, Position(0, cursor.position.n0), issue))

  // A `Region` is just a `Cursor.Mark` (an absolute `Long` position). With
  // the single-buffer model there's no need to remember the starting block
  // for boundary detection.
  type Region = Cursor.Mark

  protected inline def begin(): Cursor.Mark = cursor.mark(using heldToken.nn)

  protected inline def slice(start: Cursor.Mark): String =
    val end = cursor.mark(using heldToken.nn)
    cursor.slice(start, end): (storage, off, len) =>
      val arr = storage.asInstanceOf[Array[Byte]]
      new String(arr, off, len, java.nio.charset.StandardCharsets.US_ASCII)

  protected inline def appendRegionToBuffer(start: Cursor.Mark): Unit =
    val end = cursor.mark(using heldToken.nn)
    cursor.slice(start, end): (storage, off, len) =>
      if len > 0 then
        val arr = storage.asInstanceOf[Array[Byte]]
        ensureStringSpace(len)
        var i = 0
        while i < len do
          chars(stringCursor + i) = (arr(off + i) & 0xFF).toChar
          i += 1
        stringCursor += len

  protected def bom(): Unit =
    cursor.hold:
      val mk = cursor.mark
      val bom =
        cursor.more && cursor.datum(using Unsafe) == -17.toByte
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte }
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

      if bom then cursor.next() else cursor.cue(mk)

  protected inline def holding[result](inline action: => result): result =
    cursor.hold:
      heldToken = summon[Cursor.Held]
      try action finally heldToken = null

  // ──────────────────────────────────────────────────────────────────────────
  // String buffer plumbing (unchanged).

  protected inline def resetString(): Unit = stringCursor = 0

  protected inline def ensureStringSpace(n: Int): Unit =
    while stringCursor + n > arraySize do arraySize *= 2
    if chars.length < arraySize then
      val newArr = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArr, 0, stringCursor)
      chars = newArr

  protected inline def appendChar(char: Char): Unit =
    if stringCursor == arraySize then
      arraySize *= 2
      val newArray = new Array[Char](arraySize)
      System.arraycopy(chars, 0, newArray, 0, stringCursor)
      chars = newArray
    chars(stringCursor) = char
    stringCursor += 1

  protected inline def getString(): String = String(chars, 0, stringCursor)

  protected inline def getArrayBuffer(): ArrayBuffer[Any] =
    arrayBufferId += 1
    if arrayBuffers.length <= arrayBufferId then
      val newBuffer = ArrayBuffer.empty[Any]
      arrayBuffers += newBuffer
      newBuffer
    else
      val buffer = arrayBuffers(arrayBufferId)
      buffer.clear()
      buffer

  protected inline def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

  protected inline def getStringArrayBuffer(): ArrayBuffer[String] =
    stringArrayBufferId += 1
    if stringArrayBuffers.length <= stringArrayBufferId then
      val newBuffer = ArrayBuffer.empty[String]
      stringArrayBuffers += newBuffer
      newBuffer
    else
      val buffer = stringArrayBuffers(stringArrayBufferId)
      buffer.clear()
      buffer

  protected inline def relinquishStringArrayBuffer(): Unit = stringArrayBufferId -= 1

  // ──────────────────────────────────────────────────────────────────────────
  // Parser body (unchanged from the previous abstract base).

  protected inline def must()(using Tactic[ParseError]): Byte =
    if more then peek else errorAt(Issue.PrematureEnd)

  protected inline def next()(using Tactic[ParseError]): Byte =
    advance()
    if more then peek else errorAt(Issue.PrematureEnd)

  private def skip(): Unit =
    while more && {
      val ch = peek
      ch == Space || ch == Tab || ch == Newline || ch == Return
    } do advance()

  private def fromHex(ch: Byte)(using Tactic[ParseError]): Int =
    if ch <= Num9 && ch >= Num0 then ch - Num0
    else if ch <= UpperF && ch >= UpperA then ch - UpperA + 10
    else if ch <= LowerF && ch >= LowerA then ch - LowerA + 10
    else errorAt(Issue.ExpectedHexDigit(ch.toChar))

  private def parseUnicode()(using Tactic[ParseError]): Char =
    var acc = fromHex(next()) << 12
    acc |= fromHex(next()) << 8
    acc |= fromHex(next()) << 4
    acc |= fromHex(next())
    acc.toChar

  private def parseString()(using Tactic[ParseError]): String = holding:
    val region = begin()

    // Fast scan for plain printable ASCII that needs no escape handling. A
    // signed Byte is >= 32 only when it's printable ASCII (32..127); negative
    // bytes (0x80..0xFF) come out as -128..-1 < 32, so this single comparison
    // rejects both control characters and UTF-8 lead bytes.
    while more && {
      val b = peek
      b >= 32 && b != Quote && b != Backslash
    } do advance()

    if !more then errorAt(Issue.PrematureEnd)

    if peek == Quote then slice(region).also(advance())
    else tail(region)

  private def tail(start: Region): String raises ParseError =
    resetString()
    appendRegionToBuffer(start)

    var continue = true
    while continue do
      if !more then errorAt(Issue.PrematureEnd)
      val ch = peek
      ch match
        case Quote =>
          continue = false

        case Tab | Newline | Return =>
          errorAt(Issue.InvalidWhitespace)

        case Backslash =>
          advance()
          if !more then errorAt(Issue.PrematureEnd)
          (peek: @switch) match
            case Quote     => appendChar('"')
            case Slash     => appendChar('/')
            case Backslash => appendChar('\\')
            case LowerB    => appendChar('\b')
            case LowerF    => appendChar('\f')
            case LowerN    => appendChar('\n')
            case LowerR    => appendChar('\r')
            case LowerT    => appendChar('\t')
            case LowerU    => appendChar(parseUnicode())
            case bad       => errorAt(Issue.IncorrectEscape(bad.toChar))

        case _ =>
          if ch == 0 && holes then appendChar(' ')
          else ((ch >> 5): @switch) match
            case 0                 => errorAt(Issue.NotEscaped(ch.toChar))
            case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)

            case _ =>
              if (ch & 0xE0) == 0xC0 then
                var char: Int = (ch & 0x1F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF0) == 0xE0 then
                var char: Int = (ch & 0x0F) << 12
                char |= (next() & 0x3F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF8) == 0xF0 then
                var char: Int = (ch & 0x07) << 18
                char |= (next() & 0x3F) << 12
                char |= (next() & 0x3F) << 6
                char |= next() & 0x3F
                appendChar(char.toChar)

      if continue then advance()

    advance()
    getString()

  protected inline def expect(byte: Byte, issue: Issue): Unit raises ParseError =
    if next() != byte then errorAt(issue)

  private def parseFalse(): false raises ParseError =
    expect(LowerA, Issue.ExpectedFalse)
    expect(LowerL, Issue.ExpectedFalse)
    expect(LowerS, Issue.ExpectedFalse)
    expect(LowerE, Issue.ExpectedFalse)
    advance()
    false

  private def parseTrue(): true raises ParseError =
    expect(LowerR, Issue.ExpectedTrue)
    expect(LowerU, Issue.ExpectedTrue)
    expect(LowerE, Issue.ExpectedTrue)
    advance()
    true

  private def parseNull(): Null raises ParseError =
    expect(LowerU, Issue.ExpectedNull)
    expect(LowerL, Issue.ExpectedNull)
    expect(LowerL, Issue.ExpectedNull)
    advance()
    null

  private def parseNumber(first: Int, negative: Boolean)
  :   Double | Long | BigDecimal raises ParseError =

    var content: Long = first.toLong
    var nibbles: Int = 1
    var bcdValid: Boolean = true
    var floating: Boolean = false
    var continue: Boolean = true
    var state: Int = if first == 0 then NumZero else NumInt

    inline def appendChar0(n: Int): Unit =
      if n <= 9 then numberBuilder.append(('0' + n).toChar)
      else if n == 0xA then numberBuilder.append('.')
      else if n == 0xB then numberBuilder.append('e')
      else if n == 0xC then numberBuilder.append("e-")

    inline def fallback(extraNibble: Int): Unit =
      numberBuilder.setLength(0)
      var i = nibbles - 1
      while i >= 0 do
        val n = ((content >>> (i * 4)) & 0xFL).toInt
        if n <= 9 then numberBuilder.append(('0' + n).toChar)
        else if n == 0xA then numberBuilder.append('.')
        else if n == 0xB then numberBuilder.append('e')
        else if n == 0xC then numberBuilder.append("e-")
        i -= 1
      bcdValid = false
      appendChar0(extraNibble)

    inline def appendNibble(n: Int): Unit =
      if bcdValid then
        if nibbles >= 15 then fallback(n)
        else
          content = (content << 4) | n.toLong
          nibbles += 1
      else
        appendChar0(n)

    inline def rewriteEAsNeg(): Unit =
      if bcdValid then content = (content & ~0xFL) | 0xCL
      else numberBuilder.append('-')

    while continue && more do
      val ch = peek
      (state: @switch) match
        case NumZero =>
          ch match
            case Period =>
              appendNibble(0xA); floating = true; state = NumAfterDot; advance()

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumInt =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case Period =>
              appendNibble(0xA); floating = true; state = NumAfterDot; advance()

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumAfterDot =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumFrac; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumFrac =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case UpperE | LowerE =>
              appendNibble(0xB); state = NumAfterE; advance()

            case _ =>
              continue = false

        case NumAfterE =>
          ch match
            case Plus =>
              advance(); state = NumAfterESign

            case Minus =>
              rewriteEAsNeg(); advance(); state = NumAfterESign

            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumAfterESign =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; advance()

            case _ =>
              errorAt(Issue.ExpectedDigit(ch.toChar))

        case NumExp =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); advance()

            case _ =>
              continue = false

        case _ => ()

    (state: @switch) match
      case NumAfterDot | NumAfterE | NumAfterESign =>
        errorAt(Issue.PrematureEnd)
      case _ => ()

    if bcdValid then
      var mantissa: Long = 0L
      var decimalDigits: Int = 0
      var explicitExp: Int = 0
      var expSign: Int = 1
      var inFraction: Boolean = false
      var inExponent: Boolean = false

      var i = nibbles - 1
      while i >= 0 do
        val n = ((content >>> (i * 4)) & 0xFL).toInt
        if n <= 9 then
          if inExponent then explicitExp = explicitExp*10 + n
          else
            mantissa = mantissa*10 + n
            if inFraction then decimalDigits += 1
        else if n == 0xA then
          inFraction = true
        else if n == 0xB then
          inExponent = true
        else if n == 0xC then
          inExponent = true
          expSign = -1
        i -= 1

      if !floating then
        if negative then -mantissa else mantissa
      else
        val totalExp = expSign * explicitExp - decimalDigits

        val mag =
          if mantissa == 0L then 0.0
          else if mantissa < (1L << 53) && totalExp >= 0 && totalExp <= 22 then
            mantissa.toDouble * TenPow(totalExp)
          else if mantissa < (1L << 53) && totalExp < 0 && totalExp >= -22 then
            mantissa.toDouble / TenPow(-totalExp)
          else
            java.math.BigDecimal.valueOf(mantissa).nn.scaleByPowerOfTen(totalExp).nn.doubleValue

        if negative then -mag else mag
    else
      if floating then
        val d = java.lang.Double.parseDouble(numberBuilder.toString)
        if negative then -d else d
      else
        try
          val v = java.lang.Long.parseLong(numberBuilder, 0, numberBuilder.length, 10)
          if negative then -v else v
        catch case _: NumberFormatException =>
          val d = java.lang.Double.parseDouble(numberBuilder.toString)
          if negative then -d else d

  private def parseValue(minus: Boolean = false)(using Tactic[ParseError]): Raw =
    if !more then errorAt(Issue.PrematureEnd)
    val ch = peek
    if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
      advance()
      parseNumber(ch & 0x0F, minus)
    else if minus then
      errorAt(Issue.ExpectedDigit(ch.toChar))
    else if holes && ch == 0 then
      advance()
      Unset
    else
      (ch: @switch) match
        case Quote       => advance() yet parseString()
        case Minus       => advance() yet parseValue(true)
        case OpenBracket => advance() yet parseArray()
        case LowerF      => parseFalse()
        case LowerN      => parseNull()
        case LowerT      => parseTrue()
        case OpenBrace   => advance() yet parseObject()
        case other       => errorAt(Issue.ExpectedSomeValue(other.toChar))

  private def parseArray()(using Tactic[ParseError]): IArray[Any] =
    val items: ArrayBuffer[Any] = getArrayBuffer()
    var continue = true

    while continue do
      skip()

      must() match
        case CloseBracket =>
          if !items.nil then errorAt(Issue.ExpectedSomeValue(']'))
          continue = false

        case _ =>
          val value = parseValue()
          skip()

          must() match
            case Comma => items += value

            case CloseBracket =>
              items += value
              continue = false

            case char =>
              errorAt(Issue.ExpectedSomeValue(char.toChar))

      advance()

    val result: IArray[Any] = items.toArray.asInstanceOf[IArray[Any]]
    relinquishArrayBuffer()
    result

  private def parseObject()(using Tactic[ParseError]): (IArray[String], IArray[Any]) =
    val keys: ArrayBuffer[String] = getStringArrayBuffer()
    val values: ArrayBuffer[Any] = getArrayBuffer()
    var continue = true
    while continue do
      skip()
      must() match
        case Quote =>
          advance()
          val string = parseString()
          skip()
          must() match
            case Colon =>
              advance()
              skip()
              val value = parseValue()
              skip()
              must() match
                case Comma =>
                  advance()
                  keys += string
                  values += value
                  skip()

                case CloseBrace =>
                  advance()
                  keys += string
                  values += value
                  continue = false

                case ch  => errorAt(Issue.UnexpectedChar(ch.toChar))
            case ch => errorAt(Issue.ExpectedColon(ch.toChar))

        case 0 if holes =>
          advance()
          skip()
          must() match
            case Colon =>
              advance()
              skip()
              val value = parseValue()
              skip()
              must() match
                case Comma =>
                  advance()
                  keys += " "
                  values += value
                  skip()

                case CloseBrace =>
                  advance()
                  keys += " "
                  values += value
                  continue = false

                case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

            case Comma =>
              advance()
              keys += " "
              values += Unset
              skip()

            case CloseBrace =>
              advance()
              keys += " "
              values += Unset
              continue = false

            case ch => errorAt(Issue.UnexpectedChar(ch.toChar))

        case CloseBrace =>
          if !keys.nil then errorAt(Issue.ExpectedSomeValue('}'))
          advance()
          continue = false

        case ch =>
          errorAt(Issue.ExpectedString(ch.toChar))

    val result = (keys.toArray, values.toArray).asInstanceOf[(IArray[String], IArray[Any])]

    relinquishStringArrayBuffer()
    relinquishArrayBuffer()
    result

  def parse()(using Tactic[ParseError]): Raw =
    bom()
    skip()
    if !more then abort(ParseError(JsonAst, Position(0, 0), Issue.EmptyInput))
    val result = parseValue()

    while more do
      val ch = peek
      ch match
        case Tab | Return | Newline | Space => advance()
        case other                          => errorAt(Issue.SpuriousContent(other.toChar))

    result
