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

  // Per-thread parser pool. Reusing the parser keeps the ArrayBuffer pools, the
  // stringArray, and the numberBuilder warm across calls.
  private val pool: ThreadLocal[JsonParser] =
    ThreadLocal.withInitial(() => new JsonParser).nn

  def parse(source: Data)(using Tactic[ParseError]): Raw =
    val parser = pool.get.nn
    parser.reset(source)
    parser.parse()

private final class JsonParser:
  import JsonParser.*
  private var bytes: Array[Byte] = Array.empty
  private var pos: Int = 0
  private var end: Int = 0

  private def reset(input: Data): Unit =
    bytes = input.asInstanceOf[Array[Byte]]
    pos = 0
    end = bytes.length
    stringCursor = 0
    arrayBufferId = -1
    stringArrayBufferId = -1

  private var arraySize: Int = 16
  private var stringArray: Array[Char] = new Array(arraySize)
  private var stringCursor: Int = 0

  private var arrayBufferId: Int = -1
  private val arrayBuffers: ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty

  private var stringArrayBufferId: Int = -1
  private val stringArrayBuffers: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer.empty

  private val numberBuilder: java.lang.StringBuilder = java.lang.StringBuilder(32)

  private inline def resetString(): Unit = stringCursor = 0

  private def appendChar(char: Char): Unit =
    if stringCursor == arraySize then
      arraySize *= 2
      val newArray = new Array[Char](arraySize)
      System.arraycopy(stringArray, 0, newArray, 0, stringCursor)
      stringArray = newArray
    stringArray(stringCursor) = char
    stringCursor += 1

  private def getString(): String = String(stringArray, 0, stringCursor)

  private def getArrayBuffer(): ArrayBuffer[Any] =
    arrayBufferId += 1
    if arrayBuffers.length <= arrayBufferId then
      val newBuffer = ArrayBuffer.empty[Any]
      arrayBuffers += newBuffer
      newBuffer
    else
      val buffer = arrayBuffers(arrayBufferId)
      buffer.clear()
      buffer

  private def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

  private def getStringArrayBuffer(): ArrayBuffer[String] =
    stringArrayBufferId += 1
    if stringArrayBuffers.length <= stringArrayBufferId then
      val newBuffer = ArrayBuffer.empty[String]
      stringArrayBuffers += newBuffer
      newBuffer
    else
      val buffer = stringArrayBuffers(stringArrayBufferId)
      buffer.clear()
      buffer

  private def relinquishStringArrayBuffer(): Unit = stringArrayBufferId -= 1

  private def error(issue: Issue)(using Tactic[ParseError]): Nothing =
    abort(ParseError(JsonAst, Position(0, pos), issue))

  private inline def must()(using Tactic[ParseError]): Byte =
    if pos < end then bytes(pos) else error(Issue.PrematureEnd)

  private inline def getNext()(using Tactic[ParseError]): Byte =
    pos += 1
    if pos < end then bytes(pos) else error(Issue.PrematureEnd)

  private def skip(): Unit =
    while pos < end && {
      val ch = bytes(pos)
      ch == Space || ch == Tab || ch == Newline || ch == Return
    } do pos += 1

  private def fromHex(ch: Byte)(using Tactic[ParseError]): Int =
    if ch <= Num9 && ch >= Num0 then ch - Num0
    else if ch <= UpperF && ch >= UpperA then ch - UpperA + 10
    else if ch <= LowerF && ch >= LowerA then ch - LowerA + 10
    else error(Issue.ExpectedHexDigit(ch.toChar))

  private def parseUnicode()(using Tactic[ParseError]): Char =
    var acc = fromHex(getNext()) << 12
    acc |= fromHex(getNext()) << 8
    acc |= fromHex(getNext()) << 4
    acc |= fromHex(getNext())
    acc.toChar

  private def parseString()(using Tactic[ParseError]): String =
    val start = pos

    // Fast scan for plain printable ASCII that needs no escape handling.
    // A signed Byte is >= 32 only when it's printable ASCII (32..127);
    // negative bytes (0x80..0xFF) come out as -128..-1 < 32, so this single
    // comparison rejects both control characters and UTF-8 lead bytes.
    while pos < end && {
      val b = bytes(pos)
      b >= 32 && b != Quote && b != Backslash
    } do pos += 1

    if pos >= end then error(Issue.PrematureEnd)

    if bytes(pos) == Quote then
      val result =
        new String(bytes, start, pos - start, java.nio.charset.StandardCharsets.US_ASCII)
      pos += 1
      result
    else parseStringTail(start)

  private def parseStringTail(start: Int)(using Tactic[ParseError]): String =
    resetString()
    val n = pos - start
    if n > 0 then
      while stringCursor + n > arraySize do arraySize *= 2
      if stringArray.length < arraySize then
        val newArr = new Array[Char](arraySize)
        System.arraycopy(stringArray, 0, newArr, 0, stringCursor)
        stringArray = newArr
      var i = 0
      while i < n do
        stringArray(stringCursor + i) = (bytes(start + i) & 0xFF).toChar
        i += 1
      stringCursor += n

    var continue = true
    while continue do
      if pos >= end then error(Issue.PrematureEnd)
      val ch = bytes(pos)
      ch match
        case Quote =>
          continue = false

        case Tab | Newline | Return =>
          error(Issue.InvalidWhitespace)

        case Backslash =>
          pos += 1
          if pos >= end then error(Issue.PrematureEnd)
          (bytes(pos): @switch) match
            case Quote     => appendChar('"')
            case Slash     => appendChar('/')
            case Backslash => appendChar('\\')
            case LowerB    => appendChar('\b')
            case LowerF    => appendChar('\f')
            case LowerN    => appendChar('\n')
            case LowerR    => appendChar('\r')
            case LowerT    => appendChar('\t')
            case LowerU    => appendChar(parseUnicode())
            case bad       => error(Issue.IncorrectEscape(bad.toChar))

        case _ =>
          ((ch >> 5): @switch) match
            case 0                 => error(Issue.NotEscaped(ch.toChar))
            case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)

            case _ =>
              if (ch & 0xE0) == 0xC0 then
                var char: Int = (ch & 0x1F) << 6
                char |= getNext() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF0) == 0xE0 then
                var char: Int = (ch & 0x0F) << 12
                char |= (getNext() & 0x3F) << 6
                char |= getNext() & 0x3F
                appendChar(char.toChar)
              else if (ch & 0xF8) == 0xF0 then
                var char: Int = (ch & 0x07) << 18
                char |= (getNext() & 0x3F) << 12
                char |= (getNext() & 0x3F) << 6
                char |= getNext() & 0x3F
                appendChar(char.toChar)

      if continue then pos += 1

    pos += 1
    getString()

  private inline def expect(byte: Byte, issue: Issue)(using Tactic[ParseError]): Unit =
    if getNext() != byte then error(issue)

  private def parseFalse()(using Tactic[ParseError]): false =
    expect(LowerA, Issue.ExpectedFalse)
    expect(LowerL, Issue.ExpectedFalse)
    expect(LowerS, Issue.ExpectedFalse)
    expect(LowerE, Issue.ExpectedFalse)
    pos += 1
    false

  private def parseTrue()(using Tactic[ParseError]): Boolean =
    expect(LowerR, Issue.ExpectedTrue)
    expect(LowerU, Issue.ExpectedTrue)
    expect(LowerE, Issue.ExpectedTrue)
    pos += 1
    true

  private def parseNull()(using Tactic[ParseError]): Null =
    expect(LowerU, Issue.ExpectedNull)
    expect(LowerL, Issue.ExpectedNull)
    expect(LowerL, Issue.ExpectedNull)
    pos += 1
    null

  private def parseNumber(first: Int, negative: Boolean)(using Tactic[ParseError])
  :   Double | Long | BigDecimal =

    var content: Long = first.toLong
    var nibbleCount: Int = 1
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
      var i = nibbleCount - 1
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
        if nibbleCount >= 15 then fallback(n)
        else
          content = (content << 4) | n.toLong
          nibbleCount += 1
      else
        appendChar0(n)

    inline def rewriteEAsNeg(): Unit =
      if bcdValid then content = (content & ~0xFL) | 0xCL
      else numberBuilder.append('-')

    while continue && pos < end do
      val ch = bytes(pos)
      (state: @switch) match
        case NumZero =>
          ch match
            case Period =>
              appendNibble(0xA); floating = true; state = NumAfterDot; pos += 1

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; pos += 1

            case _ =>
              continue = false

        case NumInt =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); pos += 1

            case Period =>
              appendNibble(0xA); floating = true; state = NumAfterDot; pos += 1

            case UpperE | LowerE =>
              appendNibble(0xB); floating = true; state = NumAfterE; pos += 1

            case _ =>
              continue = false

        case NumAfterDot =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumFrac; pos += 1

            case _ =>
              error(Issue.ExpectedDigit(ch.toChar))

        case NumFrac =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); pos += 1

            case UpperE | LowerE =>
              appendNibble(0xB); state = NumAfterE; pos += 1

            case _ =>
              continue = false

        case NumAfterE =>
          ch match
            case Plus =>
              pos += 1; state = NumAfterESign

            case Minus =>
              rewriteEAsNeg(); pos += 1; state = NumAfterESign

            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; pos += 1

            case _ =>
              error(Issue.ExpectedDigit(ch.toChar))

        case NumAfterESign =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); state = NumExp; pos += 1

            case _ =>
              error(Issue.ExpectedDigit(ch.toChar))

        case NumExp =>
          ch match
            case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
              appendNibble(ch & 0xF); pos += 1

            case _ =>
              continue = false

        case _ => ()

    (state: @switch) match
      case NumAfterDot | NumAfterE | NumAfterESign =>
        error(Issue.PrematureEnd)
      case _ => ()

    if bcdValid then
      var mantissa: Long = 0L
      var decimalDigits: Int = 0
      var explicitExp: Int = 0
      var expSign: Int = 1
      var inFraction: Boolean = false
      var inExponent: Boolean = false

      var i = nibbleCount - 1
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
    if pos >= end then error(Issue.PrematureEnd)
    val ch = bytes(pos)
    if (ch & 0xF8) == Num0 || (ch & 0xFE) == 0x38 then
      pos += 1
      parseNumber(ch & 0x0F, minus)
    else if minus then
      error(Issue.ExpectedDigit(ch.toChar))
    else
      (ch: @switch) match
        case Quote       => pos += 1; parseString()
        case Minus       => pos += 1; parseValue(true)
        case OpenBracket => pos += 1; parseArray()
        case LowerF      => parseFalse()
        case LowerN      => parseNull()
        case LowerT      => parseTrue()
        case OpenBrace   => pos += 1; parseObject()
        case other       => error(Issue.ExpectedSomeValue(other.toChar))

  private def parseArray()(using Tactic[ParseError]): IArray[Any] =
    val arrayItems: ArrayBuffer[Any] = getArrayBuffer()
    var continue = true

    while continue do
      skip()
      must() match
        case CloseBracket =>
          if !arrayItems.nil then error(Issue.ExpectedSomeValue(']'))
          continue = false

        case _ =>
          val value = parseValue()
          skip()
          must() match
            case Comma        => arrayItems += value
            case CloseBracket => arrayItems += value; continue = false
            case ch           => error(Issue.ExpectedSomeValue(ch.toChar))

      pos += 1

    val result: IArray[Any] = arrayItems.toArray.asInstanceOf[IArray[Any]]
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
          pos += 1
          val string = parseString()
          skip()
          must() match
            case Colon =>
              pos += 1
              skip()
              val value = parseValue()
              skip()
              must() match
                case Comma =>
                  pos += 1
                  keys += string
                  values += value
                  skip()

                case CloseBrace =>
                  pos += 1
                  keys += string
                  values += value
                  continue = false

                case ch  => error(Issue.UnexpectedChar(ch.toChar))
            case ch => error(Issue.ExpectedColon(ch.toChar))

        case CloseBrace =>
          if !keys.nil then error(Issue.ExpectedSomeValue('}'))
          pos += 1
          continue = false

        case ch =>
          error(Issue.ExpectedString(ch.toChar))

    val result = (keys.toArray, values.toArray).asInstanceOf[(IArray[String], IArray[Any])]

    relinquishStringArrayBuffer()
    relinquishArrayBuffer()
    result

  def parse()(using Tactic[ParseError]): Raw =
    // Skip optional UTF-8 BOM (EF BB BF)
    if end >= 3 && bytes(0) == -17 && bytes(1) == -69 && bytes(2) == -65 then pos = 3

    skip()
    if pos >= end then abort(ParseError(JsonAst, Position(0, 0), Issue.EmptyInput))
    val result = parseValue()

    while pos < end do
      val ch = bytes(pos)
      ch match
        case Tab | Return | Newline | Space => pos += 1
        case other                          => error(Issue.SpuriousContent(other.toChar))

    result
