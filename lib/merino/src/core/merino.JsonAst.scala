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
import fulminate.*
import hypotenuse.*
import prepositional.*
import proscenium.*
import turbulence.*
import vacuous.*
import zephyrine.*

import JsonAst.AsciiByte.*

object JsonAst extends Format:
  def name: Text = "JSON"

  case class Position(line: Int, column: Int) extends Format.Position:
    def describe: Text = ("line "+line+", column "+column).tt

  enum Issue extends Format.Issue:
    case EmptyInput
    case UnexpectedChar(found: Char)
    case ExpectedTrue
    case ExpectedFalse
    case ExpectedNull
    case ExpectedSomeValue(char: Char)
    case ExpectedColon(found: Char)
    case InvalidWhitespace
    case ExpectedString(found: Char)
    case ExpectedHexDigit(found: Char)
    case PrematureEnd
    case NumberHasLeadingZero
    case SpuriousContent(found: Char)
    case LeadingDecimalPoint
    case NotEscaped(char: Char)
    case IncorrectEscape(char: Char)
    case MultipleDecimalPoints
    case ExpectedDigit(found: Char)

    def describe: Message = this match
      case EmptyInput              => m"the input was empty"
      case UnexpectedChar(found)   => m"the character $found was not expected here"
      case ExpectedTrue            => m"true was expected"
      case ExpectedFalse           => m"false was expected"
      case ExpectedNull            => m"null was expected"
      case ExpectedSomeValue(char) => m"a value was expected but $char was found instead"
      case ExpectedColon(found)    => m"a colon was expected but $found was found instead"
      case InvalidWhitespace       => m"invalid whitespace was found"
      case ExpectedString(found)   => m"a string was expected but $found was found instead"
      case ExpectedHexDigit(found) => m"a hexadecimal digit was expected"
      case PrematureEnd            => m"the content ended prematurely"
      case SpuriousContent(found)  => m"$found was found after the full JSON value was read"
      case LeadingDecimalPoint     => m"a number cannot start with a decimal point"
      case NotEscaped(char)        => m"the character $char must be escaped with a backslash"
      case ExpectedDigit(found)    => m"a digit was expected but $found was found instead"
      case MultipleDecimalPoints   => m"a number cannot contain more than one decimal point"

      case NumberHasLeadingZero =>
        m"a number cannot start with a zero except when followed by a decimal point"

      case IncorrectEscape(char) =>
        m"the character $char was escaped with a backslash unnecessarily"

  object AsciiByte:
    inline final val Tab:          9   = 9   // '\t'
    inline final val Newline:      10  = 10  // '\n'
    inline final val Return:       13  = 13  // '\r'
    inline final val Space:        32  = 32  // ' '
    inline final val Comma:        44  = 44  // ','
    inline final val Quote:        34  = 34  // '"'
    inline final val Minus:        45  = 45  // '-'
    inline final val Plus:         43  = 43  // '+'
    inline final val Slash:        47  = 47  // '/'
    inline final val Period:       46  = 46  // '.'
    inline final val Num0:         48  = 48  //'0'
    inline final val Num1:         49  = 49  //'1'
    inline final val Num2:         50  = 50  //'2'
    inline final val Num3:         51  = 51  //'3'
    inline final val Num4:         52  = 52  //'4'
    inline final val Num5:         53  = 53  //'5'
    inline final val Num6:         54  = 54  //'6'
    inline final val Num7:         55  = 55  //'7'
    inline final val Num8:         56  = 56  //'8'
    inline final val Num9:         57  = 57  //'9'
    inline final val Colon:        58  = 58  // ':'
    inline final val UpperA:       65  = 65  // 'A'
    inline final val UpperB:       66  = 66  // 'B'
    inline final val UpperC:       67  = 67  // 'C'
    inline final val UpperD:       68  = 68  // 'D'
    inline final val UpperE:       69  = 69  // 'E'
    inline final val UpperF:       70  = 70  // 'F'
    inline final val OpenBracket:  91  = 91  // '['
    inline final val CloseBracket: 93  = 93  // ']'
    inline final val Backslash:    92  = 92  // '\\'
    inline final val LowerA:       97  = 97  // 'a'
    inline final val LowerB:       98  = 98  // 'b'
    inline final val LowerC:       99  = 99  // 'c'
    inline final val LowerD:       100 = 100 // 'd'
    inline final val LowerE:       101 = 101 // 'e'
    inline final val LowerF:       102 = 102 // 'f'
    inline final val LowerL:       108 = 108 // 'l'
    inline final val LowerN:       110 = 110 // 'n'
    inline final val LowerR:       114 = 114 // 'r'
    inline final val LowerS:       115 = 115 // 's'
    inline final val LowerT:       116 = 116 // 't'
    inline final val LowerU:       117 = 117 // 'u'
    inline final val OpenBrace:    123 = 123 // '{'
    inline final val CloseBrace:   125 = 125 // '}'

  opaque type RawJson =
    Long | Double | BigDecimal | String | (IArray[String], IArray[Any]) | IArray[Any] | Boolean
    | Null | Unset.type

  private val TenPow: Array[Double] = Array.tabulate(23): i =>
    var p = 1.0
    var n = i
    while n > 0 do { p *= 10.0; n -= 1 }
    p

  private inline val NumZero       = 0
  private inline val NumInt        = 1
  private inline val NumAfterDot   = 2
  private inline val NumFrac       = 3
  private inline val NumAfterE     = 4
  private inline val NumAfterESign = 5
  private inline val NumExp        = 6


  def apply
    ( value
      : Long | Double | BigDecimal | String | (IArray[String], IArray[Any]) | IArray[Any] | Boolean
        | Null | Unset.type )
  :   JsonAst =

    value


  given Tactic[ParseError] => JsonAst is Aggregable by Data =
    source => parse(source.iterator)

  def parse(source: Data): JsonAst raises ParseError = parse(Iterator.single(source))

  def parse(input: Iterator[Data]): JsonAst raises ParseError =
    import Lineation.untrackedData
    val cursor = Cursor(input)

    def error(issue: Issue): Nothing =
      abort(ParseError(this, Position(cursor.line.n1, cursor.column.n1), issue))

    inline def must(): Byte = cursor.lay(error(Issue.PrematureEnd))(b => b)

    inline def getNext(): Byte =
      cursor.next()
      must()

    var arrayBufferId: Int = -1
    var stringArrayBufferId: Int = -1
    val arrayBuffers: ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty[ArrayBuffer[Any]]

    val stringArrayBuffers: ArrayBuffer[ArrayBuffer[String]] =
      ArrayBuffer.empty[ArrayBuffer[String]]

    def getArrayBuffer(): ArrayBuffer[Any] =
      arrayBufferId += 1
      if arrayBuffers.length <= arrayBufferId then
        val newBuffer = ArrayBuffer.empty[Any]
        arrayBuffers += newBuffer
        newBuffer
      else
        val buffer = arrayBuffers(arrayBufferId)
        buffer.clear()
        buffer

    def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

    def getStringArrayBuffer(): ArrayBuffer[String] =
      stringArrayBufferId += 1
      if stringArrayBuffers.length <= stringArrayBufferId then
        val newBuffer = ArrayBuffer.empty[String]
        stringArrayBuffers += newBuffer
        newBuffer
      else
        val buffer = stringArrayBuffers(stringArrayBufferId)
        buffer.clear()
        buffer

    def relinquishStringArrayBuffer(): Unit = stringArrayBufferId -= 1

    var arraySize: Int = 16
    var stringArray: Array[Char] = new Array(arraySize)
    var stringCursor: Int = 0
    inline def resetString(): Unit = stringCursor = 0

    val numberBuilder: java.lang.StringBuilder = java.lang.StringBuilder(32)

    def appendChar(char: Char): Unit =
      if stringCursor == arraySize then
        arraySize *= 2
        val newArray = new Array[Char](arraySize)
        System.arraycopy(stringArray, 0, newArray, 0, stringCursor)
        stringArray = newArray

      stringArray(stringCursor) = char
      stringCursor += 1

    def getString(): String = String(stringArray, 0, stringCursor)

    cursor.hold:
      val mk = cursor.mark

      val bom =
        cursor.more && cursor.datum(using Unsafe) == -17.toByte
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -69.toByte }
        && { cursor.next(); cursor.more && cursor.datum(using Unsafe) == -65.toByte }

      if bom then cursor.next() else cursor.cue(mk)

    def skip(): Unit =
      while cursor.more && {
        val ch = cursor.datum(using Unsafe)
        ch == Space || ch == Tab || ch == Newline || ch == Return
      } do cursor.next()

    def fromHex(ch: Byte): Int =
      if ch <= Num9 && ch >= Num0 then ch - Num0
      else if ch <= UpperF && ch >= UpperA then ch - UpperA
      else if ch <= LowerF && ch >= LowerA then ch - LowerA
      else error(Issue.ExpectedHexDigit(ch.toChar))

    def parseUnicode(): Char =
      var acc = fromHex(getNext())*4096
      acc = acc | fromHex(getNext())*256
      acc = acc | fromHex(getNext())*16
      acc = acc | fromHex(getNext())
      acc.toChar

    def parseString(): String = cursor.hold:
      // Fast scan for plain printable ASCII that needs no escape handling. A
      // signed Byte is >= 32 only when it's printable ASCII (32..127); negative
      // bytes (0x80..0xFF) come out as -128..-1 < 32, so this single
      // comparison rejects both control characters and UTF-8 lead bytes.
      val startBlock = cursor.block
      val startOffset = cursor.offsetInBlock
      val startMark = cursor.mark
      while cursor.more && {
        val b = cursor.datum(using Unsafe)
        b >= 32 && b != Quote && b != Backslash
      } do cursor.next()

      if !cursor.more then error(Issue.PrematureEnd)

      val ch = cursor.datum(using Unsafe)

      // Same-block fast path: build the String straight from the underlying
      // byte array without going through `grab`. We didn't cross a block
      // boundary if `cursor.block` is still the same instance as `startBlock`.
      if ch == Quote && (cursor.block.asInstanceOf[AnyRef] eq startBlock.asInstanceOf[AnyRef]) then
        val arr = startBlock.asInstanceOf[Array[Byte]]
        val str = new String
                   ( arr,
                     startOffset,
                     cursor.offsetInBlock - startOffset,
                     java.nio.charset.StandardCharsets.US_ASCII )
        cursor.next()
        str
      else parseStringTail(startMark)

    def parseStringTail(start: Cursor.Mark)(using Cursor.Held): String =
      resetString()

      // Pull the prefix bytes via `cursor.grab`, which handles the
      // cross-block case correctly (uses the buffer kept by `hold`).
      val prefix = cursor.grab(start, cursor.mark)
      val prefixArr = prefix.asInstanceOf[Array[Byte]]
      val prefixLen = prefixArr.length
      if prefixLen > 0 then
        while stringCursor + prefixLen > arraySize do arraySize *= 2
        if stringArray.length < arraySize then
          val newArr = new Array[Char](arraySize)
          System.arraycopy(stringArray, 0, newArr, 0, stringCursor)
          stringArray = newArr
        var i = 0
        while i < prefixLen do
          stringArray(stringCursor + i) = (prefixArr(i) & 0xFF).toChar
          i += 1
        stringCursor += prefixLen

      var continue = true
      while continue do
        cursor.lay(error(Issue.PrematureEnd)): ch =>
          ch match
            case Quote =>
              continue = false

            case Tab | Newline | Return =>
              error(Issue.InvalidWhitespace)

            case Backslash =>
              cursor.next()
              cursor.lay(error(Issue.PrematureEnd)):
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

            case ch =>
              ((ch >> 5): @switch) match
                case 0                 => error(Issue.NotEscaped(ch.toChar))
                case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)

                case _ =>
                  if (ch & bin"1110 0000") == bin"1100 0000" then
                    var char: Int = ((ch & bin"0001 1111") << 6)
                    char |= getNext() & bin"0011 1111"
                    appendChar(char.toChar)
                  else if (ch & bin"1111 0000") == bin"1110 0000" then
                    var char: Int = ((ch & bin"0001 1111") << 12)
                    char |= (getNext() & bin"0011 1111") << 6
                    char |= getNext() & bin"0011 1111"
                    appendChar(char.toChar)
                  else if (ch & bin"1111 1000") == bin"1111 0000" then
                    var char: Int = ((ch & bin"0001 1111") << 18)
                    char |= (getNext() & bin"0011 1111") << 12
                    char |= (getNext() & bin"0011 1111") << 6
                    char |= getNext() & bin"0011 1111"
                    appendChar(char.toChar)

        if continue then cursor.next()

      cursor.next()
      getString()

    inline def expect(inline byte: Byte, issue: Issue): Unit =
      if getNext() != byte then error(issue)

    def parseFalse(): false =
      expect(LowerA, Issue.ExpectedFalse)
      expect(LowerL, Issue.ExpectedFalse)
      expect(LowerS, Issue.ExpectedFalse)
      expect(LowerE, Issue.ExpectedFalse)
      cursor.next()
      false

    def parseTrue(): JsonAst =
      expect(LowerR, Issue.ExpectedTrue)
      expect(LowerU, Issue.ExpectedTrue)
      expect(LowerE, Issue.ExpectedTrue)
      cursor.next()
      true

    def parseNull(): JsonAst =
      expect(LowerU, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      expect(LowerL, Issue.ExpectedNull)
      cursor.next()
      null

    def parseNumber(first: Int, negative: Boolean): Double | Long | BigDecimal =
      var content: Long = first.toLong
      var nibbleCount: Int = 1
      var bcdValid: Boolean = true
      var floating: Boolean = false
      var continue: Boolean = true
      var state: Int = if first == 0 then NumZero else NumInt

      def fallback(extraNibble: Int): Unit =
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

      def appendChar0(n: Int): Unit =
        if n <= 9 then numberBuilder.append(('0' + n).toChar)
        else if n == 0xA then numberBuilder.append('.')
        else if n == 0xB then numberBuilder.append('e')
        else if n == 0xC then numberBuilder.append("e-")

      def appendNibble(n: Int): Unit =
        if bcdValid then
          if nibbleCount >= 15 then fallback(n) else
            content = (content << 4) | n.toLong
            nibbleCount += 1
        else
          appendChar0(n)

      def rewriteEAsNeg(): Unit =
        if bcdValid then content = (content & ~0xFL) | 0xCL
        else numberBuilder.append('-')

      while continue && cursor.more do
        val ch = cursor.datum(using Unsafe)
        (state: @switch) match
          case NumZero =>
            ch match
              case Period =>
                appendNibble(0xA); floating = true; state = NumAfterDot; cursor.next()

              case UpperE | LowerE =>
                appendNibble(0xB); floating = true; state = NumAfterE; cursor.next()

              case _ =>
                continue = false

          case NumInt =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); cursor.next()

              case Period =>
                appendNibble(0xA); floating = true; state = NumAfterDot; cursor.next()

              case UpperE | LowerE =>
                appendNibble(0xB); floating = true; state = NumAfterE; cursor.next()

              case _ =>
                continue = false

          case NumAfterDot =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumFrac; cursor.next()

              case _ =>
                error(Issue.ExpectedDigit(ch.toChar))

          case NumFrac =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); cursor.next()

              case UpperE | LowerE =>
                appendNibble(0xB); state = NumAfterE; cursor.next()

              case _ =>
                continue = false

          case NumAfterE =>
            ch match
              case Plus =>
                cursor.next(); state = NumAfterESign

              case Minus =>
                rewriteEAsNeg(); cursor.next(); state = NumAfterESign

              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumExp; cursor.next()

              case _ =>
                error(Issue.ExpectedDigit(ch.toChar))

          case NumAfterESign =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); state = NumExp; cursor.next()

              case _ =>
                error(Issue.ExpectedDigit(ch.toChar))

          case NumExp =>
            ch match
              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
                appendNibble(ch & 0xF); cursor.next()

              case _ =>
                continue = false

          case _ => ()

      (state: @switch) match
        case NumAfterDot | NumAfterE | NumAfterESign => error(Issue.PrematureEnd)
        case _                                       => ()

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

    def parseValue(minus: Boolean = false): JsonAst =
      cursor.lay(error(Issue.PrematureEnd)): ch =>
        if (ch & bin"1111 1000") == Num0 || (ch & bin"1111 1110") == bin"0011 1000" then
          cursor.next()
          parseNumber(ch & bin"0000 1111", minus)
        else if minus then
          error(Issue.ExpectedDigit(ch.toChar))
        else
          (ch: @switch) match
            case Quote       => cursor.next(); parseString()
            case Minus       => cursor.next(); parseValue(true)
            case OpenBracket => cursor.next(); parseArray()
            case LowerF      => parseFalse()
            case LowerN      => parseNull()
            case LowerT      => parseTrue()
            case OpenBrace   => cursor.next(); parseObject()
            case other       => error(Issue.ExpectedSomeValue(other.toChar))

    def parseArray(): IArray[Any] =
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

        cursor.next()

      val result: IArray[Any] = arrayItems.toArray.asInstanceOf[IArray[Any]]
      relinquishArrayBuffer()
      result

    def parseObject(): (IArray[String], IArray[Any]) =
      val keys: ArrayBuffer[String] = getStringArrayBuffer()
      val values: ArrayBuffer[Any] = getArrayBuffer()
      var continue = true
      while continue do
        skip()
        must() match
          case Quote =>
            cursor.next()
            val string = parseString()
            skip()
            must() match
              case Colon =>
                cursor.next()
                skip()
                val value = parseValue()
                skip()
                must() match
                  case Comma =>
                    cursor.next()
                    keys += string
                    values += value
                    skip()

                  case CloseBrace =>
                    cursor.next()
                    keys += string
                    values += value
                    continue = false

                  case ch  => error(Issue.UnexpectedChar(ch.toChar))
              case ch => error(Issue.ExpectedColon(ch.toChar))

          case CloseBrace =>
            if !keys.nil then error(Issue.ExpectedSomeValue('}'))
            cursor.next()
            continue = false

          case ch =>
            error(Issue.ExpectedString(ch.toChar))

      val result = (keys.toArray, values.toArray).asInstanceOf[(IArray[String], IArray[Any])]

      relinquishStringArrayBuffer()
      relinquishArrayBuffer()
      result

    skip()
    if cursor.finished then abort(ParseError(this, Position(0, 0), Issue.EmptyInput))
    val result = parseValue()

    while cursor.more do
      cursor.lay(()):
        case Tab | Return | Newline | Space => cursor.next()
        case other                          => error(Issue.SpuriousContent(other.toChar))

    result
