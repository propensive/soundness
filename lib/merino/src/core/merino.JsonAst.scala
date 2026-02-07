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
import rudiments.*
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

  def apply
       (value: Long | Double | BigDecimal | String | (IArray[String], IArray[Any]) | IArray[Any]
               | Boolean | Null | Unset.type)
  : JsonAst =

      value


  given Tactic[ParseError] => JsonAst is Aggregable by Data = source =>
    // FIXME: This is a horrible hack to avoid the problems with streaming
    parse(source.read[Data])

  def parse(source: Data): JsonAst raises ParseError =
    val stream: Stream[Data] = Stream(source)
    var line: Int = 0
    var colStart: Int = 0

    try
      if stream.nil then abort(ParseError(this, Position(0, 0), Issue.EmptyInput))

      val block: Data = stream.head
      val penultimate = block.length - 1
      var cur: Int = 0
      val numberText: StringBuilder = StringBuilder()
      var continue: Boolean = true
      var number: Long | Double | BigDecimal = 0L
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
          val buf = arrayBuffers(arrayBufferId)
          buf.clear()
          buf

      def relinquishArrayBuffer(): Unit = arrayBufferId -= 1

      def getStringArrayBuffer(): ArrayBuffer[String] =
        stringArrayBufferId += 1
        if stringArrayBuffers.length <= stringArrayBufferId then
          val newBuffer = ArrayBuffer.empty[String]
          stringArrayBuffers += newBuffer
          newBuffer
        else
          val buf = stringArrayBuffers(stringArrayBufferId)
          buf.clear()
          buf

      def relinquishStringArrayBuffer(): Unit = stringArrayBufferId -= 1

      var arraySize: Int = 16
      var stringArray: Array[Char] = new Array(arraySize)
      var stringCursor: Int = 0
      inline def resetString(): Unit = stringCursor = 0

      def appendChar(char: Char): Unit =
        if stringCursor == arraySize then
          arraySize *= 2
          val newArray = new Array[Char](arraySize)
          System.arraycopy(stringArray, 0, newArray, 0, stringCursor)
          stringArray = newArray

        stringArray(stringCursor) = char
        stringCursor += 1

      def getString(): String = String(stringArray, 0, stringCursor)

      if penultimate > 2 && block(0) == -17 && block(1) == -69 && block(2) == -65 then cur = 3

      def current: Byte = block(cur)
      def next(): Unit = cur += 1

      inline def getNext(): Byte =
        cur += 1
        block(cur)

      def skip(): Unit =
        while
          val ch = current
          if ch == Newline then
            colStart = cur
            line += 1

          ch == Space || (ch & bin"0000 1000") == bin"0000 1000" &&
              (ch == Newline || ch == Tab || ch == Return)
        do next()

      def error(issue: Issue): Nothing =
        abort(ParseError(this, Position(line, cur - colStart), issue))

      def parseValue(minus: Boolean = false): JsonAst =
        val ch = current
        if (ch & bin"1111 1000") == Num0 || (ch & bin"1111 1110") == bin"0011 1000" then
          next()
          parseNumber(ch & bin"0000 1111", minus)
        else if minus then error(Issue.ExpectedDigit(ch.toChar))
        else (current: @switch) match
          case Quote       => next(); parseString()
          case Minus       => next(); parseValue(true)
          case OpenBracket => next(); parseArray()
          case LowerF      => next(); parseFalse()
          case LowerN      => next(); parseNull()
          case LowerT      => next(); parseTrue()
          case OpenBrace   => next(); parseObject()
          case ch          => error(Issue.ExpectedSomeValue(ch.toChar))

      def parseObject(): (IArray[String], IArray[Any]) =
        val keys: ArrayBuffer[String] = getStringArrayBuffer()
        val values: ArrayBuffer[Any] = getArrayBuffer()
        var continue = true
        while continue do
          skip()
          current match
            case Quote =>
              next()
              val str = parseString()
              skip()
              current match
                case Colon =>
                  next()
                  skip()
                  val value = parseValue()
                  skip()
                  current match
                    case Comma =>
                      next()
                      keys += str
                      values += value
                      skip()
                    case CloseBrace =>
                      next()
                      keys += str
                      values += value
                      continue = false
                    case ch  => error(Issue.UnexpectedChar(ch.toChar))
                case ch => error(Issue.ExpectedColon(ch.toChar))
            case CloseBrace =>
              if !keys.nil then error(Issue.ExpectedSomeValue('}'))
              next()
              continue = false
            case ch => error(Issue.ExpectedString(ch.toChar))

        val result = (keys.toArray, values.toArray).asInstanceOf[(IArray[String], IArray[Any])]

        relinquishStringArrayBuffer()
        relinquishArrayBuffer()
        result

      def parseArray(): IArray[Any] =
        val arrayItems: ArrayBuffer[Any] = getArrayBuffer()
        var continue = true

        while continue do
          skip()
          current match
            case CloseBracket =>
              if !arrayItems.nil then error(Issue.ExpectedSomeValue(']'))
              continue = false
            case ch =>
              val value = parseValue()
              skip()
              current match
                case Comma        => arrayItems += value
                case CloseBracket => arrayItems += value; continue = false
                case ch           => error(Issue.ExpectedSomeValue(ch.toChar))

          next()

        val result: IArray[Any] = arrayItems.toArray.asInstanceOf[IArray[Any]]
        relinquishArrayBuffer()
        result

      def parseString(): String =
        val start = cur
        continue = true
        var difference = 0
        var size = 0

        while continue do
          current match
            case Quote =>
              continue = false
              size = cur - start - difference

            case Tab | Newline | Return =>
              error(Issue.InvalidWhitespace)

            case Backslash =>
              getNext() match
                case LowerU =>
                  cur += 4
                  difference += 5

                case ch =>
                  difference += 1

            case ch =>
              if (ch >> 5) > 5 then
                if (ch & bin"0000 0000 1110 0000") == bin"0000 0000 1100 0000" then
                  difference += 1
                  next()

                else if (ch & bin"0000 0000 1111 0000") == bin"0000 0000 1110 0000" then
                  difference += 2
                  cur += 2

                else if (ch & bin"0000 0000 1111 1000") == bin"0000 0000 1110 0000" then
                  difference += 3
                  cur += 3

          next()

        resetString()

        val end = cur - 1
        cur = start

        def parseUnicode(): Char =
          var acc = fromHex(getNext())*4096
          acc = acc | fromHex(getNext())*256
          acc = acc | fromHex(getNext())*16
          acc = acc | fromHex(getNext())
          acc.toChar

        while cur < end do
          current match
            case Backslash =>
              (getNext(): @switch) match
                case Quote     => appendChar('"')
                case Slash     => appendChar('/')
                case Backslash => appendChar('\\')
                case LowerB    => appendChar('\b')
                case LowerF    => appendChar('\f')
                case LowerN    => appendChar('\n')
                case LowerR    => appendChar('\r')
                case LowerT    => appendChar('\t')
                case LowerU    => appendChar(parseUnicode())
                case ch        => error(Issue.IncorrectEscape(ch.toChar))
              next()

            case ch =>
              ((ch >> 5): @switch) match
                case 0 => error(Issue.NotEscaped(ch.toChar))
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

              next()
        next()

        String(getString())

      def fromHex(ch: Byte): Int =
        if ch <= Num9 && ch >= Num0 then ch - Num0
        else if ch <= UpperF && ch >= UpperA then ch - UpperA
        else if ch <= LowerF && ch >= LowerA then ch - LowerA
        else error(Issue.ExpectedHexDigit(ch.toChar))

      def parseFalse(): false =
        var x: Long = current << 8
        x |= getNext()
        x <<= 8
        x |= getNext()
        x <<= 8
        x |= getNext()
        if x != 1634497381L then error(Issue.ExpectedFalse)
        next()
        false

      def parseTrue(): JsonAst =
        var x: Int = current << 8
        x |= getNext()
        x <<= 8
        x |= getNext()
        if x != 7501157L then error(Issue.ExpectedTrue)
        next()
        true

      def parseNull(): JsonAst =
        var x: Int = current << 8
        x |= getNext()
        x <<= 8
        x |= getNext()
        if x != 7695468L then error(Issue.ExpectedNull)
        next()
        null

      def parseNumber(first: Int, negative: Boolean): Double | Long | BigDecimal =
        var str: StringBuilder = StringBuilder((if negative then "-" else "")+first)
        var ch: Byte = 0
        var floating: Boolean = false
        continue = true

        try
          while continue do
            ch = current
            ch match
              case Period =>
                floating = true
                str.append('.')
                next()

              case UpperE | LowerE =>
                floating = true
                str.append('e')
                next()

              case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 | Minus
                   | Plus =>
                str.append(ch.toChar)
                next()

              case _ =>
                continue = false

          if floating then java.lang.Double.parseDouble(str.toString)
          else java.lang.Long.parseLong(str.toString)

        catch
          case err: ArrayIndexOutOfBoundsException =>
            if floating then java.lang.Double.parseDouble(str.toString)
            else java.lang.Long.parseLong(str.toString)

      def parseNumberOld(first: Long, negative: Boolean): Double | Long | BigDecimal =
        var mantissa: Long = first
        var exponent: Long = 0
        var decimalPosition: Int = 0
        var scale: Int = 0
        var ch: Byte = 0
        continue = true

        try
          val leadingZero: Boolean = first == 0

          transparent inline def digit(inline dec: Boolean, inline ch: Byte): Unit =
            if dec then
              mantissa = mantissa*10 + (ch & 15)
              next()
            else
              if mantissa >= 922337203685477580L then
                if negative then numberText.append('-')
                numberText.append(mantissa.toString)
                parseLargeNumber(decimalPosition != 0)
                continue = false
              else
                mantissa = mantissa*10 + (ch & 15)
                next()

          while continue do
            ch = current
            if ch <= Num9 && ch >= Num0 then digit(decimalPosition != 0, ch)
            else (ch: @switch) match
              case Period =>
                if decimalPosition != 0 then error(Issue.MultipleDecimalPoints)
                decimalPosition = cur
                next()
                ch = current
                if ch <= Num9 && ch >= Num0 then digit(decimalPosition != 0, ch)
                else error(Issue.ExpectedDigit(ch.toChar))

                while
                  ch = current
                  ch <= Num9 && ch >= Num0
                do
                  mantissa = mantissa*10 + (ch & 15)
                  ch = getNext()


              case UpperE | LowerE =>
                if decimalPosition != 0 then scale = decimalPosition - cur + 1
                next()

                ch = current
                var minus = false
                (ch: @switch) match
                  case Minus =>
                    minus = true
                    next()
                    ch = current

                  case Plus =>
                    next()
                    ch = current

                  case _ =>
                    ()
                if ch <= Num9 && ch >= Num0 then exponent = (ch & 15)
                else error(Issue.ExpectedDigit(ch.toChar))

                if minus then exponent *= -1
                next()
                while
                  ch = current
                  ch <= Num9 && ch >= Num0
                do
                  exponent = exponent*10 + (ch & 15)
                  next()

                continue = false

              case Tab | Return | Newline | Space | Comma | CloseBracket | CloseBrace =>
                if ch == Newline then
                  colStart = cur
                  line += 1
                if decimalPosition != 0 && scale == 0 then scale = decimalPosition - cur + 1
                else if leadingZero && mantissa != 0 then error(Issue.NumberHasLeadingZero)

                if negative then mantissa *= -1
                val exp = exponent + scale

                number =
                  if exp == 0 then mantissa else mantissa*math.pow(10.0, exponent.toDouble + scale)

                continue = false

              case ch =>
                error(Issue.UnexpectedChar(ch.toChar))
            end if
          end while
          number
        catch
          case err: ArrayIndexOutOfBoundsException =>
            cur -= 1
            if decimalPosition != 0 && scale == 0 then scale = decimalPosition - cur
            if current <= Num9 && current >= Num0 then
              next()
              val mantissa2 = if negative then -mantissa else mantissa
              mantissa2.toDouble*math.pow(10.0, exponent.toDouble + scale)
            else error(Issue.PrematureEnd)

      def parseLargeNumber(hasDecimalPoint: Boolean): Unit =
        var decimalPoint: Boolean = hasDecimalPoint
        continue = true
        var ch: Byte = 0

        while continue do
          ch = current
          if ch <= Num9 && ch >= Num0 then
            numberText.append(ch.toChar)
            continue = cur != penultimate
            next()
          else (ch: @switch) match
            case Period =>
              if decimalPoint then error(Issue.MultipleDecimalPoints)
              numberText.append('.')
              decimalPoint = true
              next()
              ch = current
              if ch <= Num9 && ch >= Num0 then
                numberText.append(ch.toChar)
                continue = cur != penultimate
                next()

            case UpperE | LowerE =>
              numberText.append('e')
              next()

              ch = current
              if ch == Minus || ch == Plus then
                numberText.append(ch)
                next()
                ch = current
                if ch <= Num9 && ch >= Num0 then
                  numberText.append(ch.toChar)
                  continue = cur != penultimate
                  next()

            case Tab | Return | Newline | Space | Comma | CloseBracket | CloseBrace =>
              if ch == Newline then
                colStart = cur
                line += 1
              number =
                try BigDecimal(numberText.toCharArray).also(numberText.setLength(0))
                catch case err: NumberFormatException => throw err

              continue = false

            case ch =>
              error(Issue.UnexpectedChar(ch.toChar))

      skip()
      val result = parseValue()
      while cur < block.length
      do
        (current: @switch) match
          case Newline              =>
            colStart = cur
            line += 1
          case Tab | Return | Space => ()
          case other                => error(Issue.SpuriousContent(other.toChar))

        next()

      result

    catch
      case err: ArrayIndexOutOfBoundsException =>
        abort(ParseError(this, Position(line, stream.head.length), Issue.PrematureEnd))
