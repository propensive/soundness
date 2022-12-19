package merino

import annotation.*
import gossamer.*
import rudiments.*
import turbulence.*
import scala.collection.mutable.{HashMap, ListBuffer, ArrayBuffer}

import stdouts.stdout


type JsonNum = Long | Double | BigDecimal | BigInt
type RawJson = JsonNum | String | (IArray[String], IArray[Any]) | IArray[Any] | Boolean | Null

object AsciiByte:
  inline final val OpenBracket: 91 = 91 // '['
  inline final val CloseBracket: 93 = 93 // ']'
  inline final val OpenBrace: 123 = 123 // '{'
  inline final val CloseBrace: 125 = 125 // '}'
  inline final val Comma: 44 = 44 // ','
  inline final val Colon: 58 = 58 // ':'
  inline final val Quote: 34 = 34 // '"'
  inline final val Minus: 45 = 45 // '-'
  inline final val Plus: 43 = 43 // '+'
  inline final val Slash: 47 = 47 // '/'
  inline final val Period: 46 = 46 // '.'
  inline final val Backslash: 92 = 92 // '\\'
  inline final val Num0: 48 = 48 //'0'
  inline final val Num1: 49 = 49 //'1'
  inline final val Num2: 50 = 50 //'2'
  inline final val Num3: 51 = 51 //'3'
  inline final val Num4: 52 = 52 //'4'
  inline final val Num5: 53 = 53 //'5'
  inline final val Num6: 54 = 54 //'6'
  inline final val Num7: 55 = 55 //'7'
  inline final val Num8: 56 = 56 //'8'
  inline final val Num9: 57 = 57 //'9'
  inline final val LowerA: 97 = 97 // 'a'
  inline final val LowerB: 98 = 98 // 'b'
  inline final val LowerC: 99 = 99 // 'c'
  inline final val LowerD: 100 = 100 // 'd'
  inline final val LowerE: 101 = 101 // 'e'
  inline final val LowerF: 102 = 102 // 'f'
  inline final val LowerL: 108 = 108 // 'l'
  inline final val LowerN: 110 = 110 // 'n'
  inline final val LowerR: 114 = 114 // 'r'
  inline final val LowerS: 115 = 115 // 's'
  inline final val LowerT: 116 = 116 // 't'
  inline final val LowerU: 117 = 117 // 'u'
  inline final val UpperA: 65 = 65 // 'A'
  inline final val UpperB: 66 = 66 // 'B'
  inline final val UpperC: 67 = 67 // 'C'
  inline final val UpperD: 68 = 68 // 'D'
  inline final val UpperE: 69 = 69 // 'E'
  inline final val UpperF: 70 = 70 // 'F'
  inline final val Tab: 9 = 9 // '\t'
  inline final val Space: 32 = 32 // ' '
  inline final val Newline: 10 = 10 // '\n'
  inline final val Return: 13 = 13 // '\r'

import AsciiByte.*

// object Flag:
//   final inline val DecimalPoint = 1 << 0
//   final inline val Exponent = 1 << 1
//   final inline val Negative = 1 << 2
//   final inline val NegativeExponent = 1 << 3
//   final inline val LeadingZero = 1 << 4
//   final inline val Large = 1 << 5

object JsonParseError:
  enum Issue:
    case EmptyInput
    case UnexpectedChar(found: Byte)
    case ExpectedValue(json: RawJson)
    case ExpectedSomeValue(char: Byte)
    case ExpectedColon(found: Byte)
    case ExpectedEnd(found: Byte)
    case InvalidWhitespace
    case ExpectedString(found: Byte)
    case NumberStartsWithDecimalPoint
    case ExpectedHexDigit(found: Byte)
    case PrematureEnd
    case NumberHasLeadingZero
    case SpuriousContent(found: Byte)
    case LeadingDecimalPoint
    case NotEscaped(char: Byte)
    case IncorrectEscape(char: Byte)
    case MultipleDecimalPoints
    case ExpectedDigit(found: Byte)

import JsonParseError.Issue

case class JsonParseError(pos: Int, issue: Issue) extends Error(err"Could not parse JSON because $issue at $pos")

case class Json(value: RawJson)

object Json:
  def parse(stream: DataStream): Json throws JsonParseError | StreamCutError = try
    if stream.isEmpty then throw JsonParseError(0, Issue.EmptyInput)
    val block: Bytes = stream.head
    val penultimate = block.length - 1
    var cur: Int = 0
    val numberText: StringBuilder = StringBuilder()
    var continue: Boolean = true
    var number: JsonNum = 0L
    var arrayBufferId: Int = -1
    var stringArrayBufferId: Int = -1
    val arrayBuffers: ArrayBuffer[ArrayBuffer[Any]] = ArrayBuffer.empty[ArrayBuffer[Any]]
    val stringArrayBuffers: ArrayBuffer[ArrayBuffer[String]] = ArrayBuffer.empty[ArrayBuffer[String]]
    
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
    
    def getString(): String = String(stringArray, 0, stringCursor).intern.nn

    if penultimate > 2 && block(0) == -17 && block(1) == -69 && block(2) == -65 then cur = 3

    def current: Byte = block(cur)
    def next(): Unit = cur += 1
    
    inline def getNext(): Byte =
      cur += 1
      block(cur)

    def skip(): Unit =
      while
        val ch = current
        ch == Space || (ch & 8) == 8 && (ch == Newline || ch == Tab || ch == Return)
      do next()

    def abort(issue: Issue): Nothing = throw JsonParseError(cur, issue)

    def parseValue(minus: Boolean = false): RawJson =
      val ch = current
      if (ch & -8.toByte) == Num0 || (ch & -2.toByte) == 56 then
        next()
        parseNumber(ch & 15, minus)
      else if minus then abort(Issue.ExpectedDigit(ch))
      else (current: @switch) match
        case Quote       => next(); parseString()
        case Minus       => next(); parseValue(true)
        case OpenBracket => next(); parseArray()
        case LowerF      => next(); parseFalse()
        case LowerN      => next(); parseNull()
        case LowerT      => next(); parseTrue()
        case OpenBrace   => next(); parseObject()
        case ch          => abort(Issue.ExpectedSomeValue(ch))

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
                  case ch  => abort(Issue.UnexpectedChar(ch))
              case ch => abort(Issue.ExpectedColon(ch))
          case CloseBrace =>
            if !keys.isEmpty then abort(Issue.ExpectedSomeValue('}'))
            next()
            continue = false
          case ch => abort(Issue.ExpectedString(ch))
      
      val result = (keys.toArray.immutable(using Unsafe), values.toArray.immutable(using Unsafe))
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
            if !arrayItems.isEmpty then abort(Issue.ExpectedSomeValue(']'))
            continue = false
          case ch =>
            val value = parseValue()
            skip()
            current match
              case Comma        => arrayItems += value
              case CloseBracket => arrayItems += value; continue = false
              case ch           => abort(Issue.ExpectedEnd(ch))
        
        next()
      
      val result: IArray[Any] = arrayItems.toArray.immutable(using Unsafe)
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
            abort(Issue.InvalidWhitespace)

          case Backslash =>
            getNext() match
              case LowerU =>
                cur += 4
                difference += 5
              case ch =>
                difference += 1
          
          case ch =>
            if (ch >> 5) > 5 then
              if (ch & 224) == 192 then
                difference += 1
                next()
              else if (ch & 240) == 224 then
                difference += 2
                cur += 2
              else if (ch & 248) == 240 then
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
              case ch        => abort(Issue.IncorrectEscape(ch))
            next()

          case ch =>
            ((ch >> 5): @switch) match
              case 0 => abort(Issue.NotEscaped(ch))
              case 1 | 2 | 3 | 4 | 5 => appendChar(ch.toChar)
              case _ =>
                if (ch & 224) == 192 then
                  var char: Int = ((ch & 31) << 6)
                  char |= getNext() & 63
                  appendChar(char.toChar)
                else if (ch & 240) == 224 then
                  var char: Int = ((ch & 31) << 12)
                  char |= (getNext() & 63) << 6
                  char |= getNext() & 63
                  appendChar(char.toChar)
                else if (ch & 248) == 240 then
                  var char: Int = ((ch & 31) << 18)
                  char |= (getNext() & 63) << 12
                  char |= (getNext() & 63) << 6
                  char |= getNext() & 63
                  appendChar(char.toChar)
            
            next()
      next()
      
      String(getString())
              
    def fromHex(ch: Byte): Int =
      if ch <= Num9 && ch >= Num0 then ch - Num0
      else if ch <= UpperF && ch >= UpperA then ch - UpperA
      else if ch <= LowerF && ch >= LowerA then ch - LowerA
      else abort(Issue.ExpectedHexDigit(ch))

    def parseFalse(): false =
      var x: Long = current << 8
      x |= getNext()
      x <<= 8
      x |= getNext()
      x <<= 8
      x |= getNext()
      if x != 1634497381L then abort(Issue.ExpectedValue(false))
      next()
      false
    
    def parseTrue(): RawJson =
      var x: Int = current << 8
      x |= getNext()
      x <<= 8
      x |= getNext()
      if x != 7501157L then abort(Issue.ExpectedValue(true))
      next()
      true
    
    def parseNull(): RawJson =
      var x: Int = current << 8
      x |= getNext()
      x <<= 8
      x |= getNext()
      if x != 7695468L then abort(Issue.ExpectedValue(null))
      next()
      null

    def parseNumber(first: Long, negative: Boolean): JsonNum =
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
              if decimalPosition != 0 then abort(Issue.MultipleDecimalPoints)
              decimalPosition = cur
              next()
              ch = current
              if ch <= Num9 && ch >= Num0 then digit(decimalPosition != 0, ch)
              else abort(Issue.ExpectedDigit(ch))
              
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
              if ch <= Num9 && ch >= Num0 then exponent = (ch & 15) else abort(Issue.ExpectedDigit(ch))
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
              if decimalPosition != 0 && scale == 0 then scale = decimalPosition - cur + 1
              else if leadingZero && mantissa != 0 then abort(Issue.NumberHasLeadingZero)
              
              if negative then mantissa *= -1
              number = mantissa*math.pow(10, exponent + scale)

              continue = false
            
            case ch =>
              abort(Issue.UnexpectedChar(ch))
          end if
        end while
        number
      catch
        case err: ArrayIndexOutOfBoundsException =>
          cur -= 1
          if decimalPosition != 0 && scale == 0 then scale = decimalPosition - cur
          if current <= Num9 && current >= Num0 then
            next()
            (if negative then -mantissa else mantissa).toDouble*math.pow(10, exponent + scale)
          else abort(Issue.PrematureEnd)
    
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
            if decimalPoint then abort(Issue.MultipleDecimalPoints)
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
            number = 
              try
                val number = Text(numberText.toString)
                numberText.setLength(0)
                BigDecimal(number.s)
              catch
                case err: NumberFormatException => throw err
            
            continue = false
          
          case ch =>
            abort(Issue.UnexpectedChar(ch))
    
    skip()
    val result = parseValue()
    while cur < block.length
    do
      (current: @switch) match
        case Tab | Newline | Return | Space => ()
        case other =>
          abort(Issue.SpuriousContent(other))
      
      next()

    Json(result)

  catch
    case err: ArrayIndexOutOfBoundsException =>
      throw JsonParseError(stream.head.length, Issue.PrematureEnd)

