package merino

import annotation.*
import gossamer.*
import rudiments.*

import stdouts.stdout

enum Json:
  case Number(value: Long | BigDecimal | Double)
  case JString(value: String)
  case JObject(values: (String, Json)*)
  case JArray(values: Json*)
  case True
  case False
  case Null

  override def toString(): String = this match
    case Number(value)    => value.toString
    case JString(value)   => t"\"${value}\"".s
    case JObject(values*) => values.map { (k, v) => t"\"$k\": $v" }.join(t"{ ", t", ", t" }").s
    case JArray(values*)  => values.map(_.show).join(t"[ ", t", ", t" ]").s
    case True             => "true"
    case False            => "false"
    case Null             => "null"

object AsciiByte:
  final val OpenBracket = '['.toByte
  final val CloseBracket = ']'.toByte
  final val OpenBrace = '{'.toByte
  final val CloseBrace = '}'.toByte
  final val Comma = ','.toByte
  final val Colon = ':'.toByte
  final val Quote = '"'.toByte
  final val Minus = '-'.toByte
  final val Plus = '+'.toByte
  final val Slash = '/'.toByte
  final val Period = '.'.toByte
  final val Backslash = '\\'.toByte
  final val Num0 = '0'.toByte
  final val Num1 = '1'.toByte
  final val Num2 = '2'.toByte
  final val Num3 = '3'.toByte
  final val Num4 = '4'.toByte
  final val Num5 = '5'.toByte
  final val Num6 = '6'.toByte
  final val Num7 = '7'.toByte
  final val Num8 = '8'.toByte
  final val Num9 = '9'.toByte
  final val LowerA = 'a'.toByte
  final val LowerB = 'b'.toByte
  final val LowerC = 'c'.toByte
  final val LowerD = 'd'.toByte
  final val LowerE = 'e'.toByte
  final val LowerF = 'f'.toByte
  final val LowerN = 'n'.toByte
  final val LowerL = 'l'.toByte
  final val LowerR = 'r'.toByte
  final val LowerS = 's'.toByte
  final val LowerT = 't'.toByte
  final val LowerU = 'u'.toByte
  final val UpperA = 'A'.toByte
  final val UpperB = 'B'.toByte
  final val UpperC = 'C'.toByte
  final val UpperD = 'D'.toByte
  final val UpperE = 'E'.toByte
  final val UpperF = 'F'.toByte
  final val Tab = '\t'.toByte
  final val Space = ' '.toByte
  final val Newline = '\n'.toByte
  final val Return = '\r'.toByte
import AsciiByte.*

object Flag:
  final val DecimalPoint = 1 << 0
  final val Exponent = 1 << 1
  final val Negative = 1 << 2
  final val NegativeExponent = 1 << 3
  final val LeadingZero = 1 << 4
  final val Large = 1 << 5
  final val Tail = 1 << 6
  final val Interminible = 1 << 7


case class ParseError(pos: Int, message: Text) extends Exception(message.s)

object Json:
  def parse(stream: DataStream): Json throws ParseError | StreamCutError = try
    val block: Bytes = stream.head
    val penultimate = block.length - 1
    var cur: Int = 0

    if penultimate > 2 && block(0) == -17 && block(1) == -69 && block(2) == -65 then cur = 3

    def current: Byte = block(cur)
    
    def next(): Unit = cur += 1

    def skip(): Unit =
      while
        (current: @switch) match
          case Space | Return | Newline | Tab => true
          case _                              => false
      do next()

    def abort(message: Text): Nothing = throw ParseError(cur, message)

    def parseValue(): Json =
      (current: @switch) match
        case Quote                                     => next(); parseString()
        case Minus                                     => next(); parseNumber(cur, true)
        case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 |
                 Num6 | Num7 | Num8 | Num9             => parseNumber(cur, false)
        case OpenBracket                               => next(); parseArray()
        case LowerF                                    => next(); parseFalse()
        case LowerN                                    => next(); parseNull()
        case LowerT                                    => next(); parseTrue()
        case OpenBrace                                 => next(); parseObject(Nil)
        case ch                                        => abort(t"expected a value but found '${ch.toChar}'")

    @tailrec
    def parseObject(items: List[(String, Json)]): Json.JObject =
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
                case CloseBrace =>
                  next()
                  Json.JObject(items*)
                case Comma =>
                  next()
                  skip()
                  parseObject(str.value -> value :: items)
                case ch  => abort(t"unexpected character: '${ch.toChar}'")
            case ch => abort(t"expected a colon but found '${ch.toChar}'")
        case CloseBrace =>
          if !items.isEmpty then abort(t"closing brace appears after comma")
          next()
          Json.JObject()
        case ch => abort(t"expected a string but found '${ch.toChar}'")

    @tailrec
    def parseArray(items: List[Json] = Nil): Json.JArray =
      skip()
      current match
        case CloseBracket =>
          if !items.isEmpty then abort(t"closing bracket appears after comma")
          next()
          Json.JArray(items*)
        case ch =>
          val value = parseValue()
          skip()
          current match
            case Comma        => next(); parseArray(value :: items)
            case CloseBracket => next(); Json.JArray((value :: items)*)
            case ch           => abort(t"expected ',' or ']' but found '${ch.toChar}'")

    def parseString(): Json.JString =
      val start = cur
      var continue = true
      var difference = 0
      var size = 0

      while continue do
        current match
          case Quote =>
            continue = false
            size = cur - start - difference
          
          case Tab | Newline | Return =>
            abort(t"invalid unescaped whitespace character in string")

          case Backslash =>
            next()
            current match
              case LowerU =>
                next(); next(); next(); next()
                difference += 5
              case ch =>
                difference += 1
          
          case ch =>
            // FIXME: Optimization opportunity by reversing and nesting these
            if (ch & 224) == 192 then
              difference += 1
              next()
            else if (ch & 240) == 224 then
              difference += 2
              next(); next()
            else if (ch & 248) == 240 then
              difference += 3
              next(); next(); next()

        next()

      val array: Array[Char] = new Array(size)

      continue = true
      cur = start

      var offset: Int = 0
      
      def append(char: Char): Unit =
        array(offset) = char
        offset += 1

      def parseUnicode(): Char =
        next()
        var acc = fromHex(current)*4096
        next()
        acc = acc + fromHex(current)*256
        next()
        acc = acc + fromHex(current)*16
        next()
        acc = acc + fromHex(current)
        acc.toChar

      while continue do
        current match
          case Quote =>
            next()
            continue = false
          
          case Backslash =>
            next()
            current match
              case Quote     => append('"')
              case Slash     => append('/')
              case Backslash => append('\\')
              case LowerB    => append('\b')
              case LowerF    => append('\f')
              case LowerN    => append('\n')
              case LowerR    => append('\r')
              case LowerT    => append('\t')
              case LowerU    => append(parseUnicode())
              case ch        => abort(t"the character '$ch' should not be escaped")
            next()

          case ch =>
            if ch >= 0 && ch < 32 then abort(t"unescaped control character '$ch'")
            // FIXME: Optimization opportunity by reversing and nesting these
            var char = 0
            if (ch & 224) == 192 then
              char = ((ch & 31) << 6)
              next()
              char += current & 63
              append(char.toChar)
            else if (ch & 240) == 224 then
              char = ((ch & 31) << 12)
              next()
              char += (current & 63) << 6
              next()
              char += current & 63
              append(char.toChar)
            else if (ch & 248) == 240 then
              char = ((ch & 31) << 18)
              next()
              char += (current & 63) << 12
              next()
              char += (current & 63) << 6
              next()
              char += current & 63
              append(char.toChar)
            
            next()
      
      Json.JString(String(array))
              
    def fromHex(byte: Byte): Byte = (byte: @switch) match
      case Num0  => 0
      case Num1  => 1
      case Num2  => 2
      case Num3  => 3
      case Num4  => 4
      case Num5  => 5
      case Num6  => 6
      case Num7  => 7
      case Num8  => 8
      case Num9  => 9
      case _   => (byte: @switch) match
        case UpperA  => 10
        case UpperB  => 11
        case UpperC  => 12
        case UpperD  => 13
        case UpperE  => 14
        case UpperF  => 15
        case _   => (byte: @switch) match
          case LowerA  => 10
          case LowerB => 11
          case LowerC => 12
          case LowerD => 13
          case LowerE => 14
          case LowerF => 15
          case _   => abort(t"expected a hexadecimal digit")

    def parseFalse(): Json.False.type =
      if current != LowerA then abort(t"expected 'false'")
      next()
      if current != LowerL then abort(t"expected 'false'")
      next()
      if current != LowerS then abort(t"expected 'false'")
      next()
      if current != LowerE then abort(t"expected 'false'")
      next()
      Json.False
    
    def parseTrue(): Json.True.type =
      if current != LowerR then abort(t"expected 'true'")
      next()
      if current != LowerU then abort(t"expected 'true'")
      next()
      if current != LowerE then abort(t"expected 'true'")
      next()
      Json.True
    
    def parseNull(): Json.Null.type =
      if current != LowerU then abort(t"expected 'null'")
      next()
      if current != LowerL then abort(t"expected 'null'")
      next()
      if current != LowerL then abort(t"expected 'null'")
      next()
      Json.Null

    def parseNumber(start: Int, negative: Boolean): Json.Number =
      import Flag.*
      var mantissa: Long = 0L
      lazy val bigDecimal: StringBuffer = StringBuffer()
      var fractional: Long = 0L
      var exponent: Long = 0L
      var continue: Boolean = true
      var hasExponent: Boolean = false
      var terminible: Boolean = false
      var decimalPoint: Boolean = false
      var divisor: Double = 1.0
      var leadingZero: Boolean = current == Num0
      var negativeExponent: Boolean = false
      var large: Boolean = false
      var result: Double | BigDecimal | Long = 0L
      
      if current == Period then abort(t"cannot start a number with a decimal point")

      while continue do
        (current: @switch) match
          case Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 =>
            if large then bigDecimal.append((current - 48).toChar)
            else if hasExponent then exponent = exponent*10 + current - 48
            else if decimalPoint then
              fractional = fractional*10 + current - 48
              divisor *= 10
            else
              val newMantissa = mantissa*10 + current - 48
              if newMantissa < mantissa then
                if negative then bigDecimal.append('-')
                bigDecimal.append(mantissa.toString)
                bigDecimal.append((current - 48).toChar)
                large = true
              else mantissa = newMantissa
            terminible = true
            if cur == penultimate then continue = false
            next()
          
          case Period =>
            if large then bigDecimal.append('.')
            if decimalPoint then abort(t"a number can have at most one decimal point")
            if hasExponent then abort(t"a decimal point cannot appear after an exponent")
            decimalPoint = true
            terminible = false
            next()
          
          case UpperE | LowerE =>
            if large then bigDecimal.append('e')
            if hasExponent then abort(t"a number can have at most one exponent")
            if !terminible then abort(t"a number's mantissa cannot end in a '.'")
            
            next()
            
            (current: @switch) match
              case Minus =>
                if large then bigDecimal.append('-')
                negativeExponent = true
                next()
              
              case Plus =>
                next()
              
              case _ =>
                ()
            
            hasExponent = true
            terminible = false
          
          case Tab | Return | Newline | Space | Comma | CloseBracket | CloseBrace =>
            if !terminible then abort(t"incomplete number")
            if leadingZero && mantissa != 0 then abort(t"number should not have a leading zero")
            
            if mantissa == 0 && !decimalPoint && hasExponent && exponent != 1
            then abort(t"zero integer should not have an exponent")
            
            result = 
              if large then BigDecimal(bigDecimal.toString)
              else if decimalPoint then (mantissa + fractional/divisor)
              else mantissa
            
            continue = false
          
          case ch =>
            abort(t"unexpected character: '$ch' at position $cur")
      
      Json.Number(result)
    
    skip()
    val result = parseValue()
    while cur < block.length
    do
      current match
        case Tab | Newline | Return | Space => ()
        case other =>
          abort(t"spurious extra characters at the end of the stream")
      
      next()

    result
  catch
    case err: ArrayIndexOutOfBoundsException => throw ParseError(stream.head.length, t"JSON was not properly terminated")