package spectacular

import rudiments.*

import scala.deriving.*
import scala.compiletime.*

sealed erased trait Audience

sealed erased trait EndUser extends Audience
sealed erased trait Developer extends Audience
sealed erased trait Machine extends Audience

trait Display[-ValueType, +DisplayAudienceType <: Audience]:
  def apply(value: ValueType): Text

trait Show[-ValueType] extends Display[ValueType, Audience]

object Display:

  val any: Display[Any, Developer] = value => Text(value.toString)

  def escape(char: Char, eEscape: Boolean = false): Text = char match
    case '\n' => Text("\\n")
    case '\t' => Text("\\t")
    case '\r' => Text("\\r")
    case '\\' => Text("\\\\")
    case '\"' => Text("\\\"")
    case '\'' => Text("\\\'")
    case '\b' => Text("\\b")
    case '\f' => Text("\\f")
    
    case '\u001b' if eEscape =>
      Text("\\e")
    
    case ch =>
      Text(if ch < 128 && ch >= 32 then ch.toString else String.format("\\u%04x", ch.toInt).nn)

  given Display[Text, EndUser] = identity(_)
  given Display[String, EndUser] = Text(_)
  
  given Display[Text, Developer] = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.foreach { char => builder.append(escape(char, true)) }
    
    Text("t\""+builder.toString+"\"")
  
  given Display[String, Developer] = string =>
    Text(summon[Display[Text, Developer]](Text(string)).s.substring(1).nn)

  given Display[Char, Developer] = char => Text("'"+escape(char).s+"'")
  given Display[Char, EndUser] = char => Text(char.toString)

  given Display[Long, Developer] = long => Text(long.toString+"L")
  given Display[Long, EndUser] = long => Text(long.toString)
  
  given [AudienceType <: Audience]: Display[Int, AudienceType] = int => Text(int.toString)
  given [AudienceType <: Audience]: Display[Short, AudienceType] = short => Text(short.toString)
  
  given Display[Byte, EndUser] = byte => Text(byte.toString)
  given Display[Byte, Developer] = byte => Text(byte.toString+".toByte")
  
  given Display[Short, EndUser] = short => Text(short.toString)
  given Display[Short, Developer] = short => Text(short.toString+".toShort")
  
  given (using decimalizer: DecimalConverter): Display[Double, EndUser] = decimalizer.decimalize(_)
  
  given Display[Float, Developer] =
    case Float.PositiveInfinity => Text("Float.PositiveInfinity")
    case Float.NegativeInfinity => Text("Float.NegativeInfinity")
    case float if float.isNaN   => Text("Float.NaN")
    case float                  => Text(float.toString+"F")
  
  given Display[Double, Developer] = 
    case Double.PositiveInfinity => Text("Double.PositiveInfinity")
    case Double.NegativeInfinity => Text("Double.NegativeInfinity")
    case double if double.isNaN  => Text("Double.NaN")
    case double                  => Text(double.toString)

  given (using booleanStyle: BooleanStyle): Display[Boolean, EndUser] = booleanStyle(_)
  given Display[Boolean, Developer] = boolean => Text(if boolean then "true" else "false")

  given [ValueType](using show: Display[ValueType, EndUser]): Display[Option[ValueType], EndUser] =
    case Some(value) => show(value)
    case None        => Text("none")
  
  given Display[Uuid, EndUser] = _.javaUuid.toString.show
  given Display[ByteSize, EndUser] = _.text
  given Display[reflect.Enum, EndUser] = _.toString.show
  given Display[reflect.Enum, Developer] = _.toString.show
  given Display[Pid, Developer] = pid => Text("[PID:"+pid.value+"]")

  given set[ElemType](using Display[ElemType, EndUser]): Display[Set[ElemType], EndUser] = set =>
    Text(set.map(_.show).mkString("{", ", ", "}"))
  
  given list[ElemType](using Display[ElemType, EndUser]): Display[List[ElemType], EndUser] = list =>
    Text(list.map(_.show).mkString("[", ", ", "]"))
  
  given vector[ElemType](using Display[ElemType, EndUser]): Display[Vector[ElemType], EndUser] =
    vector => Text(vector.map(_.show).mkString("[ ", " ", " ]"))
  
  inline given set2[ElemType]: Display[Set[ElemType], Developer] =
    new Display[Set[ElemType], Developer]:
      def apply(set: Set[ElemType]): Text = Text(set.map(_.debug).mkString("{", ", ", "}"))
  
  inline given list2[ElemType]: Display[List[ElemType], Developer] =
    new Display[List[ElemType], Developer]:
      def apply(list: List[ElemType]): Text = Text(list.map(_.debug).mkString("[", ", ", "]"))
  
  inline given vector2[ElemType]: Display[Vector[ElemType], Developer] =
    new Display[Vector[ElemType], Developer]:
      def apply(vector: Vector[ElemType]): Text =
        Text(vector.map(_.debug).mkString("⟨ ", " ", " ⟩"))
  
  inline given array[ElemType]: Display[Array[ElemType], Developer] =
    new Display[Array[ElemType], Developer]:
      def apply(array: Array[ElemType]): Text = Text:
        array.zipWithIndex.map: (value, index) =>
          val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
          Text(subscript+value.debug.s)
        .mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌")
  
  inline given iarray[ElemType]: Display[IArray[ElemType], Developer] =
    new Display[IArray[ElemType], Developer]:
      def apply(iarray: IArray[ElemType]): Text = Text:
        iarray.zipWithIndex.map: (value, index) =>
          val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
          Text(subscript+value.debug.s)
        .mkString("⁅"+arrayPrefix(iarray.toString), "∣", "⁆")
  
  private def renderBraille(str: String): String =
    ("0"*(str.length%2)+str).grouped(2).flatMap: pair =>
      (16*pair(0) + pair(1) - 39*(16*(pair(0)/48) + (pair(1)/48)) + 10087).toChar.toString
    .mkString
  
  private def arrayPrefix(str: String): String =
    val dimension = str.count(_ == '[').min(9)
    
    val arrayType = str(dimension) match
      case 'B' => "𝔹" // Byte
      case 'C' => "ℂ" // Char
      case 'D' => "𝔻" // Double
      case 'F' => "𝔽" // Float
      case 'I' => "𝕀" // Int
      case 'J' => "𝕁" // Long
      case 'L' => "𝕃" // Object
      case 'S' => "𝕊" // Short
      case 'Z' => "ℤ" // Boolean
      case _   => "" // Unknown
    
    arrayType+("⁰¹²³⁴⁵⁶⁷⁸⁹"(dimension))+"¦"+renderBraille(str.split("@").nn(1).nn)+"¦"

  inline given [ValueType]: Display[Option[ValueType], Developer] =
    case None =>
      Text("None")
    
    case Some(value) =>
      val valueText = compiletime.summonFrom:
        case display: Display[ValueType, Developer] => display(value)
        case display: Display[ValueType, EndUser]   => display(value)
        case _                                      => Text(value.toString)
      
      Text("Some("+valueText+")")
  
  given Display[None.type, EndUser] = none => Text("none")
  given Display[None.type, Developer] = none => Text("None")
  
  private transparent inline def deriveProduct[Labels <: Tuple](tuple: Tuple): List[Text] =
    inline tuple match
      case EmptyTuple => Nil
      case cons: (? *: ?) => cons match
        case head *: tail => inline erasedValue[Labels] match
          case _: (headLabel *: tailLabels) => inline valueOf[headLabel].asMatchable match
            case label: String =>
              val value = head.debug
              Text(label+"="+value) :: deriveProduct[tailLabels](tail)

  private transparent inline def deriveSum
      [TupleType <: Tuple, DerivedType]
      (ordinal: Int)
      : Display[DerivedType, Developer] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) =>
        if ordinal == 0
        then summonInline[Display[head, Developer]].asInstanceOf[Display[DerivedType, Developer]]
        else deriveSum[tail, DerivedType](ordinal - 1)

  inline given derived[DerivationType](using mirror: Mirror.Of[DerivationType]): Display[DerivationType, Developer] = inline mirror match
    case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) => value.asMatchable match
      case value: Product =>
        val elements = deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
        val typeName = Text(valueOf[mirror.MirroredLabel])
        Text(typeName.s+elements.mkString("(", "∣", ")"))
    
    case s: Mirror.SumOf[DerivationType] =>
      (value: DerivationType) => deriveSum[s.MirroredElemTypes, DerivationType](s.ordinal(value))(value)

extension [ValueType](value: ValueType)
  inline def show(using display: Display[ValueType, EndUser]): Text = display(value)
  
  inline def debug: Text = compiletime.summonFrom:
    case display: Display[ValueType, Developer] => display(value)
    case display: Display[ValueType, EndUser]   => display(value)
    case _                                      => Text(value.toString)

case class BooleanStyle(yes: Text, no: Text):
  def apply(boolean: Boolean): Text = if boolean then yes else no

package booleanStyles:
  given yesNo: BooleanStyle = BooleanStyle(Text("yes"), Text("no"))
  given onOff: BooleanStyle = BooleanStyle(Text("on"), Text("off"))
  given trueFalse: BooleanStyle = BooleanStyle(Text("true"), Text("false"))
  given oneZero: BooleanStyle = BooleanStyle(Text("1"), Text("0"))
