/*
    Spectacular, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import rudiments.*

import scala.deriving.*
import scala.compiletime.*

trait TextConversion[-ValueType]:
  def apply(value: ValueType): Text

trait Show[-ValueType] extends TextConversion[ValueType]
trait Debug[-ValueType] extends TextConversion[ValueType]

trait Showable[-ValueType]:
  def show(value: ValueType): Text

object Show:
  given showable[ValueType](using showable: Showable[ValueType]): Show[ValueType] = showable.show(_)

object TextConversion:
  val any: Debug[Any] = value => Text(value.toString)

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

  given Debug[Char] = char => Text("'"+escape(char).s+"'")
  given Debug[Long] = long => Text(long.toString+"L")
  given Debug[String] = string => Text(summon[Debug[Text]](Text(string)).s.substring(1).nn)
  given Debug[Byte] = byte => Text(byte.toString+".toByte")
  given Debug[Short] = short => Text(short.toString+".toShort")
  
  given Debug[Text] = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.foreach { char => builder.append(escape(char, true)) }
    
    Text("t\""+builder.toString+"\"")

  given Show[Text] = identity(_)
  given Show[String] = Text(_)
  given Show[Char] = char => Text(char.toString)
  given Show[Long] = long => Text(long.toString)
  given Show[Int] = int => Text(int.toString)
  given Show[Short] = short => Text(short.toString)
  given Show[Byte] = byte => Text(byte.toString)
  given (using decimalizer: DecimalConverter): Show[Double] = decimalizer.decimalize(_)
  
  given Debug[Float] =
    case Float.PositiveInfinity => Text("Float.PositiveInfinity")
    case Float.NegativeInfinity => Text("Float.NegativeInfinity")
    case float if float.isNaN   => Text("Float.NaN")
    case float                  => Text(float.toString+"F")
  
  given Debug[Double] = 
    case Double.PositiveInfinity => Text("Double.PositiveInfinity")
    case Double.NegativeInfinity => Text("Double.NegativeInfinity")
    case double if double.isNaN  => Text("Double.NaN")
    case double                  => Text(double.toString)

  given (using booleanStyle: BooleanStyle): Show[Boolean] = booleanStyle(_)
  given Debug[Boolean] = boolean => Text(if boolean then "true" else "false")

  given [ValueType](using show: Show[ValueType]): Show[Option[ValueType]] =
    case Some(value) => show(value)
    case None        => Text("none")
  
  given Show[Uuid] = _.text
  given Show[ByteSize] = _.text
  given Show[reflect.Enum] = _.toString.show
  given Debug[reflect.Enum] = _.toString.show
  given Debug[Pid] = pid => Text("[PID:"+pid.value+"]")

  given set[ElemType](using Show[ElemType]): Show[Set[ElemType]] = set =>
    Text(set.map(_.show).mkString("{", ", ", "}"))
  
  given list[ElemType](using Show[ElemType]): Show[List[ElemType]] = list =>
    Text(list.map(_.show).mkString("[", ", ", "]"))
  
  given vector[ElemType](using Show[ElemType]): Show[Vector[ElemType]] =
    vector => Text(vector.map(_.show).mkString("[ ", " ", " ]"))
  
  inline given set2[ElemType]: Debug[Set[ElemType]] =
    new Debug[Set[ElemType]]:
      def apply(set: Set[ElemType]): Text = Text(set.map(_.debug).mkString("{", ", ", "}"))
  
  inline given list2[ElemType]: Debug[List[ElemType]] =
    new Debug[List[ElemType]]:
      def apply(list: List[ElemType]): Text = Text(list.map(_.debug).mkString("[", ", ", "]"))
  
  inline given vector2[ElemType]: Debug[Vector[ElemType]] =
    new Debug[Vector[ElemType]]:
      def apply(vector: Vector[ElemType]): Text =
        Text(vector.map(_.debug).mkString("⟨ ", " ", " ⟩"))
  
  inline given array[ElemType]: Debug[Array[ElemType]] =
    new Debug[Array[ElemType]]:
      def apply(array: Array[ElemType]): Text = Text:
        array.zipWithIndex.map: (value, index) =>
          val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
          Text(subscript+value.debug.s)
        .mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌")
  
  inline given iarray[ElemType]: Debug[IArray[ElemType]] =
    new Debug[IArray[ElemType]]:
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

  inline given [ValueType]: Debug[Option[ValueType]] =
    case None =>
      Text("None")
    
    case Some(value) =>
      val valueText = compiletime.summonFrom:
        case display: Debug[ValueType] => display(value)
        case display: Show[ValueType]   => display(value)
        case _                                      => Text(value.toString)
      
      Text("Some("+valueText+")")
  
  given Show[None.type] = none => Text("none")
  given Debug[None.type] = none => Text("None")
  
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
      : Debug[DerivedType] =
    inline erasedValue[TupleType] match
      case _: (head *: tail) =>
        if ordinal == 0
        then summonInline[Debug[head]].asInstanceOf[Debug[DerivedType]]
        else deriveSum[tail, DerivedType](ordinal - 1)

  inline given derived[DerivationType](using mirror: Mirror.Of[DerivationType]): Debug[DerivationType] = inline mirror match
    case given Mirror.ProductOf[DerivationType & Product] => (value: DerivationType) => value.asMatchable match
      case value: Product =>
        val elements = deriveProduct[mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
        val typeName = Text(valueOf[mirror.MirroredLabel])
        Text(typeName.s+elements.mkString("(", "∣", ")"))
    
    case s: Mirror.SumOf[DerivationType] =>
      (value: DerivationType) => deriveSum[s.MirroredElemTypes, DerivationType](s.ordinal(value))(value)

extension [ValueType](value: ValueType)
  inline def show(using display: Show[ValueType]): Text = display(value)
  
  inline def debug: Text = compiletime.summonFrom:
    case display: Debug[ValueType] => display(value)
    case display: Show[ValueType]   => display(value)
    case _                                      => Text(value.toString)

case class BooleanStyle(yes: Text, no: Text):
  def apply(boolean: Boolean): Text = if boolean then yes else no

package booleanStyles:
  given yesNo: BooleanStyle = BooleanStyle(Text("yes"), Text("no"))
  given onOff: BooleanStyle = BooleanStyle(Text("on"), Text("off"))
  given trueFalse: BooleanStyle = BooleanStyle(Text("true"), Text("false"))
  given oneZero: BooleanStyle = BooleanStyle(Text("1"), Text("0"))
