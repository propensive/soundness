/*
    Spectacular, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import inimitable.*
import vacuous.*
import wisteria.*, derivationContext.relaxed
import anticipation.*
import fulminate.*

import scala.deriving.*

import language.experimental.captureChecking

trait TextConversion[-ValueType]:
  def apply(value: ValueType): Text

trait Show[-ValueType] extends TextConversion[ValueType]
trait Debug[-ValueType] extends TextConversion[ValueType]

object Show:
  given specializable: Show[Specializable] = value =>
    value.getClass.nn.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt

object Debug:
  inline given derived[ValueType]: Debug[ValueType] = new Debug[ValueType]:
    def apply(value: ValueType): Text = compiletime.summonFrom:
      case encoder: Encoder[ValueType] => encoder.encode(value)
      case given Reflection[ValueType] => DebugDerivation.derived[ValueType](value)
      case given Show[ValueType]       => value.show
      case _                           => s"⧛${value.toString.tt}⧚".tt

  given debugChar: Debug[Char] = char => ("'"+escape(char).s+"'").tt
  given debugLong: Debug[Long] = long => (long.toString+"L").tt
  given debugString: Debug[String] = string => debugText(string.tt).s.substring(1).nn.tt
  given debugByte: Debug[Byte] = byte => (byte.toString+".toByte").tt
  given debugShort: Debug[Short] = short => (short.toString+".toShort").tt
  
  given debugText: Debug[Text] = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.map(escape(_, true)).each(builder.append)
    
    ("t\""+builder.toString+"\"").tt
  
  given debugFloat: Debug[Float] =
    case Float.PositiveInfinity => "Float.PositiveInfinity".tt
    case Float.NegativeInfinity => "Float.NegativeInfinity".tt
    case float if float.isNaN   => "Float.NaN".tt
    case float                  => (float.toString+"F").tt
  
  given debugDouble: Debug[Double] = 
    case Double.PositiveInfinity => "Double.PositiveInfinity".tt
    case Double.NegativeInfinity => "Double.NegativeInfinity".tt
    case double if double.isNaN  => "Double.NaN".tt
    case double                  => double.toString.tt

  given debugBoolean: Debug[Boolean] = boolean => if boolean then "true".tt else "false".tt
  given debugReflectEnum: Debug[reflect.Enum] = _.toString.show
  given debugPid: Debug[Pid] = pid => s"[PID:${pid.value}]".tt

  def escape(char: Char, eEscape: Boolean = false): Text = char match
    case '\n' => "\\n".tt
    case '\t' => "\\t".tt
    case '\r' => "\\r".tt
    case '\\' => "\\\\".tt
    case '\"' => "\\\"".tt
    case '\'' => "\\\'".tt
    case '\b' => "\\b".tt
    case '\f' => "\\f".tt
    
    case '\u001b' if eEscape =>
      "\\e".tt
    
    case ch =>
      if ch < 128 && ch >= 32 then ch.toString.tt else String.format("\\u%04x", ch.toInt).nn.tt

  given set[ElemType: Debug]: Debug[Set[ElemType]] = _.map(_.debug).mkString("{", ", ", "}").tt
  given list[ElemType: Debug]: Debug[List[ElemType]] = _.map(_.debug).mkString("[", ", ", "]").tt
  given vector[ElemType: Debug]: Debug[Vector[ElemType]] = _.map(_.debug).mkString("⟨ ", " ", " ⟩").tt
  given indexedSeq[ElemType: Debug]: Debug[IndexedSeq[ElemType]] = _.map(_.debug).mkString("⟨ ", " ", " ⟩").tt
  
  given array[ElemType: Debug]: Debug[Array[ElemType]] = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.debug.s).tt
    .mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌").tt
  
  given lazyList[ElemType: Debug]: Debug[LazyList[ElemType]] = lazyList =>
    def recur(lazyList: LazyList[ElemType], todo: Int): Text =
      if todo <= 0 then "..?".tt
      else if lazyList.toString == "LazyList(<not computed>)" then "∿∿∿".tt
      else if lazyList.isEmpty then "⯁ ".tt
      else (lazyList.head.debug.s+" ⋰ "+recur(lazyList.tail, todo - 1)).tt
    
    recur(lazyList, 3)

  given iarray[ElemType: Debug]: Debug[IArray[ElemType]] = iarray =>
    iarray.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      subscript+value.debug.s.tt
    .mkString("⁅"+arrayPrefix(iarray.toString), "╱", "⁆").tt
  
  private def renderBraille(str: String): String =
    ("0"*(str.length%2)+str).grouped(2).flatMap: pair =>
      (16*pair(0) + pair(1) - 39*(16*(pair(0)/48) + (pair(1)/48)) + 10087).toChar.toString
    .mkString
  
  private def arrayPrefix(str: String): String =
    val brackets = str.count(_ == '[')
    
    val arrayType = str(brackets) match
      case 'B' => "𝔹" // Byte
      case 'C' => "ℂ" // Char
      case 'D' => "𝔻" // Double
      case 'F' => "𝔽" // Float
      case 'I' => "𝕀" // Int
      case 'J' => "𝕁" // Long
      case 'L' => "𝕃" // Object
      case 'S' => "𝕊" // Short
      case 'Z' => "ℤ" // Boolean
      case _   => ""  // Unknown
    
    val dimension = if brackets < 2 then "".tt else brackets.toString.map("⁰¹²³⁴⁵⁶⁷⁸⁹"(_)).tt
    
    arrayType+dimension+"¦"+renderBraille(str.split("@").nn(1).nn)+"¦"

  inline given option[ValueType]: Debug[Option[ValueType]] =
    case None        => "None".tt
    case Some(value) => s"Some(${value.debug.s})".tt
  
  given none: Debug[None.type] = none => "None".tt
  
object DebugDerivation extends Derivation[Debug]:
  inline def join[DerivationType <: Product: ProductReflection]: Debug[DerivationType] = value =>
    fields(value):
      [FieldType] => field =>
        val text = context.let(_.contextually(field.debug)).or(field.debug)
        if tuple then text else s"$label:$text"
    .mkString(if tuple then "(" else s"$typeName(", " ╱ ", ")").tt

  inline def split[DerivationType: SumReflection]: Debug[DerivationType] = value =>
    variant(value):
      [VariantType <: DerivationType] => variant =>
        context.let(_.contextually(variant.debug)).or(variant.debug)

object TextConversion:
  given textualize[ValueType](using textualizer: Textualizer[ValueType]): Show[ValueType] =
    textualizer.textual(_)

  given text: Show[Text] = identity(_)
  given string: Show[String] = _.tt
  given char: Show[Char] = char => char.toString.tt
  given long: Show[Long] = long => long.toString.tt
  given int: Show[Int] = int => int.toString.tt
  given short: Show[Short] = short => short.toString.tt
  given byte: Show[Byte] = byte => byte.toString.tt
  given message: Show[Message] = _.text
  given double(using decimalizer: DecimalConverter): Show[Double] = decimalizer.decimalize(_)
  given pid: Show[Pid] = pid => ("\u21af"+pid.value).tt
  given boolean(using booleanStyle: BooleanStyle): Show[Boolean] = booleanStyle(_)

  given option[ValueType](using show: Show[ValueType]): Show[Option[ValueType]] =
    case Some(value) => show(value)
    case None        => "none".tt
  
  given uuid: Show[Uuid] = _.text
  given byteSize: Show[ByteSize] = _.text
  given reflectEnum: Show[reflect.Enum] = _.toString.show

  given set[ElemType](using Show[ElemType]): Show[Set[ElemType]] = set =>
    set.map(_.show).mkString("{", ", ", "}").tt
  
  given list[ElemType](using Show[ElemType]): Show[List[ElemType]] = list =>
    list.map(_.show).mkString("[", ", ", "]").tt
  
  given vector[ElemType](using Show[ElemType]): Show[Vector[ElemType]] =
    vector => vector.map(_.show).mkString("[ ", " ", " ]").tt
  
  given none: Show[None.type] = none => "none".tt
  
extension [ValueType](value: ValueType)
  inline def show(using display: Show[ValueType]): Text = display(value)
  inline def debug(using debug: Debug[ValueType]): Text = debug(value)

case class BooleanStyle(yes: Text, no: Text):
  def apply(boolean: Boolean): Text = if boolean then yes else no

package booleanStyles:
  given yesNo: BooleanStyle = BooleanStyle("yes".tt, "no".tt)
  given onOff: BooleanStyle = BooleanStyle("on".tt, "off".tt)
  given trueFalse: BooleanStyle = BooleanStyle("true".tt, "false".tt)
  given oneZero: BooleanStyle = BooleanStyle("1".tt, "0".tt)
