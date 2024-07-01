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
import wisteria.*
import anticipation.*
import digression.*
import fulminate.*

import scala.deriving.*

import language.experimental.captureChecking

trait TextConversion:
  type Self
  def text(value: Self): Text

trait Showable extends TextConversion:
  type Self

trait Debug[ValueType] extends TextConversion:
  type Self = ValueType

object Showable:
  given Specializable is Showable as specializable = value =>
    value.getClass.nn.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt

  given StackTrace is Showable = stack =>
    val methodWidth = stack.frames.map(_.method.method.s.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.s.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.s.length).maxOption.getOrElse(0)

    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt

    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.className.s.endsWith("#")
        val drop = if obj then 1 else 0
        val file = (" "*(fileWidth - frame.file.s.length))+frame.file
        val dot = if obj then ".".tt else "#".tt
        val className = frame.method.className.s.dropRight(drop)
        val classPad = (" "*(classWidth - className.length)).tt
        val method = frame.method.method
        val methodPad = (" "*(methodWidth - method.s.length)).tt
        val line = frame.line.let(_.show).or("?".tt)

        s"$msg\n  at $classPad$className$dot$method$methodPad $file:$line".tt

    stack.cause.lay(root): cause =>
      s"$root\ncaused by:\n$cause".tt

object Debug:
  inline given derived[ValueType]: Debug[ValueType] = compiletime.summonFrom:
    case encoder: Encoder[ValueType]   => encoder.encode(_)
    case given Reflection[ValueType]   => DebugDerivation.derived[ValueType].text(_)
    case given (ValueType is Showable) => _.show
    case _                             => value => s"⸉${value.toString.tt}⸊".tt

  given char: Debug[Char] = char => ("'"+escape(char).s+"'").tt
  given long: Debug[Long] = long => (long.toString+"L").tt
  given string: Debug[String] = string => text.text(string.tt).s.substring(1).nn.tt
  given byte: Debug[Byte] = byte => (byte.toString+".toByte").tt
  given short: Debug[Short] = short => (short.toString+".toShort").tt

  given text: Debug[Text] = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.map(escape(_, true)).each(builder.append)

    ("t\""+builder.toString+"\"").tt

  given float: Debug[Float] =
    case Float.PositiveInfinity => "Float.PositiveInfinity".tt
    case Float.NegativeInfinity => "Float.NegativeInfinity".tt
    case float if float.isNaN   => "Float.NaN".tt
    case float                  => (float.toString+"F").tt

  given double: Debug[Double] =
    case Double.PositiveInfinity => "Double.PositiveInfinity".tt
    case Double.NegativeInfinity => "Double.NegativeInfinity".tt
    case double if double.isNaN  => "Double.NaN".tt
    case double                  => double.toString.tt

  given boolean: Debug[Boolean] = boolean => if boolean then "true".tt else "false".tt
  given reflectEnum: Debug[reflect.Enum] = _.toString.show
  given pid: Debug[Pid] = pid => s"[PID:${pid.value}]".tt

  def escape(char: Char, eEscape: Boolean = false): Text = char match
    case '\n'                => "\\n".tt
    case '\t'                => "\\t".tt
    case '\r'                => "\\r".tt
    case '\\'                => "\\\\".tt
    case '\"'                => "\\\"".tt
    case '\''                => "\\\'".tt
    case '\b'                => "\\b".tt
    case '\f'                => "\\f".tt
    case '\u001b' if eEscape => "\\e".tt

    case char =>
      if char < 128 && char >= 32 then char.toString.tt else String.format("\\u%04x", char.toInt).nn.tt

  given set[ElemType: Debug]: Debug[Set[ElemType]] = _.map(_.inspect).mkString("{", ", ", "}").tt
  given vector[ElemType: Debug]: Debug[Vector[ElemType]] = _.map(_.inspect).mkString("⟨ ", " ", " ⟩").tt
  given indexedSeq[ElemType: Debug]: Debug[IndexedSeq[ElemType]] = _.map(_.inspect).mkString("⟨ ", " ", " ⟩ᵢ").tt
  given iterable[ElemType: Debug]: Debug[Iterable[ElemType]] = _.map(_.inspect).mkString("⦗", ", ", "⦘").tt
  given list[ElemType: Debug]: Debug[List[ElemType]] = _.map(_.inspect).mkString("[", ", ", "]").tt

  given array[ElemType: Debug]: Debug[Array[ElemType]] = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.inspect.s).tt
    .mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌").tt

  given lazyList[ElemType: Debug]: Debug[LazyList[ElemType]] = lazyList =>
    def recur(lazyList: LazyList[ElemType], todo: Int): Text =
      if todo <= 0 then "..?".tt
      else if lazyList.toString == "LazyList(<not computed>)" then "∿∿∿".tt
      else if lazyList.isEmpty then "⯁ ".tt
      else (lazyList.head.inspect.s+" ⋰ "+recur(lazyList.tail, todo - 1)).tt

    recur(lazyList, 3)

  given iarray[ElemType: Debug]: Debug[IArray[ElemType]] = iarray =>
    iarray.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      subscript+value.inspect.s.tt
    .mkString(arrayPrefix(iarray.toString)+"⁅", "╱", "⁆").tt

  private def arrayPrefix(str: String): String =
    val brackets = str.count(_ == '[')

    val arrayType = str(brackets) match
      case 'B' => "🅱" // Byte
      case 'C' => "🅲" // Char
      case 'D' => "🅳" // Double
      case 'F' => "🅵" // Float
      case 'I' => "🅸" // Int
      case 'J' => "🅹" // Long
      case 'L' => "🅻" // Object
      case 'S' => "🆂" // Short
      case 'Z' => "🆉" // Boolean
      case _   => "🯄" // Unknown

    val dimension = if brackets < 2 then "".tt else brackets.toString.map("⁰¹²³⁴⁵⁶⁷⁸⁹"(_)).tt

    arrayType+dimension//+renderBraille(str.split("@").nn(1).nn)

  given option[ValueType: Debug]: Debug[Option[ValueType]] =
    case None        => "None".tt
    case Some(value) => s"Some(${value.inspect.s})".tt

  given none: Debug[None.type] = none => "None".tt

  given optional[ValueType](using debug: Debug[ValueType]): Debug[Optional[ValueType]] =
    _.let { value => s"⸂${debug.text(value)}⸃".tt }.or("⸄⸅".tt)

object DebugDerivation extends Derivation[Debug]:
  inline def join[DerivationType <: Product: ProductReflection]: Debug[DerivationType] = value =>
    fields(value):
      [FieldType] => field =>
        val text = context.text(field)
        if tuple then text else s"$label:$text"
    .mkString(if tuple then "(" else s"$typeName(", " ╱ ", ")").tt

  inline def split[DerivationType: SumReflection]: Debug[DerivationType] = value =>
    variant(value):
      [VariantType <: DerivationType] => variant =>
        context.let(_.give(variant.inspect)).or(variant.inspect)

object TextConversion:
  given [ValueType: Textualizer] => ValueType is Showable = ValueType.textual(_)

  given Text is Showable as text = identity(_)
  given String is Showable as string = _.tt
  given Char is Showable as char = char => char.toString.tt
  given Long is Showable as long = long => long.toString.tt
  given Int is Showable as int = int => int.toString.tt
  given Short is Showable as short = short => short.toString.tt
  given Byte is Showable as byte = byte => byte.toString.tt
  given Message is Showable as message = _.text
  given (using decimalizer: DecimalConverter) => Double is Showable as double = decimalizer.decimalize(_)
  given Pid is Showable as pid = pid => ("\u21af"+pid.value).tt
  given (using booleanStyle: BooleanStyle) => Boolean is Showable as boolean = booleanStyle(_)
  given [ValueType: Showable] => Option[ValueType] is Showable as option = _.fold("none".tt)(ValueType.text(_))
  given Uuid is Showable as uuid = _.text
  given ByteSize is Showable as byteSize = _.text
  given [EnumType <: reflect.Enum] => EnumType is Showable as reflectEnum = _.toString.show
  given [ElemType: Showable] => Set[ElemType] is Showable as set = _.map(_.show).mkString("{", ", ", "}").tt
  given [ElemType: Showable] => List[ElemType] is Showable as list = _.map(_.show).mkString("[", ", ", "]").tt

  given [ElemType: Showable] => Vector[ElemType] is Showable as vector =
    _.map(_.show).mkString("[ ", " ", " ]").tt

  given None.type is Showable as none = none => "none".tt

extension [ValueType: Showable](value: ValueType)
  def show: Text = ValueType.text(value)

extension [ValueType](value: ValueType)
  def inspect(using debug: Debug[ValueType]): Text = debug.text(value)

case class BooleanStyle(yes: Text, no: Text):
  def apply(boolean: Boolean): Text = if boolean then yes else no

package booleanStyles:
  given yesNo: BooleanStyle = BooleanStyle("yes".tt, "no".tt)
  given onOff: BooleanStyle = BooleanStyle("on".tt, "off".tt)
  given trueFalse: BooleanStyle = BooleanStyle("true".tt, "false".tt)
  given oneZero: BooleanStyle = BooleanStyle("1".tt, "0".tt)
