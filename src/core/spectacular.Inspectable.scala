/*
    Spectacular, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import prepositional.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

object Inspectable extends Inspectable2:
  inline given derived: [ValueType] => ValueType is Inspectable = compiletime.summonFrom:
    case given (ValueType is Encodable in Text) => _.encode
    case given Reflection[ValueType]            => InspectableDerivation.derived[ValueType].text(_)
    case given (ValueType is Showable)          => _.show
    case _                                      => value => s"⸉${value.toString.tt}⸊".tt

  given char: Char is Inspectable = char => ("'"+escape(char).s+"'").tt
  given long: Long is Inspectable = long => (long.toString+"L").tt
  given string: String is Inspectable = string => text.text(string.tt).s.substring(1).nn.tt
  given byte: Byte is Inspectable = byte => (byte.toString+".toByte").tt
  given short: Short is Inspectable = short => (short.toString+".toShort").tt

  given text: Text is Inspectable = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.map(escape(_, true)).each(builder.append)

    ("t\""+builder.toString+"\"").tt

  given float: Float is Inspectable =
    case Float.PositiveInfinity => "Float.PositiveInfinity".tt
    case Float.NegativeInfinity => "Float.NegativeInfinity".tt
    case float if float.isNaN   => "Float.NaN".tt
    case float                  => (float.toString+"F").tt

  given double: Double is Inspectable =
    case Double.PositiveInfinity => "Double.PositiveInfinity".tt
    case Double.NegativeInfinity => "Double.NegativeInfinity".tt
    case double if double.isNaN  => "Double.NaN".tt
    case double                  => double.toString.tt

  given boolean: Boolean is Inspectable = boolean => if boolean then "true".tt else "false".tt
  given reflectEnum: reflect.Enum is Inspectable = _.toString.show
  given pid: Pid is Inspectable = pid => s"[PID:${pid.value}]".tt

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
      if char < 128 && char >= 32
      then char.toString.tt else String.format("\\u%04x", char.toInt).nn.tt

  given set: [ElemType: Inspectable] => Set[ElemType] is Inspectable =
    _.map(_.inspect).mkString("{", ", ", "}").tt

  given vector: [ElemType: Inspectable] => Trie[ElemType] is Inspectable =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩").tt

  given indexedSeq: [ElemType: Inspectable] => IndexedSeq[ElemType] is Inspectable =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩ᵢ").tt

  given iterable: [ElemType: Inspectable] => Iterable[ElemType] is Inspectable =
    _.map(_.inspect).mkString("⦗", ", ", "⦘").tt

  given list: [ElemType: Inspectable] => List[ElemType] is Inspectable =
    _.map(_.inspect).mkString("[", ", ", "]").tt

  given array: [ElemType: Inspectable] => Array[ElemType] is Inspectable = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.inspect.s).tt

    . mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌").tt

  given lazyList: [ElemType: Inspectable] => LazyList[ElemType] is Inspectable = lazyList =>
    def recur(lazyList: LazyList[ElemType], todo: Int): Text =
      if todo <= 0 then "..?".tt
      else if lazyList.toString == "LazyList(<not computed>)" then "∿∿∿".tt
      else if lazyList.isEmpty then "⯁ ".tt
      else (lazyList.head.inspect.s+" ⋰ "+recur(lazyList.tail, todo - 1)).tt

    recur(lazyList, 3)

  given iarray: [ElemType: Inspectable] => IArray[ElemType] is Inspectable = iarray =>
    iarray.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      subscript+value.inspect.s.tt

    . mkString(arrayPrefix(iarray.toString)+"⁅", "╱", "⁆").tt

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

  given option: [ValueType: Inspectable] => Option[ValueType] is Inspectable =
    case None        => "None".tt
    case Some(value) => s"Some(${value.inspect.s})".tt

  given None.type is Inspectable = none => "None".tt

trait Inspectable2:
  given optional: [ValueType: Inspectable] => Optional[ValueType] is Inspectable =
    _.let { value => s"⸂${ValueType.text(value)}⸃".tt }.or("⸄⸅".tt)

trait Inspectable extends TextConversion:
  type Self
