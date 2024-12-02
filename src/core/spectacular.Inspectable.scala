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
import vacuous.*
import wisteria.*
import anticipation.*
import prepositional.*

import scala.deriving.*

import language.experimental.captureChecking

object Inspectable extends Inspectable2:
  inline given [ValueType] => ValueType is Inspectable as derived = compiletime.summonFrom:
    case given (ValueType is Encodable in Text) => _.encode
    case given Reflection[ValueType]            => InspectableDerivation.derived[ValueType].text(_)
    case given (ValueType is Showable)          => _.show
    case _                                      => value => s"⸉${value.toString.tt}⸊".tt

  given Char is Inspectable as char = char => ("'"+escape(char).s+"'").tt
  given Long is Inspectable as long = long => (long.toString+"L").tt
  given String is Inspectable as string = string => text.text(string.tt).s.substring(1).nn.tt
  given Byte is Inspectable as byte = byte => (byte.toString+".toByte").tt
  given Short is Inspectable as short = short => (short.toString+".toShort").tt

  given Text is Inspectable as text = text =>
    val builder: StringBuilder = new StringBuilder()
    text.s.map(escape(_, true)).each(builder.append)

    ("t\""+builder.toString+"\"").tt

  given Float is Inspectable as float =
    case Float.PositiveInfinity => "Float.PositiveInfinity".tt
    case Float.NegativeInfinity => "Float.NegativeInfinity".tt
    case float if float.isNaN   => "Float.NaN".tt
    case float                  => (float.toString+"F").tt

  given Double is Inspectable as double =
    case Double.PositiveInfinity => "Double.PositiveInfinity".tt
    case Double.NegativeInfinity => "Double.NegativeInfinity".tt
    case double if double.isNaN  => "Double.NaN".tt
    case double                  => double.toString.tt

  given Boolean is Inspectable as boolean = boolean => if boolean then "true".tt else "false".tt
  given reflect.Enum is Inspectable as reflectEnum = _.toString.show
  given Pid is Inspectable as pid = pid => s"[PID:${pid.value}]".tt

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

  given [ElemType: Inspectable] => Set[ElemType] is Inspectable as set =
    _.map(_.inspect).mkString("{", ", ", "}").tt

  given [ElemType: Inspectable] => Trie[ElemType] is Inspectable as vector =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩").tt

  given [ElemType: Inspectable] => IndexedSeq[ElemType] is Inspectable as indexedSeq =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩ᵢ").tt

  given [ElemType: Inspectable] => Iterable[ElemType] is Inspectable as iterable =
    _.map(_.inspect).mkString("⦗", ", ", "⦘").tt

  given [ElemType: Inspectable] => List[ElemType] is Inspectable as list =
    _.map(_.inspect).mkString("[", ", ", "]").tt

  given [ElemType: Inspectable] => Array[ElemType] is Inspectable as array = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.inspect.s).tt
    .mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌").tt

  given [ElemType: Inspectable] => LazyList[ElemType] is Inspectable as lazyList = lazyList =>
    def recur(lazyList: LazyList[ElemType], todo: Int): Text =
      if todo <= 0 then "..?".tt
      else if lazyList.toString == "LazyList(<not computed>)" then "∿∿∿".tt
      else if lazyList.isEmpty then "⯁ ".tt
      else (lazyList.head.inspect.s+" ⋰ "+recur(lazyList.tail, todo - 1)).tt

    recur(lazyList, 3)

  given [ElemType: Inspectable] => IArray[ElemType] is Inspectable as iarray = iarray =>
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

  given [ValueType: Inspectable] => Option[ValueType] is Inspectable as option =
    case None        => "None".tt
    case Some(value) => s"Some(${value.inspect.s})".tt

  given None.type is Inspectable = none => "None".tt

trait Inspectable2:
  given [ValueType: Inspectable] => Optional[ValueType] is Inspectable as optional =
    _.let { value => s"⸂${ValueType.text(value)}⸃".tt }.or("⸄⸅".tt)

trait Inspectable extends TextConversion:
  type Self
