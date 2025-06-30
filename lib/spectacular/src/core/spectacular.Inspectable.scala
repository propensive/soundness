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
┃    Soundness, version 0.36.0.                                                                    ┃
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
package spectacular

import scala.collection.mutable as scm

import anticipation.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*
import wisteria.*

object Inspectable extends Inspectable2:
  object Derivation extends Derivable[Inspectable]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Inspectable =
      value =>
        fields(value):
          [field] => field =>
            val text = context.text(field)
            if tuple then text else s"$label:$text"

        . mkString(if tuple then "(" else s"$typeName(", " ╱ ", ")").tt

    inline def split[derivation: SumReflection]: derivation is Inspectable = value =>
      variant(value):
        [variant <: derivation] => variant =>
          context.give(variant.inspect)

  inline given derived: [value] => value is Inspectable = compiletime.summonFrom:
    case given (`value` is Encodable in Text) => _.encode
    case given (`value` is Showable)          => _.show
    case given Reflection[`value`]            => Derivation.derived[value].text(_)
    case _                                    => value => ("“"+value+"”").tt

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

  given set: [element: Inspectable] => Set[element] is Inspectable =
    _.map(_.inspect).mkString("{", ", ", "}").tt

  given vector: [element: Inspectable] => Trie[element] is Inspectable =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩").tt

  given indexedSeq: [element: Inspectable] => IndexedSeq[element] is Inspectable =
    _.map(_.inspect).mkString("⟨ ", " ", " ⟩ᵢ").tt

  given list: [element: Inspectable] => List[element] is Inspectable =
    _.map(_.inspect).mkString("[", ", ", "]").tt

  given array: [element: Inspectable] => Array[element] is Inspectable = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.inspect.s).tt

    . mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌").tt

  given arraySeq: [element: Inspectable] => scm.ArraySeq[element] is Inspectable = array =>
    array.zipWithIndex.map: (value, index) =>
      val subscript = index.toString.map { digit => (digit + 8272).toChar }.mkString
      (subscript+value.inspect.s).tt

    . mkString("⦋"+arrayPrefix(array.toString), "∣", "⦌ₛ").tt

  given stream: [element: Inspectable] => Stream[element] is Inspectable = stream =>
    def recur(stream: Stream[element], todo: Int): Text =
      if todo <= 0 then "..?".tt
      else if stream.toString == "Stream(<not computed>)" then "∿∿∿".tt
      else if stream.isEmpty then "⯁ ".tt
      else (stream.head.inspect.s+" ⋰ "+recur(stream.tail, todo - 1)).tt

    recur(stream, 3)

  given iarray: [element: Inspectable] => IArray[element] is Inspectable = iarray =>
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

  given option: [value: Inspectable] => Option[value] is Inspectable =
    case None        => "None".tt
    case Some(value) => s"Some(${value.inspect.s})".tt

  given none: None.type is Inspectable = none => "None".tt

trait Inspectable2:
  given optional: [inspectable: Inspectable] => Optional[inspectable] is Inspectable =
    _.let { value => s"⸂${inspectable.text(value)}⸃".tt }.or("⸄⸅".tt)

trait Inspectable extends Typeclass:
  def text(value: Self): Text
