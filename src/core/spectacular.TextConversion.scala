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
import fulminate.*
import inimitable.*
import rudiments.*

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

  given [ValueType: Showable] => Option[ValueType] is Showable as option =
    _.fold("none".tt)(ValueType.text(_))

  given Uuid is Showable as uuid = _.text
  given Memory is Showable as memory = _.text
  given [EnumType <: reflect.Enum] => EnumType is Showable as reflectEnum = _.toString.tt

  given [ElemType: Showable] => Set[ElemType] is Showable as set =
    _.map(_.show).mkString("{", ", ", "}").tt

  given [ElemType: Showable] => List[ElemType] is Showable as list =
    _.map(_.show).mkString("[", ", ", "]").tt

  given [ElemType: Showable] => Trie[ElemType] is Showable as trie =
    _.map(_.show).mkString("[ ", " ", " ]").tt

  given None.type is Showable as none = none => "none".tt

trait TextConversion:
  type Self
  def text(value: Self): Text
