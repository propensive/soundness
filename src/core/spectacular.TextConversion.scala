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

  given Text is Showable = identity(_)
  given String is Showable = _.tt
  given Char is Showable = char => char.toString.tt
  given Long is Showable = long => long.toString.tt
  given Int is Showable = int => int.toString.tt
  given Short is Showable = short => short.toString.tt
  given Byte is Showable = byte => byte.toString.tt
  given Message is Showable = _.text
  given double: (decimalizer: DecimalConverter) => Double is Showable = decimalizer.decimalize(_)
  given pid: Pid is Showable = pid => ("\u21af"+pid.value).tt
  given boolean: (booleanStyle: BooleanStyle) => Boolean is Showable = booleanStyle(_)

  given [ValueType: Showable] => Option[ValueType] is Showable =
    _.fold("none".tt)(ValueType.text(_))

  given Uuid is Showable = _.text
  given Memory is Showable = _.text
  given [EnumType <: reflect.Enum] => EnumType is Showable = _.toString.tt

  given [ElemType: Showable] => Set[ElemType] is Showable =
    _.map(_.show).mkString("{", ", ", "}").tt

  given [ElemType: Showable] => List[ElemType] is Showable =
    _.map(_.show).mkString("[", ", ", "]").tt

  given [ElemType: Showable] => Trie[ElemType] is Showable =
    _.map(_.show).mkString("[ ", " ", " ]").tt

  given None.type is Showable = none => "none".tt

trait TextConversion:
  type Self
  def text(value: Self): Text
