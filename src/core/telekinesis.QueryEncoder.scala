/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*
import wisteria.*

import language.dynamics

object QueryEncoder extends ProductDerivation[QueryEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: QueryEncoder[DerivationType] =
    fields(_):
      [FieldType] => field => context.params(field).prefix(label)

    . reduce(_.append(_))

  given text: QueryEncoder[Text] = string => Params(List((t"", string)))
  given int: QueryEncoder[Int] = int => Params(List((t"", int.show)))
  given params: QueryEncoder[Params] = identity(_)
  given map[MapType <: Map[Text, Text]]: QueryEncoder[MapType] = map => Params(map.to(List))

trait QueryEncoder[ValueType]:
  def params(value: ValueType): Params
