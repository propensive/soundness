/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

object CodlEncoderDerivation extends ProductDerivation[CodlEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CodlEncoder[DerivationType] =
    val mapping: Map[Text, Text] = compiletime.summonFrom:
      case relabelling: CodlRelabelling[DerivationType] => relabelling.relabelling()
      case _                                            => Map()

    val schema: CodlSchema =
      val elements = contexts:
        [FieldType] => context => CodlSchema.Entry(mapping.at(label).or(label), context.schema)

      Struct(elements.to(List), Arity.One)

    def encode(product: DerivationType): List[IArray[CodlNode]] = List:
      IArray.from:
        fields(product):
          [FieldType] => field =>
            val label2 = mapping.at(label).or(label)

            context.encode(field).map: value =>
              CodlNode(Data(label2, value, Layout.empty, context.schema))

            . filter(!_.empty)

        . to(List).flatten

    CodlEncoder(schema, encode)
