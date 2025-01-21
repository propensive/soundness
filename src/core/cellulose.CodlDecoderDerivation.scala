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

import contingency.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

object CodlDecoderDerivation extends ProductDerivation[CodlDecoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CodlDecoder[DerivationType] =
    val schema: CodlSchema =
      val elements = contexts:
        [FieldType] => context =>
          val label2 = compiletime.summonFrom:
            case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
            case _                                            => label

          CodlSchema.Entry(label2, context.schema)

      Struct(elements.to(List), Arity.One)

    def decode(values: List[Indexed]): DerivationType raises CodlReadError =
      construct:
        [FieldType] => context =>
          val label2 = compiletime.summonFrom:
            case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
            case _                                            => label

          context.decode(values.prim.lest(CodlReadError(label2)).get(label2))

    CodlDecoder[DerivationType](schema, decode)
