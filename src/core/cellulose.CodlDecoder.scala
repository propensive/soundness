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
import contingency.*
import gossamer.*
import prepositional.*
import vacuous.*
import wisteria.*

import scala.deriving.*

trait CodlDecoder[ValueType]:
  def decoded(value: List[Indexed]): ValueType raises CodlReadError
  def schema: CodlSchema

object CodlDecoder:

  def apply[ValueType]
     (schema0: => CodlSchema, decode0: Tactic[CodlReadError] ?=> List[Indexed] => ValueType)
  :     CodlDecoder[ValueType] = new:
    def decoded(value: List[Indexed]): ValueType raises CodlReadError = decode0(value)
    def schema: CodlSchema  = schema0

  inline given derived[ValueType]: CodlDecoder[ValueType] = compiletime.summonFrom:
    case given (ValueType is Decodable in Text) => field[ValueType]
    case given ProductReflection[ValueType]     => CodlDecoderDerivation.derived[ValueType]

  def field[ValueType: Decodable in Text]: CodlDecoder[ValueType] =
    CodlFieldReader(ValueType.decoded(_))

  given boolean: CodlDecoder[Boolean] = CodlFieldReader(_ == t"yes")
  given text: CodlDecoder[Text] = CodlFieldReader(identity(_))

  given unit: CodlDecoder[Unit]:
    val schema: CodlSchema = Field(Arity.One)
    def decoded(nodes: List[Indexed]): Unit raises CodlReadError = ()

  given optional: [ValueType: CodlDecoder] => CodlDecoder[Optional[ValueType]]:
    def schema: CodlSchema = ValueType.schema.optional

    def decoded(value: List[Indexed]): Optional[ValueType] raises CodlReadError =
      if value.isEmpty then Unset else ValueType.decoded(value)

  given option: [ValueType: CodlDecoder] => CodlDecoder[Option[ValueType]]:
    def schema: CodlSchema = ValueType.schema.optional

    def decoded(value: List[Indexed]): Option[ValueType] raises CodlReadError =
      if value.isEmpty then None else Some(ValueType.decoded(value))

  given list: [ElementType: CodlDecoder] => CodlDecoder[List[ElementType]] =
    new CodlDecoder[List[ElementType]]:
      def schema: CodlSchema = ElementType.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)

      def decoded(value: List[Indexed]): List[ElementType] raises CodlReadError =
        ElementType.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            ElementType.decoded(List(CodlDoc(node)))

          case struct: Struct =>
            value.map { v => ElementType.decoded(List(v)) }

  given set: [ElementType: CodlDecoder] => CodlDecoder[Set[ElementType]]:
    def schema: CodlSchema = ElementType.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def decoded(value: List[Indexed]): Set[ElementType] raises CodlReadError =
      ElementType.schema match
        case Field(_, validator) =>
          value.flatMap(_.children).map { node => ElementType.decoded(List(CodlDoc(node))) }.to(Set)

        case struct: Struct =>
          value.map { v => ElementType.decoded(List(v)) }.to(Set)
