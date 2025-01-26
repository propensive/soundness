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
import gossamer.*
import prepositional.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.deriving.*

trait CodlEncoder[ValueType]:
  def encode(value: ValueType): List[IArray[CodlNode]]
  def schema: CodlSchema

object CodlEncoder:
  def apply[ValueType](schema0: CodlSchema, encode0: ValueType => List[IArray[CodlNode]])
  :     CodlEncoder[ValueType] =
    new:
      def schema: CodlSchema = schema0
      def encode(value: ValueType): List[IArray[CodlNode]] = encode0(value)

  inline given derived: [ValueType] => CodlEncoder[ValueType] = compiletime.summonFrom:
    case given (ValueType is Encodable in Text) => field[ValueType]
    case given ProductReflection[ValueType]     => CodlEncoderDerivation.derived[ValueType]

  def field[ValueType: Encodable in Text]: CodlEncoder[ValueType] = new CodlEncoder[ValueType]:
    def schema: CodlSchema = Field(Arity.One)

    def encode(value: ValueType): List[IArray[CodlNode]] =
      List(IArray(CodlNode(Data(ValueType.encode(value)))))

  given optional: [ValueType: CodlEncoder] => CodlEncoder[Optional[ValueType]]:
    def schema: CodlSchema = ValueType.schema.optional

    def encode(value: Optional[ValueType]): List[IArray[CodlNode]] =
      value.let(ValueType.encode(_)).or(List())

  given boolean: CodlFieldWriter[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldWriter[Text] = _.show

  given option: [ValueType: CodlEncoder] => CodlEncoder[Option[ValueType]]:
    def schema: CodlSchema = ValueType.schema.optional

    def encode(value: Option[ValueType]): List[IArray[CodlNode]] = value match
      case None        => List()
      case Some(value) => ValueType.encode(value)

  given list: [ElementType: CodlEncoder] => CodlEncoder[List[ElementType]]:
    def schema: CodlSchema = ElementType.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: List[ElementType]): List[IArray[CodlNode]] =
      value.map(ElementType.encode(_).head)

  given set: [ElementType: CodlEncoder] => CodlEncoder[Set[ElementType]]:
    def schema: CodlSchema = ElementType.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def encode(value: Set[ElementType]): List[IArray[CodlNode]] =
      value.map(ElementType.encode(_).head).to(List)
