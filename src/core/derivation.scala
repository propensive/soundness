/*
    Cellulose, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import fulminate.*
import spectacular.*
import perforate.*
import wisteria.*
import anticipation.*
import gossamer.*

import scala.deriving.*
import scala.compiletime.*

//import language.experimental.captureChecking

trait CodlRelabelling[+TargetType]:
  def relabelling(): Map[Text, Text]
  private lazy val labels: Map[Text, Text] = relabelling()
  
  def apply(label: Text): Optional[Text] = if labels.contains(label) then labels(label) else Unset
  
case class CodlReadError() extends Error(msg"the CoDL value is not of the right format")

trait CodlEncoder[ValueType]:
  def encode(value: ValueType): List[IArray[CodlNode]]
  def schema: CodlSchema

trait CodlDecoder[ValueType]:
  def decode(value: List[Indexed])(using codlRead: Raises[CodlReadError]): ValueType
  def schema: CodlSchema

trait CodlFieldWriter[ValueType] extends CodlEncoder[ValueType]:
  def schema: CodlSchema = Field(Arity.One)
  def encodeField(value: ValueType): Text
  def encode(value: ValueType): List[IArray[CodlNode]] =
    List(IArray(CodlNode(Data(encodeField(value)))))

class CodlFieldReader[ValueType](lambda: Text => ValueType)
extends CodlDecoder[ValueType]:
  val schema: CodlSchema = Field(Arity.One)

  def decode(nodes: List[Indexed])(using codlError: Raises[CodlReadError]): ValueType =
    nodes.headOption.getOrElse(abort(CodlReadError())).children match
      case IArray(CodlNode(Data(value, _, _, _), _)) =>
        lambda(value)
      case _ =>
        abort(CodlReadError())

object CodlEncoderDerivation extends ProductDerivation[CodlEncoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CodlEncoder[DerivationType] =
    new CodlEncoder[DerivationType]:
      def schema: CodlSchema =
        val elements = contexts:
          [FieldType] => context =>
            val label2 = summonFrom:
              case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
              case _                                            => label

            CodlSchema.Entry(label2, context.schema)

        Struct(elements.to(List), Arity.One)
      
      def encode(product: DerivationType): List[IArray[CodlNode]] = List:
        IArray.from:
          fields(product):
            [FieldType] => field =>
              val label2 = summonFrom:
                case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
                case _                                            => label
              
              context.encode(field).map: value =>
                CodlNode(Data(label2, value, Layout.empty, context.schema))
              .filter(!_.empty)

          .to(List).flatten

object CodlEncoder:

  inline given derived[ValueType]: CodlEncoder[ValueType] = summonFrom:
    case given Encoder[ValueType]           => field[ValueType]
    case given ProductReflection[ValueType] => CodlEncoderDerivation.derived[ValueType]

  def field[ValueType](using encoder: Encoder[ValueType]): CodlEncoder[ValueType]/*^{encoder}*/ =
    new CodlEncoder[ValueType]:
      def schema: CodlSchema = Field(Arity.One)
      
      def encode(value: ValueType): List[IArray[CodlNode]] =
        List(IArray(CodlNode(Data(encoder.encode(value)))))

  given optional
      [ValueType]
      (using encoder: CodlEncoder[ValueType])
      : CodlEncoder[Optional[ValueType]] =
    new CodlEncoder[Optional[ValueType]]:
      def schema: CodlSchema = encoder.schema.optional
      def encode(value: Optional[ValueType]): List[IArray[CodlNode]] = value.let(encoder.encode(_)).or(List())

  given boolean: CodlFieldWriter[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldWriter[Text] = _.show
  
  given option
      [ValueType]
      (using encoder: CodlEncoder[ValueType])
      : CodlEncoder[Option[ValueType]] =
    new CodlEncoder[Option[ValueType]]:
      def schema: CodlSchema = encoder.schema.optional
      def encode(value: Option[ValueType]): List[IArray[CodlNode]] = value match
        case None        => List()
        case Some(value) => encoder.encode(value)
  
  given list
      [ElementType]
      (using encoder: CodlEncoder[ElementType])
      : CodlEncoder[List[ElementType]] =
    new CodlEncoder[List[ElementType]]:
      def schema: CodlSchema = encoder.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def encode(value: List[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => encoder.encode(value).head }
  
  given set
      [ElementType]
      (using encoder: CodlEncoder[ElementType])
      : CodlEncoder[Set[ElementType]] =
    new CodlEncoder[Set[ElementType]]:
      def schema: CodlSchema = encoder.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def encode(value: Set[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => encoder.encode(value).head }.to(List)
  
object CodlDecoderDerivation extends ProductDerivation[CodlDecoder]:
  inline def join[DerivationType <: Product: ProductReflection]: CodlDecoder[DerivationType] =
    new CodlDecoder[DerivationType]:
      def schema: CodlSchema =
        val elements = contexts:
          [FieldType] => context =>
            val label2 = summonFrom:
              case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
              case _                                            => label
            
            CodlSchema.Entry(label2, context.schema)
        
        Struct(elements.to(List), Arity.One)
          
      def decode(values: List[Indexed])(using codlRead: Raises[CodlReadError]): DerivationType =
        construct:
          [FieldType] => context =>
            val label2 = summonFrom:
              case relabelling: CodlRelabelling[DerivationType] => relabelling(label).or(label)
              case _                                            => label

            context.decode(values.headOption.getOrElse(abort(CodlReadError())).get(label2))
 

object CodlDecoder:

  inline given derived[ValueType]: CodlDecoder[ValueType] = summonFrom:
    case given Decoder[ValueType]           => field[ValueType]
    case given ProductReflection[ValueType] => CodlDecoderDerivation.derived[ValueType]

  def field[ValueType](using decoder: Decoder[ValueType]): CodlDecoder[ValueType]/*^{decoder}*/ =
    CodlFieldReader(decoder.decode(_))

  given boolean: CodlDecoder[Boolean] = CodlFieldReader(_ == t"yes")
  given text: CodlDecoder[Text] = CodlFieldReader(identity(_))

  given optional
      [ValueType]
      (using DummyImplicit)
      (using decoder: CodlDecoder[ValueType])
      : CodlDecoder[Optional[ValueType]] =

    new CodlDecoder[Optional[ValueType]]:
      def schema: CodlSchema = decoder.schema.optional
      
      def decode(value: List[Indexed])(using codlRead: Raises[CodlReadError]): Optional[ValueType] =
        if value.isEmpty then Unset else decoder.decode(value)

  given option[ValueType](using decoder: CodlDecoder[ValueType]): CodlDecoder[Option[ValueType]] =
    new CodlDecoder[Option[ValueType]]:
      def schema: CodlSchema = decoder.schema.optional
      def decode(value: List[Indexed])(using codlRead: Raises[CodlReadError]): Option[ValueType] =
        if value.isEmpty then None else Some(decoder.decode(value))
 
  given list[ElementType](using decoder: CodlDecoder[ElementType]): CodlDecoder[List[ElementType]] =
    new CodlDecoder[List[ElementType]]:
      def schema: CodlSchema = decoder.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def decode
          (value: List[Indexed])(using codlRead: Raises[CodlReadError])
          : List[ElementType] =
        decoder.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            decoder.decode(List(CodlDoc(node)))
        
          case struct: Struct =>
            value.map { v => decoder.decode(List(v)) }
  
  given set[ElementType](using decoder: CodlDecoder[ElementType]): CodlDecoder[Set[ElementType]] =
    new CodlDecoder[Set[ElementType]]:
      def schema: CodlSchema = decoder.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def decode(value: List[Indexed])(using coldRead: Raises[CodlReadError]): Set[ElementType] =
        decoder.schema match
          case Field(_, validator) =>
            value.flatMap(_.children).map: node =>
              decoder.decode(List(CodlDoc(node)))
            .to(Set)
          
          case struct: Struct =>
            value.map { v => decoder.decode(List(v)) }.to(Set)
