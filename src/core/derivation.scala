/*
    Cellulose, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import spectacular.*
import anticipation.*
import gossamer.*

import scala.deriving.*
import scala.compiletime.*

import language.experimental.captureChecking

case class CodlLabel[+TargetType, +FieldNameType <: Label](label: String)

case class CodlReadError() extends Error(msg"the CoDL value is not of the right format")

trait CodlSerializer[-ValueType]:
  def serialize(value: ValueType): List[IArray[CodlNode]]
  def schema: CodlSchema

trait CodlDeserializer[ValueType]:
  def deserialize(value: List[Indexed])(using codlRead: CanThrow[CodlReadError]): ValueType
  def schema: CodlSchema

trait CodlFieldSerializer[-ValueType] extends CodlSerializer[ValueType]:
  def schema: CodlSchema = Field(Arity.One)
  def serializeField(value: ValueType): Text
  def serialize(value: ValueType): List[IArray[CodlNode]] =
    List(IArray(CodlNode(Data(serializeField(value)))))

class CodlFieldDeserializer[ValueType](deserializer: Text => ValueType)
extends CodlDeserializer[ValueType]:
  val schema: CodlSchema = Field(Arity.One)

  def deserialize(nodes: List[Indexed])(using codlError: CanThrow[CodlReadError]): ValueType =
    nodes.headOption.getOrElse(throw CodlReadError()).children match
      case IArray(CodlNode(Data(value, _, _, _), _)) =>
        deserializer(value)
      case _ =>
        throw CodlReadError()

trait CodlSerializer2:
  given maybe
      [ValueType]
      (using serializer: CodlSerializer[ValueType])
      : CodlSerializer[ValueType | Unset.type] =
    new CodlSerializer[Maybe[ValueType]]:
      def schema: CodlSchema = serializer.schema.optional
      def serialize(value: Maybe[ValueType]): List[IArray[CodlNode]] =
        value.mm(serializer.serialize(_)).or(List())
  
object CodlSerializer extends CodlSerializer2:
  given field[ValueType](using encoder: Encoder[ValueType]): CodlSerializer[ValueType]^{encoder} =
    new CodlSerializer[ValueType]:
      def schema: CodlSchema = Field(Arity.One)
      
      def serialize(value: ValueType): List[IArray[CodlNode]] =
        List(IArray(CodlNode(Data(encoder.encode(value)))))

  given boolean: CodlFieldSerializer[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldSerializer[Text] = _.show
  
  given option
      [ValueType]
      (using serializer: CodlSerializer[ValueType])
      : CodlSerializer[Option[ValueType]] =
    new CodlSerializer[Option[ValueType]]:
      def schema: CodlSchema = serializer.schema.optional
      def serialize(value: Option[ValueType]): List[IArray[CodlNode]] = value match
        case None        => List()
        case Some(value) => serializer.serialize(value)
  
  given list
      [ElementType]
      (using serializer: CodlSerializer[ElementType])
      : CodlSerializer[List[ElementType]] =
    new CodlSerializer[List[ElementType]]:
      def schema: CodlSchema = serializer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def serialize(value: List[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => serializer.serialize(value).head }
  
  given set
      [ElementType]
      (using serializer: CodlSerializer[ElementType])
      : CodlSerializer[Set[ElementType]] =
    new CodlSerializer[Set[ElementType]]:
      def schema: CodlSchema = serializer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def serialize(value: Set[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => serializer.serialize(value).head }.to(List)
  
  inline given derived
      [DerivationType]
      (using mirror: Mirror.Of[DerivationType])
      : CodlSerializer[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => new CodlSerializer[DerivationType]:
        def schema: CodlSchema =
          Struct(CodlDeserializer.deriveSchema[DerivationType, mirror.MirroredElemTypes,
              mirror.MirroredElemLabels], Arity.One)
        
        def serialize(value: DerivationType): List[IArray[CodlNode]] =
          (value.asMatchable: @unchecked) match
            case value: Product =>
              val entries = deriveProduct[DerivationType, mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
              
              List(IArray.from(entries))
      
      case _ => compiletime.error("cannot derive a CodlSerializer sum type")

  private transparent inline def deriveProduct
      [DerivationType, Labels <: Tuple]
      (tuple: Tuple)
      : List[CodlNode] =
    inline tuple match
      case cons: (? *: ?) => cons match
        case head *: tail => inline erasedValue[Labels] match
          case _: (headLabel *: tailLabels) =>
            val label: String = summonFrom:
              case label: CodlLabel[DerivationType, `headLabel` & Label] =>
                label.label
              
              case _ => (valueOf[headLabel].asMatchable: @unchecked) match
                case label: String => label
            
            (label.asMatchable: @unchecked) match
              case label: String =>
                val serializer = summonInline[CodlSerializer[head.type]]
                
                val serialization =
                  serializer.serialize(head).map: value =>
                    CodlNode(Data(label.tt, value, Layout.empty, serializer.schema))
                  .filter(!_.empty)
                
                serialization ::: deriveProduct[DerivationType, tailLabels](tail)
        
      case _ => Nil

trait CodlDeserializer2:
  inline given derived
      [DerivationType]
      (using mirror: Mirror.Of[DerivationType])
      : CodlDeserializer[DerivationType] =
    inline mirror match
      case mirror: Mirror.ProductOf[DerivationType & Product] =>
        new CodlDeserializer[DerivationType]:
          def deserialize
              (value: List[Indexed])(using codlRead: CanThrow[CodlReadError])
              : DerivationType =
            mirror.fromProduct(deriveProduct[DerivationType, mirror.MirroredElemTypes,
                mirror.MirroredElemLabels](value))
        
          def schema: CodlSchema = Struct(deriveSchema[DerivationType, mirror.MirroredElemTypes,
              mirror.MirroredElemLabels], Arity.One)
      
      case _ => compiletime.error("cannot derive a CodlDeserializer sum type")
    
  transparent inline def deriveSchema
      [DerivationType, ElementTypes <: Tuple, Labels <: Tuple]
      : List[CodlSchema.Entry] =
    inline erasedValue[ElementTypes] match
      case EmptyTuple =>
        Nil
      
      case cons: (headType *: tailType) => inline erasedValue[Labels] match
        case _: (headLabel *: tailLabels) =>
          val label: String = summonFrom:
            case label: CodlLabel[DerivationType, `headLabel` & Label] =>
              label.label
            
            case _ => (valueOf[headLabel].asMatchable: @unchecked) match
              case label: String => label
          
          (label.asMatchable: @unchecked) match
            case label: String =>
              CodlSchema.Entry(label.tt, summonInline[CodlDeserializer[headType]].schema) ::
                  deriveSchema[DerivationType, tailType, tailLabels]

  private transparent inline def deriveProduct
      [DerivationType, ElementTypes <: Tuple, Labels <: Tuple]
      (value: List[Indexed])
      (using codlRead: CanThrow[CodlReadError])
      : Tuple =
    inline erasedValue[ElementTypes] match
      case EmptyTuple =>
        EmptyTuple
      
      case cons: (headType *: tailType) => inline erasedValue[Labels] match
        case _: (headLabel *: tailLabels) =>
          val label: String = summonFrom:
            case label: CodlLabel[DerivationType, `headLabel` & Label] =>
              label.label
            
            case _ => (valueOf[headLabel].asMatchable: @unchecked) match
              case label: String => label
          
          (label.asMatchable: @unchecked) match
            case label: String =>
              summonInline[CodlDeserializer[headType]].deserialize(value.head.get(label.tt)) *:
                  deriveProduct[DerivationType, tailType, tailLabels](value)
        
object CodlDeserializer extends CodlDeserializer2:
  given maybe
      [ValueType]
      (using deserializer: CodlDeserializer[ValueType])
      : CodlDeserializer[Maybe[ValueType]] =
    new CodlDeserializer[Maybe[ValueType]]:
      def schema: CodlSchema = deserializer.schema.optional
      
      def deserialize
          (value: List[Indexed])(using codlRead: CanThrow[CodlReadError])
          : Maybe[ValueType] =
        if value.isEmpty then Unset else deserializer.deserialize(value)
 
  given field[ValueType](using decoder: Decoder[ValueType]): CodlDeserializer[ValueType]^{decoder} =
    CodlFieldDeserializer(decoder.decode(_))
  
  given boolean: CodlDeserializer[Boolean] = CodlFieldDeserializer(_ == t"yes")
  given text: CodlDeserializer[Text] = CodlFieldDeserializer(identity(_))

  given option
      [ValueType]
      (using deserializer: CodlDeserializer[ValueType])
      : CodlDeserializer[Option[ValueType]] =
    new CodlDeserializer[Option[ValueType]]:
      def schema: CodlSchema = deserializer.schema.optional
      def deserialize
          (value: List[Indexed])(using codlRead: CanThrow[CodlReadError])
          : Option[ValueType] =
        if value.isEmpty then None else Some(deserializer.deserialize(value))
 
  given list
      [ElementType]
      (using deserializer: CodlDeserializer[ElementType])
      : CodlDeserializer[List[ElementType]] =
    new CodlDeserializer[List[ElementType]]:
      def schema: CodlSchema = deserializer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def deserialize
          (value: List[Indexed])(using codlRead: CanThrow[CodlReadError])
          : List[ElementType] =
        deserializer.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            deserializer.deserialize(List(CodlDoc(node)))
        
          case struct: Struct =>
            value.map { v => deserializer.deserialize(List(v)) }
  
  given set
      [ElementType]
      (using deserializer: CodlDeserializer[ElementType])
      : CodlDeserializer[Set[ElementType]] =
    new CodlDeserializer[Set[ElementType]]:
      def schema: CodlSchema = deserializer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def deserialize
          (value: List[Indexed])(using coldRead: CanThrow[CodlReadError])
          : Set[ElementType] =
        deserializer.schema match
          case Field(_, validator) =>
            value.flatMap(_.children).map: node =>
              deserializer.deserialize(List(CodlDoc(node)))
            .to(Set)
          
          case struct: Struct =>
            value.map { v => deserializer.deserialize(List(v)) }.to(Set)
