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
import vacuous.*
import fulminate.*
import spectacular.*
import perforate.*
import anticipation.*
import gossamer.*

import scala.deriving.*
import scala.compiletime.*

//import language.experimental.captureChecking

case class CodlLabel[+TargetType, +FieldNameType <: Label](label: String)

case class CodlReadError() extends Error(msg"the CoDL value is not of the right format")

trait CodlWriter[-ValueType]:
  def write(value: ValueType): List[IArray[CodlNode]]
  def schema: CodlSchema

trait CodlReader[ValueType]:
  def read(value: List[Indexed])(using codlRead: Raises[CodlReadError]): ValueType
  def schema: CodlSchema

trait CodlFieldWriter[-ValueType] extends CodlWriter[ValueType]:
  def schema: CodlSchema = Field(Arity.One)
  def writeField(value: ValueType): Text
  def write(value: ValueType): List[IArray[CodlNode]] =
    List(IArray(CodlNode(Data(writeField(value)))))

class CodlFieldReader[ValueType](reader: Text => ValueType)
extends CodlReader[ValueType]:
  val schema: CodlSchema = Field(Arity.One)

  def read(nodes: List[Indexed])(using codlError: Raises[CodlReadError]): ValueType =
    nodes.headOption.getOrElse(abort(CodlReadError())).children match
      case IArray(CodlNode(Data(value, _, _, _), _)) =>
        reader(value)
      case _ =>
        abort(CodlReadError())

trait CodlWriter3:
  given maybe
      [ValueType]
      (using writer: CodlWriter[ValueType])
      : CodlWriter[ValueType | Unset.type] =
    new CodlWriter[Optional[ValueType]]:
      def schema: CodlSchema = writer.schema.optional
      def write(value: Optional[ValueType]): List[IArray[CodlNode]] =
        value.let(writer.write(_)).or(List())

trait CodlWriter2 extends CodlWriter3:
  inline given derived
      [DerivationType]
      (using mirror: Mirror.Of[DerivationType])
      : CodlWriter[DerivationType] =
    inline mirror match
      case given Mirror.ProductOf[DerivationType & Product] => new CodlWriter[DerivationType]:
        def schema: CodlSchema =
          Struct(CodlReader.deriveSchema[DerivationType, mirror.MirroredElemTypes,
              mirror.MirroredElemLabels], Arity.One)
        
        def write(value: DerivationType): List[IArray[CodlNode]] =
          (value.asMatchable: @unchecked) match
            case value: Product =>
              val entries = deriveProduct[DerivationType, mirror.MirroredElemLabels](Tuple.fromProductTyped(value))
              
              List(IArray.from(entries))
      
      case _ => compiletime.error("cannot derive a CodlWriter sum type")

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
                val writer = summonInline[CodlWriter[head.type]]
                
                val serialization =
                  writer.write(head).map: value =>
                    CodlNode(Data(label.tt, value, Layout.empty, writer.schema))
                  .filter(!_.empty)
                
                serialization ::: deriveProduct[DerivationType, tailLabels](tail)
        
      case _ => Nil
  
object CodlWriter extends CodlWriter2:
  given field[ValueType](using encoder: Encoder[ValueType]): CodlWriter[ValueType]/*^{encoder}*/ =
    new CodlWriter[ValueType]:
      def schema: CodlSchema = Field(Arity.One)
      
      def write(value: ValueType): List[IArray[CodlNode]] =
        List(IArray(CodlNode(Data(encoder.encode(value)))))

  given boolean: CodlFieldWriter[Boolean] = if _ then t"yes" else t"no"
  given text: CodlFieldWriter[Text] = _.show
  
  given option
      [ValueType]
      (using writer: CodlWriter[ValueType])
      : CodlWriter[Option[ValueType]] =
    new CodlWriter[Option[ValueType]]:
      def schema: CodlSchema = writer.schema.optional
      def write(value: Option[ValueType]): List[IArray[CodlNode]] = value match
        case None        => List()
        case Some(value) => writer.write(value)
  
  given list
      [ElementType]
      (using writer: CodlWriter[ElementType])
      : CodlWriter[List[ElementType]] =
    new CodlWriter[List[ElementType]]:
      def schema: CodlSchema = writer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def write(value: List[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => writer.write(value).head }
  
  given set
      [ElementType]
      (using writer: CodlWriter[ElementType])
      : CodlWriter[Set[ElementType]] =
    new CodlWriter[Set[ElementType]]:
      def schema: CodlSchema = writer.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def write(value: Set[ElementType]): List[IArray[CodlNode]] =
        value.map { (value: ElementType) => writer.write(value).head }.to(List)
  
trait CodlReader2:
  inline given derived
      [DerivationType]
      (using mirror: Mirror.Of[DerivationType])
      : CodlReader[DerivationType] =
    inline mirror match
      case mirror: Mirror.ProductOf[DerivationType & Product] =>
        new CodlReader[DerivationType]:
          def read
              (value: List[Indexed])(using codlRead: Raises[CodlReadError])
              : DerivationType =
            mirror.fromProduct(deriveProduct[DerivationType, mirror.MirroredElemTypes,
                mirror.MirroredElemLabels](value))
        
          def schema: CodlSchema = Struct(deriveSchema[DerivationType, mirror.MirroredElemTypes,
              mirror.MirroredElemLabels], Arity.One)
      
      case _ => compiletime.error("cannot derive a CodlReader sum type")
    
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
              CodlSchema.Entry(label.tt, summonInline[CodlReader[headType]].schema) ::
                  deriveSchema[DerivationType, tailType, tailLabels]

  private transparent inline def deriveProduct
      [DerivationType, ElementTypes <: Tuple, Labels <: Tuple]
      (value: List[Indexed])
      (using codlRead: Raises[CodlReadError])
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
              summonInline[CodlReader[headType]].read(value.headOption.getOrElse(abort(CodlReadError())).get(label.tt)) *:
                  deriveProduct[DerivationType, tailType, tailLabels](value)
        
object CodlReader extends CodlReader2:
  given maybe
      [ValueType]
      (using reader: CodlReader[ValueType])
      : CodlReader[Optional[ValueType]] =
    new CodlReader[Optional[ValueType]]:
      def schema: CodlSchema = reader.schema.optional
      
      def read
          (value: List[Indexed])(using codlRead: Raises[CodlReadError])
          : Optional[ValueType] =
        if value.isEmpty then Unset else reader.read(value)
 
  given field[ValueType](using decoder: Decoder[ValueType]): CodlReader[ValueType]/*^{decoder}*/ =
    CodlFieldReader(decoder.decode(_))
  
  given boolean: CodlReader[Boolean] = CodlFieldReader(_ == t"yes")
  given text: CodlReader[Text] = CodlFieldReader(identity(_))

  given option
      [ValueType]
      (using reader: CodlReader[ValueType])
      : CodlReader[Option[ValueType]] =
    new CodlReader[Option[ValueType]]:
      def schema: CodlSchema = reader.schema.optional
      def read
          (value: List[Indexed])(using codlRead: Raises[CodlReadError])
          : Option[ValueType] =
        if value.isEmpty then None else Some(reader.read(value))
 
  given list
      [ElementType]
      (using reader: CodlReader[ElementType])
      : CodlReader[List[ElementType]] =
    new CodlReader[List[ElementType]]:
      def schema: CodlSchema = reader.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def read
          (value: List[Indexed])(using codlRead: Raises[CodlReadError])
          : List[ElementType] =
        reader.schema match
          case Field(_, validator) => value.flatMap(_.children).map: node =>
            reader.read(List(CodlDoc(node)))
        
          case struct: Struct =>
            value.map { v => reader.read(List(v)) }
  
  given set
      [ElementType]
      (using reader: CodlReader[ElementType])
      : CodlReader[Set[ElementType]] =
    new CodlReader[Set[ElementType]]:
      def schema: CodlSchema = reader.schema match
        case Field(_, validator) => Field(Arity.Many, validator)
        case struct: Struct      => struct.copy(structArity = Arity.Many)
      
      def read
          (value: List[Indexed])(using coldRead: Raises[CodlReadError])
          : Set[ElementType] =
        reader.schema match
          case Field(_, validator) =>
            value.flatMap(_.children).map: node =>
              reader.read(List(CodlDoc(node)))
            .to(Set)
          
          case struct: Struct =>
            value.map { v => reader.read(List(v)) }.to(Set)
