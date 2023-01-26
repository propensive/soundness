/*
    Cellulose, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import deviation.*
import gossamer.*
import wisteria.*

import Arity.*

import language.experimental.pureFunctions

case class codlLabel(label: String) extends StaticAnnotation

case class CodlReadError() extends Error(err"the CoDL value is not of the right format")

trait Codec[T]:
  def serialize(value: T): List[IArray[CodlNode]]
  def deserialize(value: List[Indexed]): T throws CodlReadError
  def schema: CodlSchema

class FieldCodec[T](serializer: T => Text, deserializer: Text => T) extends Codec[T]:
  val schema: CodlSchema = Field(Arity.One)
  def serialize(value: T): List[IArray[CodlNode]] = List(IArray(CodlNode(Data(serializer(value)))))
  
  def deserialize(nodes: List[Indexed]): T throws CodlReadError =
    nodes.headOption.getOrElse(throw CodlReadError()).children match
      case IArray(CodlNode(Data(value, _, _, _), _)) => deserializer(value)
      case _                                       => throw CodlReadError()

object Codec extends ProductDerivation[Codec]:
  given maybe[T](using codec: Codec[T]): Codec[T | Unset.type] = new Codec[Maybe[T]]:
    def schema: CodlSchema = summon[Codec[T]].schema.optional
  
    def serialize(value: Maybe[T]): List[IArray[CodlNode]] = value match
      case Unset               => List()
      case value: T @unchecked => codec.serialize(value)
    
    def deserialize(value: List[Indexed]): Maybe[T] throws CodlReadError =
      if value.isEmpty then Unset else codec.deserialize(value)

  def join[T](ctx: CaseClass[Codec, T]): Codec[T] = new Codec[T]:
    def schema: CodlSchema =
      val entries: IArray[CodlSchema.Entry] = ctx.params.map: param =>
        val label = param.annotations.collectFirst { case `codlLabel`(name) => name }.getOrElse(param.label)
        CodlSchema.Entry(label.show, param.typeclass.schema)

      Struct(entries.to(List), Arity.One)
    
    def serialize(value: T): List[IArray[CodlNode]] = List:
      ctx.params.flatMap: p =>
        val children = p.typeclass.serialize(p.deref(value)).map: children =>
          val index = children.zipWithIndex.foldLeft(Map[Text, Int]()):
            case (map, (child, idx)) => child.mm(_.id).fm(map)(map.updated(_, idx))

        p.typeclass.serialize(p.deref(value)).map: value =>
          val label = p.annotations.collectFirst { case `codlLabel`(name) => name }.getOrElse(p.label)
          CodlNode(Data(label.show, value, Layout.empty, p.typeclass.schema))
        .filter(!_.empty)

    def deserialize(value: List[Indexed]): T throws CodlReadError = ctx.construct: param =>
      val label = param.annotations.collectFirst { case `codlLabel`(name) => name }.getOrElse(param.label)
      param.typeclass.deserialize(value.head.get(label.show))

  given [T](using canon: Canonical[T]): Codec[T] = FieldCodec(canon.serialize, canon.deserialize)
  given (using CanThrow[IncompatibleTypeError]): Codec[Char] = FieldCodec(_.show, _.as[Char])
  given Codec[Text] = FieldCodec(_.show, identity(_))
  
  given (using CanThrow[IncompatibleTypeError]): Codec[Boolean] =
    FieldCodec(v => if v then t"yes" else t"no", _ == t"yes")

  given option[T](using codec: Codec[T]): Codec[Option[T]] = new Codec[Option[T]]:
    def schema: CodlSchema = summon[Codec[T]].schema.optional
  
    def serialize(value: Option[T]): List[IArray[CodlNode]] = value match
      case None        => List()
      case Some(value) => codec.serialize(value)
    
    def deserialize(value: List[Indexed]): Option[T] throws CodlReadError =
      if value.isEmpty then None else Some(codec.deserialize(value))
    
  given list[T](using codec: => Codec[T]): Codec[List[T]] = new Codec[List[T]]:
    def schema: CodlSchema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def serialize(value: List[T]): List[IArray[CodlNode]] = value.map { (value: T) => codec.serialize(value).head }

    def deserialize(value: List[Indexed]): List[T] throws CodlReadError = codec.schema match
      case Field(_, validator) => value.flatMap(_.children).map: node =>
        codec.deserialize(List(CodlDoc(node)))
      
      case struct: Struct =>
        value.map { v => codec.deserialize(List(v)) }
  
  given set[T](using codec: Codec[T]): Codec[Set[T]] = new Codec[Set[T]]:
    def schema: CodlSchema = codec.schema match
      case Field(_, validator) => Field(Arity.Many, validator)
      case struct: Struct      => struct.copy(structArity = Arity.Many)

    def serialize(value: Set[T]): List[IArray[CodlNode]] =
      value.map { (value: T) => codec.serialize(value).head }.to(List)

    def deserialize(value: List[Indexed]): Set[T] throws CodlReadError = codec.schema match
      case Field(_, validator) =>
        value.flatMap(_.children).map { node => codec.deserialize(List(CodlDoc(node))) }.to(Set)
      
      case struct: Struct =>
        value.map { v => codec.deserialize(List(v)) }.to(Set)
