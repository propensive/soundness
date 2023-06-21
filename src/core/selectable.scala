/*
    Polyvinyl, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package polyvinyl

import rudiments.*

import scala.quoted.*

trait SimpleSchema[FieldType]:
  def fields: List[String]
  transparent inline def record(inline access: String => FieldType): SimpleRecord[FieldType]
  
  final def build
      [FieldType: Type]
      (access: Expr[String => FieldType])(using Quotes)
      : Expr[SimpleRecord[FieldType]] =
    import quotes.reflect.*
    
    val simpleRecordType = TypeRepr.of[SimpleRecord[FieldType]]
    val refinedType = fields.foldLeft(simpleRecordType)(Refinement(_, _, TypeRepr.of[FieldType]))
    
    (refinedType.asType: @unchecked) match
      case '[type refinedType <: SimpleRecord[FieldType]; refinedType] =>
        '{new SimpleRecord[FieldType]($access).asInstanceOf[refinedType]}
  
class SimpleRecord[FieldType](access: String => FieldType) extends Selectable:
  def selectDynamic(name: String): FieldType = access(name)

class Record(access: String => Any) extends Selectable:
  def selectDynamic(name: String): Any = access(name)

trait Schema[InitEnumType <: reflect.Enum]:
  type EnumType = InitEnumType
  type Result[_ <: EnumType]
  
  def types: Map[String, EnumType]
  
  transparent inline def record(inline access: String => Any): Record

  def build(access: Expr[String => Any])(using Quotes, Type[EnumType], Type[Result]): Expr[Record] =
    import quotes.reflect.*

    val refinedType = types.foldLeft(TypeRepr.of[Record]):
      case (acc, (key, enumType)) =>
        val companion = Ref(TypeRepr.of[EnumType].typeSymbol.companionModule)
        val sym = companion.symbol.declaredField(enumType.toString)
        
        val returnType = (Singleton(companion.select(sym)).tpe.asType: @unchecked) match
          case '[type singletonType <: EnumType; singletonType] =>
            TypeRepr.of[Result[singletonType]].simplified

        Refinement(acc, key, returnType)
    
    (refinedType.asType: @unchecked) match
      case '[type refinedType <: Record; refinedType] =>
        '{new Record($access(_)).asInstanceOf[refinedType]}
