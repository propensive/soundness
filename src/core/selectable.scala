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
import scala.compiletime.*

trait Record extends Selectable:
  def access(name: String): Any
  def selectDynamic(name: String): Any = access(name)

erased trait ValueCast[RecordType, TypeNameType <: Label, ValueType]
erased trait RecordCast[RecordType, TypeNameType <: Label, TypeConstructorType[_]]


enum RecordField:
  case Value(fieldType: String)
  case Record(fieldType: String, map: Map[String, RecordField])

trait Schema[RecordType <: Record]:
  def fields: Map[String, RecordField]
  def make(value: Any): RecordType

  def build
      (value: Expr[Any])
      (using Quotes, Type[RecordType])
      (using thisType: Type[this.type])
      : Expr[RecordType] =
    import quotes.reflect.*

    val target = (thisType: @unchecked) match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule).asExprOf[Schema[RecordType]]

    def refine(fields: List[(String, RecordField)], refinedType: TypeRepr): TypeRepr = fields match
      case Nil =>
        refinedType
      
      case (name, RecordField.Record(typeName, map)) :: tail =>
        (ConstantType(StringConstant(typeName)).asType: @unchecked) match
          case '[type typeName <: Label; typeName] =>
            (Expr.summon[RecordCast[RecordType, typeName, ?]]: @unchecked) match
              case None =>
                fail(s"it was not possible to find a RecordCast instance for the field $name with "+
                    s"type $typeName")
              case Some('{type typeConstructor[_]; $expr: RecordCast[a, b, typeConstructor]}) =>
                val recordType =
                  (refine(map.to(List), TypeRepr.of[RecordType]).asType: @unchecked) match
                    case '[recordType] => TypeRepr.of[typeConstructor[recordType]]

                refine(tail, Refinement(refinedType, name, recordType))

      case (name, RecordField.Value(typeName)) :: tail =>
        (ConstantType(StringConstant(typeName)).asType: @unchecked) match
          case '[type typeName <: Label; typeName] =>
            (Expr.summon[ValueCast[RecordType, typeName, ?]]: @unchecked) match
              case None =>
                fail(s"it was not possible to find a ValueCast instance for the field $name with "+
                    s"type $typeName")
            
              case Some('{$expr: ValueCast[a, b, valueType]}) =>
                refine(tail, Refinement(refinedType, name, TypeRepr.of[valueType]))

    (refine(fields.to(List), TypeRepr.of[RecordType]).asType: @unchecked) match
      case '[type refinedType <: RecordType; refinedType] =>
        '{$target.make($value).asInstanceOf[refinedType]}

object JsonRecord:
  erased given ValueCast[JsonRecord, "boolean", Boolean] = ###
  erased given ValueCast[JsonRecord, "string", String] = ###
  erased given ValueCast[JsonRecord, "integer", Int] = ###
  erased given ValueCast[JsonRecord, "number", Double] = ###
  erased given RecordCast[JsonRecord, "array", [T] =>> List[T]] = ###
  erased given RecordCast[JsonRecord, "object", [T] =>> T] = ###

class JsonRecord(value: Map[String, Any]) extends Record:
  def access(name: String): Any = value(name)

abstract class JsonSchema() extends Schema[JsonRecord]:
  def make(value: Any): JsonRecord = JsonRecord:
    value.asInstanceOf[Map[String, Any]].view.mapValues: value =>
      value.asMatchable match
        case array: List[Map[?, ?]] => array.map(make)
        case record: Map[?, ?]      => make(record)
        case other                  => other
    .to(Map)

object ExampleSchema extends JsonSchema():
  import RecordField.*

  val fields: Map[String, RecordField] = Map(
    "age"  -> Value("integer"),
    "name" -> Value("string"),
    "male" -> Value("boolean"),
    "items" -> Record("array", Map(
      "year" -> Value("integer"),
      "month" -> Value("string"),
      "day" -> Value("integer")
    )),
    "data" -> Record("object", Map(
      "color" -> Value("string"),
      "size"  -> Value("integer")
    ))
  )

  transparent inline def record(inline value: Any): JsonRecord = ${build('value)}