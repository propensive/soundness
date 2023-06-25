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

trait Record(access: String => Any) extends Selectable:
  def selectDynamic(name: String): Any = access(name)

trait ValueAccessor[RecordType, LabelType <: Label, ValueType]:
  def transform(value: Any): ValueType

trait RecordAccessor[RecordType, LabelType <: Label, TypeConstructorType[_]]:
  def transform(value: Any, make: Any => RecordType): TypeConstructorType[RecordType]

enum RecordField:
  case Value(fieldType: String)
  case Record(fieldType: String, map: Map[String, RecordField])

trait Schema[RecordType <: Record]:
  def fields: Map[String, RecordField]
  def make(transform: String => Any): RecordType
  def access(name: String, value: Any): Any

  def build
      (value: Expr[Any])
      (using Quotes, Type[RecordType])
      (using thisType: Type[this.type])
      : Expr[RecordType] =
    import quotes.reflect.*
  
    val target = (thisType: @unchecked) match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule).asExprOf[Schema[RecordType]]

    def refine
        (value: Expr[Any], fields: List[(String, RecordField)], refinedType: TypeRepr, caseDefs: List[CaseDef])
        : (TypeRepr, List[CaseDef]) = fields match
      case Nil =>
        (refinedType, caseDefs)

      case (name, RecordField.Value(typeName)) :: tail =>
        (ConstantType(StringConstant(typeName)).asType: @unchecked) match
          case '[type typeName <: Label; typeName] =>
            (Expr.summon[ValueAccessor[RecordType, typeName, ?]]: @unchecked) match
              case None =>
                fail(s"it was not possible to find a ValueAccessor instance for the field $name with type $typeName")
            
              case Some('{$accessor: ValueAccessor[recordType, labelType, valueType]}) =>
                val rhs = '{$accessor.transform($target.access(${Expr(name)}, $value))}
                val caseDef = CaseDef(Literal(StringConstant(name)), None, rhs.asTerm)
                refine(value, tail, Refinement(refinedType, name, TypeRepr.of[valueType]), caseDef :: caseDefs)

      case (name, RecordField.Record(typeName, map)) :: tail =>
        (ConstantType(StringConstant(typeName)).asType: @unchecked) match
          case '[type typeName <: Label; typeName] =>
            (Expr.summon[RecordAccessor[RecordType, typeName, ?]]: @unchecked) match
              case None =>
                fail(s"it was not possible to find a RecordAccessor instance for the field $name with type $typeName")
              
              case Some('{
                  type typeConstructor[_]
                  $accessor: RecordAccessor[RecordType, labelType, typeConstructor]
                  }) =>

                val value2 = '{$target.access(${Expr(name)}, $value)}
                
                val (nestedType, nestedCases) =
                  refine(value2, map.to(List), TypeRepr.of[RecordType], Nil)
                  
                val nestedRecordType = (nestedType.asType: @unchecked) match
                  case '[recordType] => TypeRepr.of[typeConstructor[recordType]]
                
                val nestedMatchFn: Expr[Any => String => Any] =
                  '{(value: Any) => (name: String) => ${Match('name.asTerm, nestedCases).asExpr}}
                
                println(nestedMatchFn.show)

                val rhs = '{$accessor.transform($value2, name => $target.make($nestedMatchFn(name)))}
                val caseDef = CaseDef(Literal(StringConstant(name)), None, rhs.asTerm)
                
                refine(value, tail, Refinement(refinedType, name, nestedRecordType), caseDef :: caseDefs)
    
    val (refinedType, caseDefs) = refine(value, fields.to(List), TypeRepr.of[RecordType], Nil)

    val matchFn: Expr[Any => String => Any] =
      '{ (value: Any) => (name: String) => ${Match('name.asTerm, caseDefs).asExpr} }
    
    (refinedType.asType: @unchecked) match
      case '[type refinedType <: RecordType; refinedType] =>
        '{$target.make($matchFn($value)).asInstanceOf[refinedType]}