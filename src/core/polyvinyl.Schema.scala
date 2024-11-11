/*
    Polyvinyl, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import scala.quoted.*

import rudiments.*
import anticipation.*
import fulminate.*
import prepositional.*

trait Schema[DataType, RecordType <: Record in DataType]:
  def fields: Map[String, RecordField]
  def make(data: DataType, transform: String => DataType => Any): RecordType
  def access(name: String, value: DataType): DataType

  def build(value: Expr[DataType])(using Quotes, Type[RecordType], Type[DataType])
     (using thisType: Type[this.type])
          : Expr[RecordType] =

    import quotes.reflect.*

    given Realm = realm"polyvinyl"

    val target = (thisType: @unchecked) match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule).asExprOf[Schema[DataType, RecordType]]

    def refine
       (value:       Expr[DataType],
        fields:      List[(String, RecordField)],
        refinedType: TypeRepr,
        caseDefs:    List[CaseDef] = List(CaseDef(Wildcard(), None, '{???}.asTerm)))
            : (TypeRepr, List[CaseDef]) =
      fields match
        case Nil =>
          (refinedType, caseDefs)

        case (name, RecordField.Value(typeName, params*)) :: tail =>
          (ConstantType(StringConstant(typeName)).asType: @unchecked) match
            case '[type typeName <: Label; typeName] =>
              (Expr.summon[Schematic[RecordType, DataType, typeName, ?]]: @unchecked) match
                case None =>
                  abandon
                   (m"could not find a Schematic instance for the field $name with type $typeName")

                case Some('{$accessor: Schematic[RecordType, DataType, typeName, valueType]}) =>

                  val rhs: Expr[DataType => Any] =
                    '{
                      (data: DataType) =>
                        $accessor.transform($target.access(${Expr(name)}, data), ${Expr(params.to(List))})
                    }

                  val caseDefs2 = CaseDef(Literal(StringConstant(name)), None, rhs.asTerm) :: caseDefs
                  val refinement = Refinement(refinedType, name, TypeRepr.of[valueType])

                  refine(value, tail, refinement, caseDefs2)

        case (name, RecordField.Record(typeName, map)) :: tail =>
          (ConstantType(StringConstant(typeName)).asType: @unchecked) match
            case '[type typeName <: Label; typeName] =>
              (Expr.summon[RecordAccessor[RecordType, DataType, typeName, ?]]: @unchecked) match
                case None =>
                  abandon
                   (m"""could not find a RecordAccessor instance for the field $name with type
                        $typeName""")

                case Some('{ type typeConstructor[_]
                             $accessor: RecordAccessor[RecordType, DataType, typeName, typeConstructor] }) =>

                  val nested = '{$target.access(${Expr(name)}, $value)}
                  val recordTypeRepr = TypeRepr.of[RecordType]
                  val (nestedType, nestedCaseDefs) = refine(nested, map.to(List), recordTypeRepr)

                  val matchFn: Expr[String => DataType => Any] =
                    '{ (name: String) => ${Match('name.asTerm, nestedCaseDefs).asExprOf[DataType => Any]} }

                  val maker: Expr[DataType => RecordType] = '{ field => $target.make(field, $matchFn) }

                  val rhs: Expr[DataType => Any] =
                    '{ data => $accessor.transform($target.access(${Expr(name)}, data), $maker) }

                  val caseDef = CaseDef(Literal(StringConstant(name)), None, rhs.asTerm)

                  (nestedType.asType: @unchecked) match
                    case '[nestedRecordType] =>
                      val typeRepr = TypeRepr.of[typeConstructor[nestedRecordType]]
                      refine(value, tail, Refinement(refinedType, name, typeRepr), caseDef :: caseDefs)

    val (refinedType, caseDefs) = refine(value, fields.to(List), TypeRepr.of[RecordType])

    val matchFn: Expr[String => DataType => Any] =
      '{ (name: String) => ${Match('name.asTerm, caseDefs).asExprOf[DataType => Any]} }

    (refinedType.asType: @unchecked) match
      case '[type refinedType <: RecordType; refinedType] =>
        '{$target.make($value, $matchFn).asInstanceOf[refinedType]}
