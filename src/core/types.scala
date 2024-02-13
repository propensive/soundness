/*
    Typonym, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package typonym

import scala.quoted.*

erased trait TypeList[+TupleType <: Tuple]
erased trait TypeSet[+TupleType <: Tuple]
erased trait TypeMap[+TupleType <: Tuple]

transparent inline def reify[PhantomType]: Any = ${Typonym.reify[PhantomType]}
//transparent inline def erase(inline value: Any): Any = ${Typonym.erase(value)}

object Typonym:
  private def untuple[TupleType <: Tuple: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    Type.of[TupleType] match
      case '[type tailType <: Tuple; headType *: tailType] => TypeRepr.of[headType] :: untuple[tailType]
      case _                                               => Nil

  def reify[PhantomType: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    Type.of[PhantomType] match
      case '[type listType <: Tuple; TypeList[listType]] =>
        untuple[listType].map(_.asType).map { case '[elementType] => reify[elementType] }
          .reverse
          .foldLeft('{Nil}) { (list, next) => '{$next :: $list} }

      case '[type setType <: Tuple; TypeSet[setType]] =>
        untuple[setType].map(_.asType).map { case '[elementType] => reify[elementType] }
          .foldLeft('{Set()}) { (set, next) => '{$set + $next} }

      case '[type mapType <: Tuple; TypeMap[mapType]] =>
        val entries = untuple[mapType].map(_.asType).map:
          case '[(keyType, valueType)] => '{(${reify[keyType]}, ${reify[valueType]})}
        .foldLeft('{Nil}): (list, next) =>
          '{$next :: $list}
        
        '{$entries.to(Map)}
        
      case other => TypeRepr.of[PhantomType] match
        case ConstantType(BooleanConstant(boolean)) => Expr(boolean)
        case ConstantType(IntConstant(int))         => Expr(int)
        case ConstantType(DoubleConstant(double))   => Expr(double)
        case ConstantType(StringConstant(string))   => Expr(string)

  def reflect(value: Any)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    value match
      case string: String   => ConstantType(StringConstant(string))
      case int: Int         => ConstantType(IntConstant(int))
      case double: Double   => ConstantType(DoubleConstant(double))
      case boolean: Boolean => ConstantType(BooleanConstant(boolean))

      case list: List[?] =>
        val tuple = list.map(reflect).reverse.foldLeft(TypeRepr.of[EmptyTuple]): (tuple, next) =>
          tuple.asType match
            case '[type tupleType <: Tuple; tupleType] => next.asType match
              case '[nextType] => TypeRepr.of[nextType *: tupleType]
        
        tuple.asType match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[TypeList[tupleType]]
 
      case set: Set[?] =>
        val tuple = set.to(List).map(reflect).reverse.foldLeft(TypeRepr.of[EmptyTuple]): (tuple, next) =>
          tuple.asType match
            case '[type tupleType <: Tuple; tupleType] => next.asType match
              case '[nextType] => TypeRepr.of[nextType *: tupleType]
        
        tuple.asType match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[TypeSet[tupleType]]