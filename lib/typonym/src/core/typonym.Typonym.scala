                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package typonym

import scala.quoted.*

import proscenium.*

object Typonym:
  private def untuple[TupleType <: Tuple: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    Type.of[TupleType] match
      case '[type tailType <: Tuple; headType *: tailType] =>
        TypeRepr.of[headType] :: untuple[tailType]

      case _ =>
        Nil

  def reify[PhantomType: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*

    Type.of[PhantomType] match
      case '[type listType <: Tuple; TypeList[listType]] =>
        untuple[listType].map(_.asType).map:
          _.runtimeChecked match
            case '[elementType] => reify[elementType]

        . reverse
        . foldLeft('{Nil}) { (list, next) => '{$next :: $list} }

      case '[type mapType <: Tuple; TypeMap[mapType]] =>
        val entries =
          val pairs: List[TypeRepr] = untuple[mapType]

          val keyValues: List[Expr[(Any, Any)]] = pairs.map(_.asType).map:
            _.runtimeChecked match
              case '[(keyType, valueType)] => '{(${reify[keyType]}, ${reify[valueType]})}

          def recur(todo: List[Expr[(Any, Any)]]): Expr[List[(Any, Any)]] = todo match
            case Nil => '{Nil}
            case head :: tail => '{$head :: ${recur(tail)}}

          recur(keyValues)

        '{$entries.to(Map)}

      case other => TypeRepr.of[PhantomType].runtimeChecked match
        case ConstantType(BooleanConstant(boolean)) => Expr(boolean)
        case ConstantType(IntConstant(int))         => Expr(int)
        case ConstantType(DoubleConstant(double))   => Expr(double)
        case ConstantType(StringConstant(string))   => Expr(string)

  def reflect(value: Any)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    value.runtimeChecked match
      case string: String   => ConstantType(StringConstant(string))
      case int: Int         => ConstantType(IntConstant(int))
      case double: Double   => ConstantType(DoubleConstant(double))
      case boolean: Boolean => ConstantType(BooleanConstant(boolean))

      case list: List[?] =>
        val tuple = list.map(reflect).reverse.foldLeft(TypeRepr.of[Zero]): (tuple, next) =>
          tuple.asType.runtimeChecked match
            case '[type tupleType <: Tuple; tupleType] => next.asType.runtimeChecked match
              case '[nextType] => TypeRepr.of[nextType *: tupleType]

        tuple.asType.runtimeChecked match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[TypeList[tupleType]]
