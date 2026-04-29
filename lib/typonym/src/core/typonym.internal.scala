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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import gigantism.*
import proscenium.*

object internal:
  private def untuple[tuple <: Tuple: Type](using Quotes): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    Type.of[tuple] match
      case '[type tail <: Tuple; head *: tail] =>
        TypeRepr.of[head] :: untuple[tail]

      case _ =>
        Nil

  def reifyAs[phantom: Type, result: Type]: Macro[result] =
    reify[phantom].asExprOf[result]

  def reify[phantom: Type]: Macro[Any] =
    import quotes.reflect.*

    def constant(repr: TypeRepr): Expr[Any] = repr.absolve match
      case ConstantType(BooleanConstant(boolean)) => Expr(boolean)
      case ConstantType(IntConstant(int))         => Expr(int)
      case ConstantType(DoubleConstant(double))   => Expr(double)
      case ConstantType(StringConstant(string))   => Expr(string)

    Type.of[phantom] match
      case '[type list <: Tuple; TypeList[list]] =>
        untuple[list].map(_.asType).map:
          _.absolve match
            case '[element] => reify[element]

        . reverse
        . foldLeft('{Nil}) { (list, next) => '{$next :: $list} }

      case '[type map <: Tuple; TypeMap[map]] =>
        val entries =
          val pairs: List[TypeRepr] = untuple[map]

          val keyValues: List[Expr[(Any, Any)]] = pairs.map(_.asType).map:
            _.absolve match
              case '[(key, value)] => '{(${reify[key]}, ${reify[value]})}

          def recur(todo: List[Expr[(Any, Any)]]): Expr[List[(Any, Any)]] = todo match
            case Nil          => '{Nil}
            case head :: tail => '{$head :: ${recur(tail)}}

          recur(keyValues)

        '{$entries.to(Map)}

      case '[type set; TypeSet[set]] =>
        def recur(repr: TypeRepr): List[Expr[set]] = repr.dealias match
          case OrType(left, right) => recur(left) ++ recur(right)
          case other               => List(constant(other).asExprOf[set])

        '{List[set](${Varargs(recur(TypeRepr.of[set]))}*)}

      case other => constant(TypeRepr.of[phantom])

  def reflect(value: Any)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    value.absolve match
      case string: String   => ConstantType(StringConstant(string))
      case int: Int         => ConstantType(IntConstant(int))
      case double: Double   => ConstantType(DoubleConstant(double))
      case boolean: Boolean => ConstantType(BooleanConstant(boolean))

      case list: List[?] =>
        val tuple = list.map(reflect).reverse.foldLeft(TypeRepr.of[Zero]): (tuple, next) =>
          tuple.asType.absolve match
            case '[type tuple <: Tuple; tuple] => next.asType.absolve match
              case '[next] => TypeRepr.of[next *: tuple]

        tuple.asType.absolve match
          case '[type tuple <: Tuple; tuple] => TypeRepr.of[TypeList[tuple]]
