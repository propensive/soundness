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
┃    Soundness, version 0.64.0.                                                                    ┃
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
import murmuration.*

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
        // Hoisted from the `map` below: a quoted pattern match inside a combinator lambda in a
        // macro risks the `wildApprox` crash.
        def reifyElement(repr: TypeRepr): Expr[Any] = repr.asType.absolve match
          case '[element] => reify[element]

        val elements: List[Expr[Any]] = untuple[list].map(reifyElement)

        // The `.stdlib` bridge feeds `Expr.ofList`, whose parameter is a stdlib `Seq`.
        '{List.from(${Expr.ofList(elements.stdlib)})}

      case '[type map <: Tuple; TypeMap[map]] =>
        val entries =
          val pairs: List[TypeRepr] = untuple[map]

          // Hoisted for the same `wildApprox` reason as `reifyElement` above.
          def reifyPair(repr: TypeRepr): Expr[(Any, Any)] = repr.asType.absolve match
            case '[(key, value)] => '{(${reify[key]}, ${reify[value]})}

          val keyValues: List[Expr[(Any, Any)]] = pairs.map(reifyPair)

          '{List.from(${Expr.ofList(keyValues.stdlib)})}

        '{Map.from(List.iterator($entries))}

      case '[type set; TypeSet[set]] =>
        def recur(repr: TypeRepr): List[Expr[set]] = repr.dealias match
          case OrType(left, right) => List.concat(recur(left), recur(right))
          case other               => List(constant(other).asExprOf[set])

        // The `.stdlib` bridge feeds `Varargs`, whose parameter is a stdlib `Seq`.
        '{List[set](${Varargs(recur(TypeRepr.of[set]).stdlib)}*)}

      case other => constant(TypeRepr.of[phantom])

  def reflect(value: Any)(using Quotes): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    value.absolve match
      case string: String   => ConstantType(StringConstant(string))
      case int: Int         => ConstantType(IntConstant(int))
      case double: Double   => ConstantType(DoubleConstant(double))
      case boolean: Boolean => ConstantType(BooleanConstant(boolean))

      case list: List[?] =>
        // Hoisted from the `fold` below: quoted pattern matches inside a combinator lambda in a
        // macro risk the `wildApprox` crash.
        def cons(tuple: TypeRepr, next: TypeRepr): TypeRepr = tuple.asType.absolve match
          case '[type tuple <: Tuple; tuple] => next.asType.absolve match
            case '[next] => TypeRepr.of[next *: tuple]

        val reflected: List[TypeRepr] = List.from(List.iterator(list).map(reflect))
        val tuple = List.invert(reflected).fold(TypeRepr.of[Zero])(cons)

        tuple.asType.absolve match
          case '[type tuple <: Tuple; tuple] => TypeRepr.of[TypeList[tuple]]
