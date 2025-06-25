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
┃    Soundness, version 0.36.0.                                                                    ┃
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
package vacuous

import scala.quoted.*
import scala.compiletime.*

import fulminate.*
import prepositional.*
import proscenium.*

object Vacuous:
  def check[value: Type]: Macro[Optionality[value]] =
    import quotes.reflect.*
    if TypeRepr.of[Unset.type] <:< TypeRepr.of[value].widen
    then '{null.asInstanceOf[Optionality[value]]}
    else halt(m"the type ${TypeRepr.of[value].widen} is not an `Optional`")

  def concrete[typeRef: Type]: Macro[typeRef is Concrete] =
    import quotes.reflect.*

    if TypeRepr.of[typeRef].typeSymbol.isAbstractType
    then halt(m"type ${Type.of[typeRef]} is abstract")
    else '{erasedValue[typeRef is Concrete]}

  def distinct[typeRef: Type, union: Type]: Macro[typeRef is Distinct from union] =
    import quotes.reflect.*

    concrete[typeRef]

    if TypeRepr.of[typeRef] <:< TypeRepr.of[union]
    then halt(m"type ${Type.of[typeRef]} cannot be proven distinct from ${Type.of[union]}")
    else '{erasedValue[typeRef is Distinct from union]}


  def optimizeOr[value: Type](optional: Expr[Optional[value]], default: Expr[value]): Macro[value] =
    import quotes.reflect.*

    optional.runtimeChecked match
      case '{ $optional: optionalType } => check[optionalType]

    def optimize(term: Term): Term = term match
      case inlined@Inlined
            (call@Some(Apply(TypeApply(Ident("optimizable"), _), _)), bindings, term) =>
        term match
          case Typed(Apply(select, List(_)), typeTree) =>
            Inlined(call, bindings, Typed(Apply(select, List(default.asTerm)), typeTree))

          case term =>
            ' { $optional match
                  case Unset => $default
                  case term  => term.asInstanceOf[value] } . asTerm

      case Inlined(call, bindings, term) =>
        Inlined(call, bindings, optimize(term))

      case term =>
        ' { $optional match
              case Unset => $default
              case term  => term.asInstanceOf[value] } . asTerm

    '{${optimize(optional.asTerm).asExpr}.asInstanceOf[value]}.asExprOf[value]
