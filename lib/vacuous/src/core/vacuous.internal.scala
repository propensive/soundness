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
package vacuous

import scala.collection.immutable.{List, Nil, ::}

import scala.quoted.*

import fulminate.*
import gigantism.*
import prepositional.*

object internal:
  def check[value: Type]: Macro[Optionality[value]] =
    import quotes.reflect.*

    if TypeRepr.of[Unset.type] <:< TypeRepr.of[value].widen
    then '{null.asInstanceOf[Optionality[value]]}
    else halt(465, m"the type ${TypeRepr.of[value].widen.show} is not an `Optional`")

  def concrete[typeRef: Type]: Macro[typeRef is Concrete] =
    import quotes.reflect.*

    if TypeRepr.of[typeRef].typeSymbol.isAbstractType
    then halt(991, m"type ${TypeRepr.of[typeRef].show} is abstract")
    else '{Concrete[typeRef]()}

  def distinct[typeRef: Type, union: Type]: Macro[typeRef is Distinct from union] =
    import quotes.reflect.*

    concrete[typeRef]

    if TypeRepr.of[typeRef] <:< TypeRepr.of[union] then
      halt
        ( 401,
          m"""
            type ${TypeRepr.of[typeRef].show} cannot be proven distinct from
            ${TypeRepr.of[union].show}
          """ )
    else
      '{Distinct[typeRef, union]()}

  def mandatable[typeRef: Type]: Macro[typeRef is Mandatable] =
    import quotes.reflect.*

    var seen = false

    def recur(repr: TypeRepr): TypeRepr = repr match
      case OrType(left, right) =>
        if left <:< TypeRepr.of[Unset] then
          seen = true
          recur(right)
        else if right <:< TypeRepr.of[Unset] then
          seen = true
          recur(left)
        else
          OrType(recur(left), recur(right))

      case other =>
        other

    recur(TypeRepr.of[typeRef]).asType.absolve match case '[type result <: typeRef; result] =>
      if seen then '{Mandatable[typeRef, result]()}
      else halt(259, m"the value is not an `Optional`")

  def optimizeOr[value: Type](optional: Expr[Optional[value]], default: Expr[value]): Macro[value] =
    import quotes.reflect.*

    optional.absolve match case '{$optional: optionalType} => check[optionalType]

    // `Expr` is invariant, so when `value` is inferred with a refinement or capture
    // annotation that the `default` argument's own type lacks (common in macro
    // contexts, e.g. an abstract `Relative` type with `^`-annotated members), splicing
    // `default` into a `value`-typed position fails. Drop it to an untyped `Expr[Any]`
    // so its splice carries no constraint, and cast each branch to `value` instead;
    // the result is cast to `value` at the boundary anyway.
    val anyOptional: Expr[Optional[Any]] = optional.asTerm.asExprOf[Optional[Any]]
    val anyDefault: Expr[Any] = default.asTerm.asExpr

    def fallback: Term =
      '{
          $anyOptional match
            case Unset => $anyDefault
            case other => other
        }
      . asTerm

    def optimize(term: Term): Term = term match
      case
        ( inlined@Inlined
            ( call@Some(Apply(TypeApply(Ident("optimizable"), _), _)), bindings, term ) ) =>

        term match
          case Typed(Apply(select, List(_)), typeTree) =>
            Inlined(call, bindings, Typed(Apply(select, List(default.asTerm)), typeTree))

          case term =>
            fallback

      case Inlined(call, bindings, term) =>
        Inlined(call, bindings, optimize(term))

      case term =>
        fallback

    '{${optimize(optional.asTerm).asExpr}.asInstanceOf[value]}.asExprOf[value]
