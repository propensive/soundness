/*
    Vacuous, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package vacuous

import scala.quoted.*

import fulminate.*

object Vacuous:
  def check[ValueType: Type](using Quotes): Expr[Optionality[ValueType]] =
    import quotes.reflect.*
    if TypeRepr.of[Unset.type] <:< TypeRepr.of[ValueType].widen
    then '{null.asInstanceOf[Optionality[ValueType]]}
    else halt(m"the type ${TypeRepr.of[ValueType].widen.show} is not an `Optional`")

  def optimizeOr[ValueType: Type]
     (optional: Expr[Optional[ValueType]], default: Expr[ValueType])(using Quotes)
  :     Expr[ValueType] =

    import quotes.reflect.*

    optional match
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
                  case term  => term.asInstanceOf[ValueType] } . asTerm

      case Inlined(call, bindings, term) =>
        Inlined(call, bindings, optimize(term))

      case term =>
        ' { $optional match
              case Unset => $default
              case term  => term.asInstanceOf[ValueType] } . asTerm

    '{${optimize(optional.asTerm).asExpr}.asInstanceOf[ValueType]}.asExprOf[ValueType]
