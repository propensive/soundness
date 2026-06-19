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
package gigantism

import scala.quoted.*

import dotty.tools.dotc.*

object internal:
  // Collects every given instance of `value` visible at the use site into an `Every`, including
  // mutually-ambiguous instances (the public reflection API surfaces ambiguity only as a failure;
  // recovering both alternatives needs the compiler-internal `AmbiguousImplicits`).
  def every[value: Type]: Macro[Every[value]] =
    import quotes.reflect.*

    given context: core.Contexts.Context = quotes.asInstanceOf[runtime.impl.QuotesImpl].ctx

    def underlying(tree: Term): Symbol = tree match
      case Inlined(_, _, body) => underlying(body)
      case Apply(fun, _)       => underlying(fun)
      case TypeApply(fun, _)   => underlying(fun)
      case Block(_, expr)      => underlying(expr)
      case Typed(expr, _)      => underlying(expr)
      case other               => other.symbol

    def collect(ignored: List[Symbol], acc: List[Expr[value]]): List[Expr[value]] =
      Implicits.searchIgnoring(TypeRepr.of[value])(ignored*).absolve match
        case success: ImplicitSearchSuccess =>
          val symbol = underlying(success.tree)

          if symbol.isNoSymbol || ignored.contains(symbol) then acc.reverse
          else collect(symbol :: ignored, success.tree.asExprOf[value] :: acc)

        case failure: ImplicitSearchFailure =>
          failure.asInstanceOf[ast.tpd.Tree].tpe match
            case ambiguous: typer.Implicits.AmbiguousImplicits =>
              val tree1 = ambiguous.alt1.tree.asInstanceOf[Term]
              val tree2 = ambiguous.alt2.tree.asInstanceOf[Term]
              val symbol1 = underlying(tree1)
              val symbol2 = underlying(tree2)

              val unusable =
                symbol1.isNoSymbol || symbol2.isNoSymbol ||
                  ignored.contains(symbol1) || ignored.contains(symbol2)

              if unusable then acc.reverse
              else collect
                ( symbol1 :: symbol2 :: ignored,
                  tree2.asExprOf[value] :: tree1.asExprOf[value] :: acc )

            case _ =>
              acc.reverse

    '{Every[value](${Expr.ofList(collect(Nil, Nil))})}
