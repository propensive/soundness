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
package gigantism

import scala.quoted.*

type Macro[result] = Quotes ?=> Expr[result]

// The native-typed counterparts of `Expr.ofList` and `Varargs`: macro code holding a native
// `List` of already-lifted expressions builds the lifted native list (or a varargs splice)
// directly, with the stdlib hop confined here. `Lifts.Varargs` is the extractor counterpart,
// yielding a varargs argument's element expressions as a native `List`. (Named `Lifts`, not
// `Exprs`: the wildcard `scala.quoted.*` import would shadow an `Exprs` here at use sites.)
object Lifts:
  def list[element: Type](elements: List[Expr[element]])(using Quotes): Expr[List[element]] =
    // `.stdlib`: `Expr.ofList` takes a stdlib `Seq`; this is the confined hop.
    '{List.from(${Expr.ofList(elements.stdlib)})}

  def varargs[element: Type](elements: List[Expr[element]])(using Quotes)
  :   Expr[scala.Seq[element]] =
    // `.stdlib`: `Varargs` takes a stdlib `Seq`; this is the confined hop.
    scala.quoted.Varargs(elements.stdlib)

  object Varargs:
    def unapply[element](expr: Expr[scala.Seq[element]])(using Quotes)
    :   Option[List[Expr[element]]] =
      scala.quoted.Varargs.unapply(expr).map(List.from(_))

inline def every[value]: Every[value] = ${Every.summonAll[value]}

inline def metaprogramming(using quotes: Quotes): Metaprogramming(quotes) =
  Metaprogramming(quotes)

case class Metaprogramming(tracked val quotes: Quotes):
  import dotty.tools.dotc.core.*
  import quotes.reflect.*

  private val context: Contexts.Context = quotes.asInstanceOf[runtime.impl.QuotesImpl].ctx

  case class Import(wildcard: Boolean, term: quotes.reflect.TermRef)

  def imports: List[Import] =
    def recur(context: Contexts.Context, found: List[Import]): List[Import] =
      if context == dotty.tools.dotc.core.Contexts.NoContext then found else

        val found2 = if !context.isImportContext then found else
          val info = context.importInfo.nn
          val termRef = info.site(using context).asInstanceOf[quotes.reflect.TermRef]

          Import(info.isWildcardImport, termRef) :: found

        recur(context.outer, found2)

    recur(context, Nil)
