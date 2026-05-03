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
package frontier

import scala.quoted.*

import dotty.tools.dotc.*

import anticipation.*
import dendrology.*
import escapade.*
import fulminate.*
import gigantism.*
import gossamer.*
import proscenium.*
import spectacular.*
import symbolism.*

object internal:
  inline def explanation[target]: target = ${explain[target]}

  private object SafeInlined:
    def unapply(using Quotes)(scrutinee: quotes.reflect.ImplicitSearchFailure)
    :   Option
          [ (Option[quotes.reflect.Tree], List[quotes.reflect.Definition], quotes.reflect.Term) ] =

      import quotes.reflect.*

      try
        scrutinee match
          case inlined: Inlined @unchecked => Some((inlined.call, inlined.bindings, inlined.body))
          case _                           => None
      catch case error: Throwable => None

  object NoCandidates:
    def unapply(using Quotes)(scrutinee: quotes.reflect.ImplicitSearchFailure): Option[Text] =
      scrutinee match
        case _: dotty.tools.dotc.ast.untpd.SearchFailureIdent => Some(scrutinee.explanation.tt)
        case _                                                => None

  given treeStyle: [text: Textual] => TextualTreeStyle[text] =
    TextualTreeStyle(t"   ", t" └─", t" ├─", t" │ ")

  private var counter = 0

  private def next(): Int =
    counter = counter + 1
    counter

  given realm: Realm = realm"fr"

  def explain[target: Type]: Macro[target] =
    import quotes.reflect.*

    val id = next()
    val self = Symbol.requiredMethod("frontier.missingContext.explain")

    sealed trait Result

    given context: core.Contexts.Context = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl => quotes.ctx

    case class Candidate(name: Text, missing: List[Result]) extends Result
    case class Missing(name: Text, candidates: List[Candidate]) extends Result
    case class Found(name: Text, expr: Expr[Any]) extends Result

    def seek(repr: TypeRepr, exclusions: List[Symbol], depth: Int): Result =
      Implicits.searchIgnoring(repr)(self :: exclusions*).absolve match
        case success: ImplicitSearchSuccess =>
          Found(t"${stenography.internal.name(repr)}", success.tree.asExpr)

        case failure: ImplicitSearchFailure =>
          failure match
            case _: dotty.tools.dotc.ast.untpd.SearchFailureIdent =>
              Missing(stenography.internal.name(repr), Nil)

            case SafeInlined(call, bindings, body) => call match
              case None =>
                Missing(stenography.internal.name(repr), Nil)

              case Some(term) => term match
                case TypeApply(left, right) =>
                  if term.symbol.name == "explainMissingContext"
                  then Missing(stenography.internal.name(repr), Nil)
                  else
                    Candidate
                      ( "???", List(seek(right.head.tpe, term.symbol :: exclusions, depth + 1)) )

                case _ =>
                  Missing(stenography.internal.name(repr), Nil)


            case apply: Apply @unchecked => apply match case Apply(function, arguments) =>
              def resolve(methodType: TypeRepr): Missing = methodType match
                case PolyType(_, _, _) =>
                  resolve(function.tpe.simplified)

                case MethodType(_, types, more) =>
                  val name = stenography.internal.name(function.symbol.termRef).show.skip(5, Rtl)
                  val candidate = Candidate(name, types.map(seek(_, Nil, depth + 1)))

                  val candidates = seek(repr, function.symbol :: exclusions, depth) match
                    case Missing(_, candidates) => candidate :: candidates
                    case _                      => Nil

                  Missing(stenography.internal.name(repr), candidates)

                case _ =>
                  Missing(stenography.internal.name(repr), Nil)

              resolve(function.symbol.info)

            case _ =>
              Missing(stenography.internal.name(repr), Nil)


    seek(TypeRepr.of[target], Nil, 1).absolve match
      case Found(_, expr) => expr.asExprOf[target]

      case Missing(_, results) =>
        report.errorAndAbort:
          given Result is Expandable =
            case Candidate(_, missing)  => missing
            case Missing(_, candidates) => candidates
            case Found(_, _)            => Nil

          TreeDiagram[Result](results*).render:
            case Found(name, _) =>
              e" \e[38;5;34m$Bold(✓)\e[0m found \e[38;5;119m$Italic($name)\e[0m"

            case Missing(name, _) =>
              e" \e[38;5;88m$Bold(✗)\e[0m requires \e[38;5;114m$Italic($name)\e[0m"

            case Candidate(name, _) =>
              e" \e[38;5;208m$Bold(▪)\e[0m candidate \e[38;5;227m$Italic($name)\e[0m"

          . join
              ( e"contextual value not found\n\n \e[38;5;88m$Bold(■)\e[0m resolving "
                +e"\e[38;5;208m$Italic(${stenography.internal.name[target]})\e[0m\n",
                e"\n",
                e"\n" )

          . render(termcapDefinitions.xterm256)
          . s

  def every[value: Type]: Macro[Every[value]] =
    import quotes.reflect.*

    given context: core.Contexts.Context = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl => quotes.ctx

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
                symbol1.isNoSymbol || symbol2.isNoSymbol
                || ignored.contains(symbol1) || ignored.contains(symbol2)

              if unusable then acc.reverse
              else collect
                ( symbol1 :: symbol2 :: ignored,
                  tree2.asExprOf[value] :: tree1.asExprOf[value] :: acc )

            case _ =>
              acc.reverse

    '{Every[value](${Expr.ofList(collect(Nil, Nil))})}
