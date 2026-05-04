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
    case class Available(name: Text, requirements: List[Missing]) extends Result

    case class Missing(name: Text, available: List[Available], candidates: List[Candidate])
    extends Result

    case class Found(name: Text, expr: Expr[Any]) extends Result

    val initialGas = 3

    def resultOf(t: TypeRepr): TypeRepr = t match
      case PolyType(_, _, body)   => resultOf(body)
      case MethodType(_, _, body) => resultOf(body)
      case _                      => t

    def availableFor(repr: TypeRepr, exclusions: List[Symbol], gas: Int): List[Available] =
      beneficence.givens(repr)
      . filterNot(exclusions.contains)
      . filter(conforms(_, repr))
      . map: symbol =>
          val requirements =
            if gas <= 0 then Nil
            else usingTypesInstantiated(symbol, repr).map: paramType =>
              val nextAvailable = availableFor(paramType, symbol :: exclusions, gas - 1)
              Missing(stenography.internal.name(paramType), nextAvailable, Nil)
          Available(displayName(symbol.fullName).tt, requirements)

    def conforms(symbol: Symbol, target: TypeRepr): Boolean =
      val resultType = resultOf(symbol.info)

      // For non-polymorphic givens, a direct subtype check is precise.
      if resultType <:< target then true
      else
        // For polymorphic givens, the result type contains type variables that
        // would be instantiated at the call site; we accept if the result type
        // and the target share the same type constructor and arity. Stricter
        // unification is left for later refinement.
        symbol.info match
          case _: PolyType =>
            (resultType.dealias, target.dealias) match
              case (AppliedType(rTycon, rArgs), AppliedType(tTycon, tArgs)) =>
                rTycon.dealias.classSymbol == tTycon.dealias.classSymbol
                && rArgs.length == tArgs.length
              case _ =>
                resultType.dealias.classSymbol == target.dealias.classSymbol
          case _ =>
            false

    def usingTypesInstantiated(symbol: Symbol, target: TypeRepr): List[TypeRepr] =
      val raw = usingTypes(symbol)
      if raw.isEmpty then raw
      else
        val typeParams =
          symbol.paramSymss.headOption.filter(_.forall(_.isType)).getOrElse(Nil)

        if typeParams.isEmpty then raw
        else
          val bindings = unify(resultOf(symbol.info), target, typeParams.toSet)
          val from = typeParams.filter(bindings.contains)
          if from.isEmpty then raw
          else raw.map(_.substituteTypes(from, from.map(bindings)))

    // Walk a `template` type (which may reference type parameters from `params`)
    // alongside a concrete `target` and collect the implied parameter bindings.
    // Used to instantiate type variables in `using` clauses according to the
    // type that the proposed given is being asked to provide.
    def unify(template: TypeRepr, target: TypeRepr, params: Set[Symbol])
    :     Map[Symbol, TypeRepr] =

      val templateSymbol = template.typeSymbol

      if params.contains(templateSymbol) then Map(templateSymbol -> target)
      else (template.dealias, target.dealias) match
        case (AppliedType(tTycon, tArgs), AppliedType(rTycon, rArgs))
        if tArgs.length == rArgs.length =>
          val tyconBindings = unify(tTycon, rTycon, params)
          tArgs.zip(rArgs).foldLeft(tyconBindings): (acc, pair) =>
            acc ++ unify(pair(0), pair(1), params)

        case _ =>
          Map.empty

    def usingTypes(symbol: Symbol): List[TypeRepr] =
      symbol.tree.absolve match
        case d: DefDef =>
          d.paramss.flatMap:
            case TermParamClause(params) =>
              if params.headOption.exists(_.symbol.flags.is(Flags.Given))
              then params.map(_.tpt.tpe)
              else Nil

            case _ =>
              Nil

        case _ =>
          Nil

    def displayName(fqn: String): String =
      fqn.split('.').iterator.filterNot: segment =>
        val stripped = if segment.endsWith("$") then segment.dropRight(1) else segment
        stripped.endsWith("$package")
      .mkString(".")

    def missing(repr: TypeRepr, candidates: List[Candidate]): Missing =
      Missing(stenography.internal.name(repr), availableFor(repr, Nil, initialGas), candidates)

    def seek(repr: TypeRepr, exclusions: List[Symbol], depth: Int): Result =
      Implicits.searchIgnoring(repr)(self :: exclusions*).absolve match
        case success: ImplicitSearchSuccess =>
          Found(t"${stenography.internal.name(repr)}", success.tree.asExpr)

        case failure: ImplicitSearchFailure =>
          failure match
            case _: dotty.tools.dotc.ast.untpd.SearchFailureIdent =>
              missing(repr, Nil)

            case SafeInlined(call, bindings, body) => call match
              case None =>
                missing(repr, Nil)

              case Some(term) => term match
                case TypeApply(left, right) =>
                  if term.symbol.name == "explainMissingContext"
                  then missing(repr, Nil)
                  else
                    Candidate
                      ( "???", List(seek(right.head.tpe, term.symbol :: exclusions, depth + 1)) )

                case _ =>
                  missing(repr, Nil)


            case apply: Apply @unchecked => apply match case Apply(function, arguments) =>
              def resolve(methodType: TypeRepr): Missing = methodType match
                case PolyType(_, _, _) =>
                  resolve(function.tpe.simplified)

                case MethodType(_, types, more) =>
                  val name = stenography.internal.name(function.symbol.termRef).show.skip(5, Rtl)
                  val candidate = Candidate(name, types.map(seek(_, Nil, depth + 1)))

                  val candidates = seek(repr, function.symbol :: exclusions, depth) match
                    case Missing(_, _, candidates) => candidate :: candidates
                    case _                         => Nil

                  missing(repr, candidates)

                case _ =>
                  missing(repr, Nil)

              resolve(function.symbol.info)

            case _ =>
              missing(repr, Nil)


    seek(TypeRepr.of[target], Nil, 1).absolve match
      case Found(_, expr) => expr.asExprOf[target]

      case Missing(_, available, candidates) =>
        report.errorAndAbort:
          given Result is Expandable =
            case Candidate(_, missing)             => missing
            case Missing(_, available, candidates) => available ::: candidates
            case Available(_, requirements)        => requirements
            case Found(_, _)                       => Nil

          TreeDiagram[Result]((available ::: candidates)*).render:
            case Found(name, _) =>
              e" \e[38;5;34m$Bold(✓)\e[0m found \e[38;5;119m$Italic($name)\e[0m"

            case Missing(name, _, _) =>
              e" \e[38;5;88m$Bold(✗)\e[0m requires \e[38;5;114m$Italic($name)\e[0m"

            case Candidate(name, _) =>
              e" \e[38;5;208m$Bold(▪)\e[0m candidate \e[38;5;227m$Italic($name)\e[0m"

            case Available(name, _) =>
              e" \e[38;5;75m$Bold(▸)\e[0m propose \e[38;5;117m$Italic($name)\e[0m"

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
