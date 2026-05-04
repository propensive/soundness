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

    case class Matched
                ( symbol:     Symbol,
                  typeParams: List[Symbol],
                  bindings:   Map[Symbol, TypeRepr] )

    def availableFor(repr: TypeRepr, exclusions: List[Symbol], gas: Int): List[Available] =
      val matched =
        beneficence.givens(repr)
        . filterNot(exclusions.contains)
        . filter(respectsOpaqueScope(_, repr))
        . flatMap(matchAgainst(_, repr))

      dedupeExportsByCanonical(matched).map: m =>
        val requirements =
          if gas <= 0 then Nil
          else usingTypesInstantiated(m).map: paramType =>
            val nextAvailable = availableFor(paramType, m.symbol :: exclusions, gas - 1)
            Missing(stenography.internal.name(paramType), nextAvailable, Nil)
        Available(formatProposal(m), requirements)

    // A symbol introduced via `export X.y` carries the `Exported` flag and its
    // tree's body forwards to the original. Walk through forwarders to find
    // the canonical (non-exported) symbol that ultimately backs it.
    def canonical(symbol: Symbol): Symbol =
      if !symbol.flags.is(Flags.Exported) then symbol
      else
        val target = symbol.tree.absolve match
          case d: DefDef => d.rhs.fold(Symbol.noSymbol)(underlyingSymbol)
          case _         => Symbol.noSymbol

        if target.exists && target != symbol then canonical(target) else symbol

    def underlyingSymbol(tree: Term): Symbol = tree match
      case Apply(fun, _)     => underlyingSymbol(fun)
      case TypeApply(fun, _) => underlyingSymbol(fun)
      case Inlined(_, _, b)  => underlyingSymbol(b)
      case Block(_, expr)    => underlyingSymbol(expr)
      case Typed(expr, _)    => underlyingSymbol(expr)
      case _                 => tree.symbol

    // Several classpath givens can be exports/re-exports of the same canonical
    // definition. Group by canonical symbol and keep only the one whose
    // stenography rendering is shortest (i.e. the most readable in the current
    // import scope).
    def dedupeExportsByCanonical(matched: List[Matched]): List[Matched] =
      matched.groupBy(m => canonical(m.symbol)).values.toList.map: group =>
        group.minBy(m => renderSymbol(m.symbol).s.length)

    // Render a proposal: the symbol's stenography path, suffixed by an
    // explicit type-parameter substitution `[T = X, U = Y]` whenever the
    // proposal's type parameters were bound from unification against the
    // required type. Lets the user see exactly which choices the search has
    // committed to before recursing.
    def formatProposal(matched: Matched): Text =
      val baseName = renderSymbol(matched.symbol).s
      val pairs = matched.typeParams.collect:
        case tp if matched.bindings.contains(tp) =>
          s"${tp.name.toString} = ${stenography.internal.name(matched.bindings(tp)).s}"
      if pairs.isEmpty then baseName.tt
      else s"$baseName [${pairs.mkString(", ")}]".tt

    // Render a symbol's path via Stenography (so import-scope abbreviations
    // apply). Strip the trailing `.type` that comes from rendering a singleton
    // TermRef, and the synthetic `<filename>$package` segments Scala 3 inserts
    // for top-level definitions.
    def renderSymbol(symbol: Symbol): Text =
      val raw = stenography.internal.name(symbol.termRef).s
      val noType = if raw.endsWith(".type") then raw.substring(0, raw.length - 5).nn else raw
      noType.split('.').iterator.filterNot: segment =>
        val stripped = if segment.endsWith("$") then segment.dropRight(1).nn else segment
        stripped.endsWith("$package")
      .mkString(".").tt

    // Determine whether a given symbol can produce a value conforming to
    // `target`, and if so, what bindings its type parameters must take.
    // Returns None for any candidate that fails to fully match: missing
    // bindings, bounds violations, or a substituted result type that doesn't
    // satisfy `<:< target` after substitution.
    def matchAgainst(symbol: Symbol, target: TypeRepr): Option[Matched] =
      val typeParams =
        symbol.paramSymss.headOption.filter(_.forall(_.isType)).getOrElse(Nil)

      if typeParams.isEmpty then
        val resultRaw = resultOf(symbol.info)
        if resultRaw <:< target then Some(Matched(symbol, Nil, Map.empty)) else None
      else
        candidateArgLists(symbol, target, typeParams)
        . iterator
        . flatMap: args =>
            val pairs = typeParams.zip(args).toMap
            if !boundsRespected(typeParams, pairs) then None
            else
              val substituted = resultOf(symbol.info.appliedTo(args))
              if substituted <:< target then Some(Matched(symbol, typeParams, pairs))
              else None
        . nextOption()

    // Generate candidate type-argument lists to try when matching a polymorphic
    // given against a concrete target. We combine:
    //  1. Bindings recovered by structural unification of the raw result type
    //     (handles plain `F[T]` shapes).
    //  2. The target's `AppliedType` arguments, in order.
    //  3. Type-alias values pulled out of the target's `Refinement` chain
    //     (e.g. the `Char` in `Concatenable { type Self = Char }`, produced by
    //     the modular `T is U` syntax — for which the synth-class result type
    //     doesn't share top-level structure with the target).
    def candidateArgLists
                ( symbol:     Symbol,
                  target:     TypeRepr,
                  typeParams: List[Symbol] )
    :     List[List[TypeRepr]] =

      val raw = resultOf(symbol.info)
      val bindings = unify(raw, target, typeParams.toSet)

      val unified: List[List[TypeRepr]] =
        if typeParams.forall(bindings.contains)
        then List(typeParams.map(bindings))
        else Nil

      val applied: List[List[TypeRepr]] = target.dealias match
        case AppliedType(_, args) if args.length == typeParams.length => List(args)
        case _                                                        => Nil

      val refinementValues = collectRefinementValues(target.dealias)
      val refinement: List[List[TypeRepr]] =
        if refinementValues.length == typeParams.length then List(refinementValues) else Nil

      (unified ++ applied ++ refinement).distinct

    def collectRefinementValues(t: TypeRepr): List[TypeRepr] =
      t.dealias match
        case Refinement(parent, _, info) =>
          val parentValues = collectRefinementValues(parent)
          val infoValue = info match
            case TypeBounds(lo, hi) if lo =:= hi => List(hi)
            case _                               => Nil
          parentValues ++ infoValue

        case _ =>
          Nil

    def boundsRespected(typeParams: List[Symbol], bindings: Map[Symbol, TypeRepr])
    :     Boolean =

      typeParams.forall: tp =>
        bindings.get(tp).fold(true): boundType =>
          tp.info match
            case TypeBounds(lo, hi) => lo <:< boundType && boundType <:< hi
            case _                  => true

    // Compute the using-clause types for a matched candidate, with type
    // variables instantiated according to the bindings recovered during
    // matching. We obtain them from the symbol's type (`info.appliedTo(args)`)
    // rather than from the source AST, so the substitution applies to the
    // bound `TypeParamRef`s that `Symbol.tree` would expose unsubstituted.
    def usingTypesInstantiated(matched: Matched): List[TypeRepr] =
      if matched.typeParams.isEmpty then usingTypes(matched.symbol)
      else
        val args = matched.typeParams.map(matched.bindings)
        matched.symbol.info.appliedTo(args) match
          case mt: MethodType =>
            collectUsingClause(mt)
          case _ =>
            usingTypes(matched.symbol)

    def collectUsingClause(mt: MethodType): List[TypeRepr] =
      val MethodType(paramNames, paramTypes, _) = mt
      // Tell whether this is a using clause by checking if it's a contextual
      // method type. In dotty's quotes.reflect, `MethodType.isImplicit` is
      // exposed for contextual/implicit clauses.
      if mt.isImplicit then paramTypes else Nil

    // Detect when a candidate is a member of an opaque type's companion
    // object. For a `given` declared in such a companion, dotty's `<:<` will
    // (incorrectly, from the call site's perspective) see through the opaque
    // alias when matching it against types based on the opaque's underlying.
    // Scan the owner chain for any module whose enclosing scope contains a
    // sibling opaque type with the same name.
    def opaqueCompanionType(symbol: Symbol): Option[Symbol] =
      def loop(current: Symbol): Option[Symbol] =
        if current.isNoSymbol || current == defn.RootClass then None
        else if current.flags.is(Flags.Module) then
          val moduleName = current.name.toString match
            case n if n.endsWith("$") => n.substring(0, n.length - 1).nn
            case n                    => n

          val sibling = current.owner.declaredTypes.find: t =>
            t.name.toString == moduleName && t.flags.is(Flags.Opaque)

          sibling.orElse(loop(current.owner))
        else loop(current.owner)

      loop(symbol.owner)

    // For a candidate from an opaque companion, only accept it if `target`
    // literally mentions the opaque type — otherwise the apparent match is
    // only via the opaque's underlying alias, which isn't supposed to be
    // visible at the call site.
    def respectsOpaqueScope(symbol: Symbol, target: TypeRepr): Boolean =
      opaqueCompanionType(symbol) match
        case Some(opaqueType) => mentions(target, opaqueType)
        case None             => true

    def mentions(t: TypeRepr, sym: Symbol): Boolean =
      if t.typeSymbol == sym then true
      else t.dealias match
        case AppliedType(tycon, args) =>
          mentions(tycon, sym) || args.exists(mentions(_, sym))

        case Refinement(parent, _, info) =>
          mentions(parent, sym) || mentions(info, sym)

        case TypeBounds(lo, hi) =>
          mentions(lo, sym) || mentions(hi, sym)

        case _ =>
          false

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

        case (tRef: Refinement, rRef: Refinement) =>
          val Refinement(tParent, tName, tInfo) = tRef
          val Refinement(rParent, rName, rInfo) = rRef
          if tName != rName then Map.empty
          else unify(tParent, rParent, params) ++ unifyInfo(tInfo, rInfo, params)

        case _ =>
          Map.empty

    def unifyInfo(template: TypeRepr, target: TypeRepr, params: Set[Symbol])
    :     Map[Symbol, TypeRepr] =

      (template, target) match
        case (TypeBounds(tLo, tHi), TypeBounds(rLo, rHi)) =>
          unify(tHi, rHi, params) ++ unify(tLo, rLo, params)

        case _ =>
          unify(template, target, params)

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
