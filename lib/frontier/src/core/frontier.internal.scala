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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import proscenium.compat.*

import scala.collection.immutable.{List, Nil, ::}
import scala.quoted.*

import dotty.tools.dotc.*

import anticipation.*
import denominative.*
import gigantism.*
import gossamer.*
import proscenium.*
import spectacular.*
import vacuous.*

object internal:
  inline def explanation[target]: target = ${explain[target]}

  // Non-transparent inline: expands in the `inlining` phase (post-typer), so
  // its `report.errorAndAbort` is a terminal error that always reaches the
  // user — unlike an `errorAndAbort` fired during the nested implicit search
  // that selected the transparent `explainMissingContext` catch-all, which
  // Dotty buffers and shadows. The catch-all returns a call to this in place
  // of a found value.
  inline def fail[target](message: String): target = ${failMacro[target]('message)}

  def failMacro[target: Type](message: Expr[String]): Macro[target] =
    import quotes.reflect.*
    report.errorAndAbort(message.valueOrAbort)

  private object SafeInlined:
    def unapply(using Quotes)(scrutinee: quotes.reflect.ImplicitSearchFailure)
    :
      Option
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

  def explain[target: Type]: Macro[target] =
    import quotes.reflect.*

    // The catch-all `given`s to exclude from the diagnostic re-search so it
    // doesn't propose (or recurse into) itself. Resolved by name and guarded
    // against absence — the real recursion stop is the name check in `seek`.
    val self: List[Symbol] =
      List("frontier.context.explainMissingContext", "soundness.context.explainMissingContext")
      . flatMap: path =>
          try List(Symbol.requiredMethod(path)) catch case _: Throwable => Nil

    sealed trait Result

    given context: core.Contexts.Context = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl => quotes.ctx

    case class Candidate(name: Text, symbol: Symbol, missing: List[Result]) extends Result
    case class Available(name: Text, requirements: List[Missing]) extends Result

    case class Missing(name: Text, available: List[Available], candidates: List[Candidate])
    extends Result

    case class Found(name: Text, expr: Expr[Any]) extends Result

    val initialGas = 3

    def resultOf(t: TypeRepr): TypeRepr = t match
      case PolyType(_, _, body)   => resultOf(body)
      case MethodType(_, _, body) => resultOf(body)
      case ByNameType(body)       => resultOf(body)
      case _                      => t

    case class Matched
      ( symbol:     Symbol,
        typeParams: List[Symbol],
        bindings:   scala.collection.immutable.Map[Symbol, TypeRepr] )

    val givensCache = scala.collection.mutable.Map.empty[TypeRepr, List[Symbol]]

    def cachedGivens(repr: TypeRepr): List[Symbol] =
      givensCache.getOrElseUpdate(repr, beneficence.givens(repr))

    val canonicalCache = scala.collection.mutable.Map.empty[Symbol, Symbol]

    def canonical(symbol: Symbol): Symbol =
      canonicalCache.getOrElseUpdate(symbol, computeCanonical(symbol))

    def availableFor(repr: TypeRepr, exclusions: List[Symbol], gas: Int): List[Available] =
      // Compare exclusions by canonical (de-exported) symbol so that excluding
      // `gossamer.foo` also filters out any re-export like `soundness.foo`.
      val excludedCanonicals = exclusions.map(canonical).filterNot(_.isNoSymbol).toSet

      val matched =
        cachedGivens(repr)
        . filterNot: s => excludedCanonicals.contains(canonical(s))
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
    def computeCanonical(symbol: Symbol): Symbol =
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
    // definition. We can't always trace the export forwarder via `canonical`
    // (binary classes loaded via TASTY expose the symbol's `tree` without its
    // `rhs`), and the forwarder's `info` resolves to the typeclass type while
    // the original's `info` is its synthesised module-class type — so neither
    // canonical-symbol nor result-class matching merges them reliably. Group
    // by simple name instead: within a single typeclass's META-INF file, two
    // givens sharing a name are almost always exports of each other. Within
    // each group, prefer an exported symbol over the original (tie-breaking
    // by shortest stenography rendering).
    def dedupeExportsByCanonical(matched: List[Matched]): List[Matched] =
      matched.groupBy(_.symbol.name.toString.stripSuffix("$")).values.toList.map: group =>
        val exports = group.filter(_.symbol.flags.is(Flags.Exported))
        val candidates = if exports.nonEmpty then exports else group
        candidates.minBy: m => renderSymbol(m.symbol).s.length

    // Same export-preferring dedupe applied to method-level candidates
    // surfaced by the implicit-search-failure trace.
    def dedupeCandidates(candidates: List[Candidate]): List[Candidate] =
      candidates.groupBy(_.symbol.name.toString.stripSuffix("$")).values.toList.flatMap: group =>
        val exports = group.filter(_.symbol.flags.is(Flags.Exported))
        val pool = if exports.nonEmpty then exports else group
        List(pool.minBy{ c => renderSymbol(c.symbol).s.length })

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
      raw.stripSuffix(".type").split('.').iterator
      . filterNot(_.stripSuffix("$").endsWith("$package"))
      . mkString(".").tt

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
        if resultRaw <:< target
        then Some(Matched(symbol, Nil, scala.collection.immutable.Map.empty))
        else None
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
    :   List[List[TypeRepr]] =

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

    def boundsRespected
      ( typeParams: List[Symbol],
        bindings:   scala.collection.immutable.Map[Symbol, TypeRepr] )
    :   Boolean =

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
          val moduleName = current.name.toString.stripSuffix("$")

          val sibling = current.owner.declaredTypes.find: t =>
            t.name.toString == moduleName && t.flags.is(Flags.Opaque)

          sibling.orElse(loop(current.owner))
        else
          loop(current.owner)

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
    def unify(template: TypeRepr, target: TypeRepr, params: scala.collection.immutable.Set[Symbol])
    :   scala.collection.immutable.Map[Symbol, TypeRepr] =

      val templateSymbol = template.typeSymbol

      if params.contains(templateSymbol) then scala.collection.immutable.Map(templateSymbol -> target)
      else (template.dealias, target.dealias) match
        case (AppliedType(tTycon, tArgs), AppliedType(rTycon, rArgs))
        if tArgs.length == rArgs.length =>
          val tyconBindings = unify(tTycon, rTycon, params)

          tArgs.zip(rArgs).foldLeft(tyconBindings): (acc, pair) =>
            acc ++ unify(pair(0), pair(1), params)

        case (tRef: Refinement, rRef: Refinement) =>
          val Refinement(tParent, tName, tInfo) = tRef
          val Refinement(rParent, rName, rInfo) = rRef

          if tName != rName then scala.collection.immutable.Map.empty
          else unify(tParent, rParent, params) ++ unifyInfo(tInfo, rInfo, params)

        case _ =>
          scala.collection.immutable.Map.empty

    def unifyInfo
      ( template: TypeRepr,
        target:   TypeRepr,
        params:   scala.collection.immutable.Set[Symbol] )
    :   scala.collection.immutable.Map[Symbol, TypeRepr] =

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
      // Don't propose any classpath given that the implicit search has already
      // tried as a candidate for this position; doing so would just repeat
      // information already shown under the `▪ candidate` lines.
      val candidateSymbols = candidates.map(_.symbol).filterNot(_.isNoSymbol)

      Missing
        ( stenography.internal.name(repr),
          availableFor(repr, candidateSymbols, initialGas),
          candidates )

    def seek(repr: TypeRepr, exclusions: List[Symbol], depth: Int): Result =
      Implicits.searchIgnoring(repr)((self ::: exclusions)*).absolve match
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
                      ( "???",
                        term.symbol,
                        List(seek(right.head.tpe, term.symbol :: exclusions, depth + 1)) )

                case _ =>
                  missing(repr, Nil)


            // The `Apply @unchecked` test matches non-`Apply` trees too (the
            // unchecked bind skips the runtime check), so a `TypeApply` — e.g.
            // from a polymorphic given whose using-clause failed — reaches
            // `Apply.unapply`, which throws a `MatchError`. Guard the extractor
            // so Frontier degrades to a plain `missing` rather than crashing
            // expansion on any tree shape it doesn't recognise.
            case apply: Apply @unchecked =>
              try apply match
                case Apply(function, arguments) =>
                  def resolve(methodType: TypeRepr): Missing = methodType match
                    case PolyType(_, _, _) =>
                      resolve(function.tpe.simplified)

                    case MethodType(_, types, more) =>
                      val candidate =
                        Candidate
                          ( renderSymbol(function.symbol),
                            function.symbol,
                            types.map(seek(_, Nil, depth + 1)) )

                      val candidates = seek(repr, function.symbol :: exclusions, depth) match
                        case Missing(_, _, candidates) => candidate :: candidates
                        case _                         => Nil

                      missing(repr, dedupeCandidates(candidates))

                    case _ =>
                      missing(repr, Nil)

                  resolve(function.symbol.info)

              catch case _: Throwable => missing(repr, Nil)

            case _ =>
              missing(repr, Nil)


    // Convert the compiler-typed search `Result` into the format-neutral,
    // pre-rendered `Diagnostic` model the shared renderer consumes.
    def toDiagnostic(result: Result): Diagnostic = result match
      case Found(name, _) =>
        Diagnostic.Found(name, Unset, proscenium.Nil)

      case Available(name, requirements) =>
        Diagnostic.Propose(name, Unset, proscenium.List.of(requirements.map(toDiagnostic)))

      case Candidate(name, _, missing) =>
        Diagnostic.Candidate(name, Unset, proscenium.List.of(missing.map(toDiagnostic)))

      case Missing(name, available, candidates) =>
        val children = available.map(toDiagnostic) ++ candidates.map(toDiagnostic)
        Diagnostic.Requires(name, Unset, proscenium.List.of(children))

    // Build the diagnostic tree rooted at the type Frontier was asked to
    // resolve, with its tried candidates and proposed alternatives beneath it.
    // The catch-all fires at the *deepest* missing implicit in any using-clause
    // chain (the inner search resolves through the catch-all first), and the
    // chain that led there isn't available at macro-expansion time, so the
    // root is that deepest type — Frontier reports the innermost cause.
    def buildDiagnostic(missing: Missing): Diagnostic =
      val children = missing.available.map(toDiagnostic) ++ missing.candidates.map(toDiagnostic)
      Diagnostic.Resolving(missing.name, Unset, proscenium.List.of(children))

    // The transparent catch-all cannot emit a terminal error from inside the
    // nested implicit search where it fires (Dotty buffers and shadows it).
    // Instead it returns a call to the *non-transparent* `fail`, which expands
    // later, in the `inlining` phase, where `report.errorAndAbort` is terminal
    // and always reaches the user.
    def emit(missing: Missing): Expr[target] =
      val text = Diagnostic.render(buildDiagnostic(missing)).s
      '{frontier.internal.fail[target](${Expr(text)})}

    seek(TypeRepr.of[target], Nil, 1).absolve match
      case Found(_, expr) =>
        // Return the resolved term at its own (precise) static type, not
        // widened to `target`; `transparent` then surfaces the precise type to
        // the call site.
        expr.asInstanceOf[Expr[target]]

      case m: Missing =>
        emit(m)

      case c: Candidate =>
        emit(Missing(stenography.internal.name[target], Nil, List(c)))

      case a: Available =>
        emit(Missing(stenography.internal.name[target], List(a), Nil))
