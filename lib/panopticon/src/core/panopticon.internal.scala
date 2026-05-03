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
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package panopticon

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

object internal:
  private given realm: Realm = realm"pa"

  /** Shared, non-inlined fallback for `value.lens(...)` when the macro can't statically
    * fuse the updates (e.g. one of the lambdas contains a traversal). Centralising the
    * foldLeft in a single method keeps the JIT's job easy at non-fused call sites.
    */
  def applyFold[value]
    (v: value, lambdas: Seq[(Optic from value onto value) => value => value])
  :   value =
    lambdas.foldLeft(v): (acc, lambda) =>
      lambda(Optic.identity[value])(acc)

  def lens[self: Type, origin <: Product: Type]: Macro[self is Lens from origin] =
    import quotes.reflect.*

    val name: String = TypeRepr.of[self].literal[String].or:
      halt(m"cannot derive non-String field names")

    val symbol = TypeRepr.of[origin].typeSymbol

    val field = symbol.caseFields.find(_.name == name).getOrElse:
      halt(m"${TypeRepr.of[origin].show} has no field called $name")

    val make = symbol.companionModule.methodMember("apply").head

    field.info.asType.absolve match
      case '[target] =>
        ' {
            Lens[self, origin, target]
              ( value => ${'value.asTerm.select(field).asExprOf[target]},
                (origin, value) =>
                  $ {
                      val params = symbol.caseFields.map: field =>
                        if field.name == name then 'value.asTerm else 'origin.asTerm.select(field)

                      Ref(symbol.companionModule).select(make).appliedToArgs(params)
                      . asExprOf[origin]
                    } )
          }


  def fuse[value: Type]
    ( valueExpr:   Expr[value],
      lambdasExpr: Expr[Seq[(Optic from value onto value) => value => value]] )
    (using Quotes): Expr[value] =

    import quotes.reflect.*

    def fallback: Expr[value] =
      '{ panopticon.internal.applyFold[value]($valueExpr, $lambdasExpr) }

    // ─── parse helpers ────────────────────────────────────────────────────

    def strip(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strip(inner)
      case Block(Nil, expr)       => strip(expr)
      case Typed(expr, _)         => strip(expr)
      case other                  => other

    /** Matches `receiver.selectDynamic("name")(<using lens>)`. */
    def matchSelectDynamic(t: Term): Option[(Term, String)] = t match
      case Apply(Apply(Select(receiver, "selectDynamic"), List(Literal(StringConstant(name)))), _)  =>
        Some((receiver, name))
      case Apply(Select(receiver, "selectDynamic"), List(Literal(StringConstant(name))))            =>
        Some((receiver, name))
      case _ => None

    /** Matches `receiver.updateDynamic("name")(<using lens>)(value)`. */
    def matchUpdateDynamic(t: Term): Option[(Term, String, Term)] = t match
      case Apply(
              Apply(Apply(Select(receiver, "updateDynamic"), List(Literal(StringConstant(name)))), _),
              List(value)) =>
        Some((receiver, name, value))
      case Apply(
              Apply(Select(receiver, "updateDynamic"), List(Literal(StringConstant(name)))),
              List(value)) =>
        Some((receiver, name, value))
      case _ => None

    def gatherSelectDynamic(body: Term, paramSym: Symbol): Option[List[String]] =
      val s = strip(body)
      s match
        case Ident(_) if s.symbol == paramSym => Some(Nil)
        case _ => matchSelectDynamic(s) match
          case Some((receiver, name)) => gatherSelectDynamic(receiver, paramSym).map(_ :+ name)
          case None                   => None

    def parseChain(body: Term, paramSym: Symbol): Option[(List[String], Term)] =
      matchUpdateDynamic(body) match
        case Some((receiver, name, leaf)) =>
          gatherSelectDynamic(receiver, paramSym).map(prefix => (prefix :+ name, leaf))
        case None => None

    def parseLambda(lam: Expr[Any]): Option[(List[String], Term)] =
      strip(lam.asTerm) match
        case Block(List(DefDef(_, paramss, _, Some(body))), _) =>
          paramss.head match
            case TermParamClause(List(p)) => parseChain(strip(body), p.symbol)
            case _                        => None
        case _ => None

    // ─── tree types (live inside this method scope) ──────────────────────

    /** Parsed prefix tree, before lens resolution. */
    sealed trait Branch
    case class FieldB(name: String, children: List[Branch]) extends Branch
    case class LeafB(leaf: Term) extends Branch

    /** After resolution: each FieldB's `Lens` typeclass instance has been summoned
      * for the appropriate origin type, and its Target type is captured. */
    sealed trait Resolved
    case class FieldR(name: String, lens: Term, target: TypeRepr, children: List[Resolved])
        extends Resolved
    case class LeafR(leaf: Term) extends Resolved

    def toBranches(parsed: (List[String], Term)): Branch =
      val (fields, leaf) = parsed
      fields.foldRight[Branch](LeafB(leaf)): (name, child) =>
        FieldB(name, List(child))

    def mergeAdjacent(branches: List[Branch]): List[Branch] =
      branches.foldRight[List[Branch]](Nil):
        case (FieldB(n, cs), FieldB(n2, cs2) :: rest) if n == n2 =>
          FieldB(n, mergeAdjacent(cs ++ cs2)) :: rest
        case (b, rest) =>
          b :: rest

    // ─── resolve: summon a Lens for each (name, originTpe) ───────────────

    /** Summons `name.type is Lens from originTpe`, i.e. `Lens { type Self = name.type;
      * type Origin = originTpe }`. Returns the summoned term, or None if no Lens given
      * is in scope (in which case we fall back to the foldLeft path).
      */
    def summonLens(name: String, originTpe: TypeRepr): Option[Term] =
      val nameTpe = ConstantType(StringConstant(name))
      // `name.type is Lens from originT` desugars to `Lens { type Self = name.type;
      // type Origin = originT }`. Type-member refinements need TypeBounds with equal
      // lower and upper bounds.
      val refined = Refinement
                     ( Refinement(TypeRepr.of[Lens], "Self", TypeBounds(nameTpe, nameTpe)),
                       "Origin", TypeBounds(originTpe, originTpe) )
      refined.asType match
        case '[lensT] => Expr.summon[lensT].map(_.asTerm)

    /** Pulls the concrete `Target` type out of a resolved Lens's term. Type-member
      * refinements store their info as `TypeBounds(lo, hi)`; for an alias `type X = T`,
      * `lo == hi == T`, so we return either bound.
      */
    def extractTarget(lensTerm: Term): Option[TypeRepr] =
      def unbounds(info: TypeRepr): TypeRepr = info match
        case TypeBounds(_, hi) => hi
        case other             => other

      def walk(t: TypeRepr): Option[TypeRepr] = t.dealias match
        case Refinement(_, "Target", info) => Some(unbounds(info))
        case Refinement(parent, _, _)      => walk(parent)
        case _                             => None

      walk(lensTerm.tpe)

    /** Walks the prefix tree, summoning a Lens at each step. Returns None if any
      * step fails to resolve — the orchestrator then falls back to `applyFold`. */
    def resolveAll(branches: List[Branch], originTpe: TypeRepr): Option[List[Resolved]] =
      val opts: List[Option[Resolved]] = branches.map:
        case LeafB(leaf) => Some(LeafR(leaf))
        case FieldB(name, children) =>
          summonLens(name, originTpe).flatMap: lensTerm =>
            extractTarget(lensTerm).flatMap: targetTpe =>
              resolveAll(children, targetTpe).map: resolvedChildren =>
                FieldR(name, lensTerm, targetTpe, resolvedChildren)

      if opts.forall(_.isDefined) then Some(opts.flatten) else None

    // ─── emit ────────────────────────────────────────────────────────────

    def applyLeaf[T: Type](acc: Expr[T], leafTerm: Term): Expr[T] =
      // Leaf is a context function `(T aka "prior") ?=> T`. `aka` is opaque (Tagged), so
      // at runtime tagging is a no-op. We pass `acc` directly via aka to satisfy the typer.
      '{ ${ leafTerm.asExprOf[(T `aka` "prior") ?=> T] }(using $acc.aka["prior"]) }

    def emit[T: Type](origin: Expr[T], branches: List[Resolved]): Expr[T] =
      // Each iteration must see `acc` as a cheap reference, otherwise multi-use of the
      // accumulator inside `emitFieldUpdate` (one read + one update, sharing `origin`)
      // would inline the previous step's expression more than once. We bind every
      // non-final intermediate result to a fresh val.
      if branches.isEmpty then origin else
        var acc: Term = origin.asTerm
        val defs = scala.collection.mutable.ListBuffer.empty[Statement]
        val last = branches.length - 1

        branches.zipWithIndex.foreach: (branch, idx) =>
          val nextExpr: Expr[T] = branch match
            case LeafR(leaf) => applyLeaf[T](acc.asExprOf[T], leaf)
            case FieldR(name, lens, target, children) =>
              emitFieldUpdate[T](acc.asExprOf[T], name, lens, target, children)

          if idx < last then
            val sym = Symbol.newVal
                       ( Symbol.spliceOwner, s"v$$$idx", TypeRepr.of[T], Flags.EmptyFlags,
                         Symbol.noSymbol )
            defs += ValDef(sym, Some(nextExpr.asTerm.changeOwner(sym)))
            acc = Ref(sym)
          else acc = nextExpr.asTerm

        if defs.isEmpty then acc.asExprOf[T]
        else Block(defs.toList, acc).asExprOf[T]

    /** If `lensTerm` is `Lens.apply(getLambda, setLambda)` (possibly under Inlined
      * wrappers), peel off the wrappers and return the two lambdas. This catches both
      * `Optic.deref` (which is `transparent inline given` expanding to the case-class
      * Lens constructor) and any user-defined Lens built via `Lens(...)`/`Lens.apply(...)`
      * — including Jacinta's `Lens(_.selectDynamic(name), _.modify(name, _))`.
      */
    def lensConstructorLambdas(lensTerm: Term): Option[(Term, Term)] =
      def stripWrappers(t: Term): Term = t match
        case Inlined(_, Nil, inner) => stripWrappers(inner)
        case Block(Nil, expr)       => stripWrappers(expr)
        case Typed(expr, _)         => stripWrappers(expr)
        case _                      => t

      stripWrappers(lensTerm) match
        case Apply(applyFn, List(getLambda, setLambda))
          if applyFn.symbol.exists && applyFn.symbol.owner == TypeRepr.of[Lens.type].typeSymbol
          && applyFn.symbol.name == "apply" =>
          Some((getLambda, setLambda))
        case _ => None

    def emitFieldUpdate[T: Type]
      (origin: Expr[T], name: String, lensTerm: Term, targetTpe: TypeRepr, children: List[Resolved])
    :   Expr[T] =
      targetTpe.asType match
        case '[targetT] =>
          // Build the get expression. If the lens was constructed via `Lens.apply(getFn,
          // setFn)`, beta-reduce `getFn(origin)` so the typer can inline a direct field
          // access (for case classes) or a direct `selectDynamic` call (for Jacinta's
          // Json lens). Falls back to `lens.apply(origin)` for opaque lens shapes.
          val (lensPrelude, getTerm, doSet) = lensConstructorLambdas(lensTerm) match
            case Some((getLambda, setLambda)) =>
              // Inline-friendly lens: cast lambdas to Function types so the typer
              // resolves apply, then beta-reduce. For case classes this collapses to
              // `origin.field`; for any other inline lens to its own get/set body.
              val getFn = getLambda.asExprOf[T => targetT]
              val setFn = setLambda.asExprOf[(T, targetT) => T]
              val rawGet = '{ $getFn($origin) }.asTerm
              val get = Term.betaReduce(rawGet).getOrElse(rawGet)
              val set: Term => Term = (newValue: Term) =>
                val rawSet = '{ $setFn($origin, ${ newValue.asExprOf[targetT] }) }.asTerm
                Term.betaReduce(rawSet).getOrElse(rawSet)
              (Nil, get, set)

            case None =>
              // Opaque lens (e.g. Jacinta's `given lens`, which isn't inline). Bind it
              // to a val so apply + update share it. Use a quoted block so the typer picks
              // the right overload of apply (Lens.apply takes Origin; Optic.apply takes a
              // traversal — Select.unique can't disambiguate by name).
              val lensSym = Symbol.newVal
                             ( Symbol.spliceOwner, s"l$$$name", lensTerm.tpe, Flags.EmptyFlags,
                               Symbol.noSymbol )
              val lensDef = ValDef(lensSym, Some(lensTerm.changeOwner(lensSym)))
              val lensExpr = Ref(lensSym)
                              . asExprOf[Lens { type Origin = T; type Target = targetT }]
              val getRaw = '{ $lensExpr($origin) }.asTerm
              val set: Term => Term = (newValue: Term) =>
                val newValueExpr = newValue.asExprOf[targetT]
                '{ $lensExpr.update($origin, $newValueExpr) }.asTerm
              (List(lensDef), getRaw, set)

          val inSym = Symbol.newVal
                       ( Symbol.spliceOwner, s"v$$$name", targetTpe, Flags.EmptyFlags,
                         Symbol.noSymbol )
          val inDef   = ValDef(inSym, Some(getTerm.changeOwner(inSym)))
          val inRef   = Ref(inSym).asExprOf[targetT]

          val updated = emit[targetT](inRef, children)

          val outSym = Symbol.newVal
                        ( Symbol.spliceOwner, s"v$$$name'", targetTpe, Flags.EmptyFlags,
                          Symbol.noSymbol )
          val outDef  = ValDef(outSym, Some(updated.asTerm.changeOwner(outSym)))
          val outRef  = Ref(outSym)

          val rebuilt = doSet(outRef)

          Block(lensPrelude ++ List(inDef, outDef), rebuilt).asExprOf[T]

    def emitTop(branches: List[Resolved]): Expr[value] =
      val rootSym  = Symbol.newVal
                      ( Symbol.spliceOwner, "v$root", TypeRepr.of[value], Flags.EmptyFlags,
                        Symbol.noSymbol )
      val rootDef  = ValDef(rootSym, Some(valueExpr.asTerm.changeOwner(rootSym)))
      val rootRef  = Ref(rootSym).asExprOf[value]
      val resultEx = emit[value](rootRef, branches)
      Block(List(rootDef), resultEx.asTerm).asExprOf[value]

    // ─── orchestrate ─────────────────────────────────────────────────────

    Varargs.unapply(lambdasExpr) match
      case None        => fallback
      case Some(exprs) =>
        if exprs.isEmpty then valueExpr else
          val parsed = exprs.toList.map(parseLambda)
          if parsed.exists(_.isEmpty) then
            // Single unparseable lambda: skip building a Seq and calling foldLeft —
            // emit the lambda call directly. Matches the original `def lens`
            // micro-benchmark for a one-shot traversal update.
            if exprs.length == 1 then
              val singleLambda =
                exprs.head.asExprOf[(Optic from value onto value) => value => value]
              '{ $singleLambda(Optic.identity[value])($valueExpr) }
            else fallback
          else
            val merged = mergeAdjacent(parsed.flatten.map(toBranches))
            resolveAll(merged, TypeRepr.of[value]) match
              case Some(resolved) => emitTop(resolved)
              case None           => fallback
