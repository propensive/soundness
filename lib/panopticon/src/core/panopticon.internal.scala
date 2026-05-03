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

    /** A path step in a parsed `.lens` lambda. `FieldStep` is a `selectDynamic`/
      * `updateDynamic` call; `TraversalStep` carries the operand of an `applyDynamic`/
      * `update`/`apply` call together with its TypeRepr (the singleton type of the
      * operand, used as the merge identity).
      */
    sealed trait Step
    case class FieldStep(name: String) extends Step
    case class TraversalStep(operand: Term, operandTpe: TypeRepr) extends Step

    def strip(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strip(inner)
      case Block(Nil, expr)       => strip(expr)
      case Typed(expr, _)         => strip(expr)
      case other                  => other

    def isSingleton(tpe: TypeRepr): Boolean = tpe.widen != tpe

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

    /** Strips `Apply` and `TypeApply` layers off a term, recording the term-arg lists in
      * order. After typer, dynamic/traversal calls have several nested
      * Apply/TypeApply layers; the matchers below identify the call shape from the
      * innermost `Select(receiver, methodName)` plus the recorded term-arg lists.
      */
    def stripApplyLayers(t: Term): (Term, List[List[Term]]) =
      var cur: Term = t
      val termArgss = scala.collection.mutable.ListBuffer.empty[List[Term]]
      var done = false
      while !done do
        cur match
          case Apply(inner, args)  => termArgss.prepend(args); cur = inner
          case TypeApply(inner, _) => cur = inner
          case _                   => done = true
      (cur, termArgss.toList)

    /** Matches `receiver.applyDynamic("name")(<using lens>)(traversal)(<using optical>)`.
      * Term-arg layout: `[(name)] [(using lens)] [(traversal)] [(using optical)]`. */
    def matchApplyDynamic(t: Term): Option[(Term, String, Term)] =
      val (core, termArgss) = stripApplyLayers(t)
      core match
        case Select(receiver, "applyDynamic") if termArgss.length >= 3 =>
          (termArgss(0), termArgss(2)) match
            case (List(Literal(StringConstant(name))), List(traversalArg)) =>
              Some((receiver, name, traversalArg))
            case _ => None
        case _ => None

    /** Matches `receiver.update[T](traversal, value)(<using optical>)` — the terminal
      * write through a traversal (`_.field(Prim) = value`). */
    def matchUpdate(t: Term): Option[(Term, Term, Term)] =
      val (core, termArgss) = stripApplyLayers(t)
      core match
        case Select(receiver, "update") if termArgss.nonEmpty =>
          termArgss.head match
            case List(traversalArg, valueArg) => Some((receiver, traversalArg, valueArg))
            case _                            => None
        case _ => None

    /** Matches `receiver.apply[T, O](traversal)(<using optical>)` — the bare
      * `_(traversal)` form (one traversal step, no field). */
    def matchOpticApply(t: Term): Option[(Term, Term)] =
      val (core, termArgss) = stripApplyLayers(t)
      core match
        case Select(receiver, "apply") if termArgss.nonEmpty =>
          termArgss.head match
            case List(traversalArg) => Some((receiver, traversalArg))
            case _                  => None
        case _ => None

    /** Walks a non-terminal chain, building the list of steps from the parameter
      * outward. Returns None if any step is unrecognised, or if a TraversalStep
      * operand isn't singleton-typed (we can't safely fuse non-singleton operands).
      */
    def gatherSteps(body: Term, paramSym: Symbol): Option[List[Step]] =
      val s = strip(body)
      s match
        case Ident(_) if s.symbol == paramSym => Some(Nil)
        case _ => matchSelectDynamic(s) match
          case Some((receiver, name)) =>
            gatherSteps(receiver, paramSym).map(_ :+ FieldStep(name))
          case None => matchApplyDynamic(s) match
            case Some((receiver, name, opTerm)) if isSingleton(opTerm.tpe) =>
              gatherSteps(receiver, paramSym).map: steps =>
                steps :+ FieldStep(name) :+ TraversalStep(opTerm, opTerm.tpe)
            case Some(_) => None  // non-singleton operand — can't fuse
            case None    => matchOpticApply(s) match
              case Some((receiver, opTerm)) if isSingleton(opTerm.tpe) =>
                gatherSteps(receiver, paramSym).map: steps =>
                  steps :+ TraversalStep(opTerm, opTerm.tpe)
              case Some(_) => None
              case None    => None

    def parseChain(body: Term, paramSym: Symbol): Option[(List[Step], Term, Boolean)] =
      matchUpdateDynamic(body) match
        case Some((receiver, name, leaf)) =>
          // updateDynamic's value param is a context function `(T aka "prior") ?=> T`.
          gatherSteps(receiver, paramSym).map: prefix =>
            (prefix :+ FieldStep(name), leaf, true)
        case None => matchUpdate(body) match
          case Some((receiver, opTerm, leaf)) if isSingleton(opTerm.tpe) =>
            // update's value param is a plain T.
            gatherSteps(receiver, paramSym).map: prefix =>
              (prefix :+ TraversalStep(opTerm, opTerm.tpe), leaf, false)
          case Some(_) => None
          case None    => None

    def parseLambda(lam: Expr[Any]): Option[(List[Step], Term, Boolean)] =
      strip(lam.asTerm) match
        case Block(List(DefDef(_, paramss, _, Some(body))), _) =>
          paramss.head match
            case TermParamClause(List(p)) => parseChain(strip(body), p.symbol)
            case _                        => None
        case _ => None

    // ─── tree types (live inside this method scope) ──────────────────────

    /** Parsed prefix tree, before resolution. */
    sealed trait Branch
    case class StepB(step: Step, children: List[Branch]) extends Branch
    /** A leaf write. `isCtxFn` is true when the leaf is the `(T aka "prior") ?=> T`
      * context function produced by `updateDynamic(name)(value)`; false when it's a
      * plain `T` value produced by `update(traversal, value)`. */
    case class LeafB(leaf: Term, isCtxFn: Boolean) extends Branch

    /** After resolution: a `Lens` (for field steps) or `Optical`+optic (for traversal
      * steps) typeclass instance has been summoned for the appropriate origin type,
      * and the inner Target type is captured.
      */
    sealed trait Resolved
    case class FieldR(name: String, lens: Term, target: TypeRepr, children: List[Resolved])
        extends Resolved
    case class TravR(operand: Term, optical: Term, target: TypeRepr, children: List[Resolved])
        extends Resolved
    case class LeafR(leaf: Term, isCtxFn: Boolean) extends Resolved

    def toBranches(parsed: (List[Step], Term, Boolean)): Branch =
      val (steps, leaf, isCtxFn) = parsed
      steps.foldRight[Branch](LeafB(leaf, isCtxFn)): (step, child) =>
        StepB(step, List(child))

    /** Two adjacent step nodes merge if their step identities match: FieldStep by name,
      * TraversalStep by singleton operand type (`=:=`). */
    def stepEq(a: Step, b: Step): Boolean = (a, b) match
      case (FieldStep(n1), FieldStep(n2))           => n1 == n2
      case (TraversalStep(_, t1), TraversalStep(_, t2)) => t1 =:= t2
      case _                                         => false

    def mergeAdjacent(branches: List[Branch]): List[Branch] =
      branches.foldRight[List[Branch]](Nil):
        case (StepB(s, cs), StepB(s2, cs2) :: rest) if stepEq(s, s2) =>
          StepB(s, mergeAdjacent(cs ++ cs2)) :: rest
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

    /** Summons `(? >: operandTpe) is Optical from originTpe`. Used for traversal steps
      * (`Each`, `Prim`/`Ordinal`, map keys). The Self bound is `>: operandTpe` (not
      * exact equality), matching the wildcard `(? >: traversal.type)` shape that
      * `Optic.applyDynamic` summons at runtime — so e.g. a `Prim.type` operand is
      * served by the `Ordinal is Optical from List[…]` given. Origin is exact.
      */
    def summonOptical(operandTpe: TypeRepr, originTpe: TypeRepr): Option[Term] =
      val refined = Refinement
                     ( Refinement
                        ( TypeRepr.of[Optical], "Self",
                          TypeBounds(operandTpe, TypeRepr.of[Any]) ),
                       "Origin", TypeBounds(originTpe, originTpe) )
      refined.asType match
        case '[opT] => Expr.summon[opT].map(_.asTerm)

    /** Walks the prefix tree, resolving each step. Returns None if any step fails
      * to resolve — the orchestrator then falls back to `applyFold`. */
    def resolveAll(branches: List[Branch], originTpe: TypeRepr): Option[List[Resolved]] =
      val opts: List[Option[Resolved]] = branches.map:
        case LeafB(leaf, isCtxFn) => Some(LeafR(leaf, isCtxFn))
        case StepB(FieldStep(name), children) =>
          summonLens(name, originTpe).flatMap: lensTerm =>
            extractTarget(lensTerm).flatMap: targetTpe =>
              resolveAll(children, targetTpe).map: resolvedChildren =>
                FieldR(name, lensTerm, targetTpe, resolvedChildren)
        case StepB(TraversalStep(operand, _), children) =>
          summonOptical(operand.tpe, originTpe).flatMap: opticalTerm =>
            extractTarget(opticalTerm).flatMap: targetTpe =>
              resolveAll(children, targetTpe).map: resolvedChildren =>
                TravR(operand, opticalTerm, targetTpe, resolvedChildren)

      if opts.forall(_.isDefined) then Some(opts.flatten) else None

    // ─── emit ────────────────────────────────────────────────────────────

    def applyLeaf[T: Type](acc: Expr[T], leafTerm: Term, isCtxFn: Boolean): Expr[T] =
      if isCtxFn then
        // From updateDynamic — leaf is `(T aka "prior") ?=> T`. `aka` is opaque (Tagged),
        // so at runtime tagging is a no-op; we pass `acc` via aka to satisfy the typer.
        '{ ${ leafTerm.asExprOf[(T `aka` "prior") ?=> T] }(using $acc.aka["prior"]) }
      else
        // From update(traversal, value) — leaf is a plain `T`; `acc` is unused.
        leafTerm.asExprOf[T]

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
            case LeafR(leaf, isCtxFn) =>
              applyLeaf[T](acc.asExprOf[T], leaf, isCtxFn)
            case FieldR(name, lens, target, children) =>
              emitFieldUpdate[T](acc.asExprOf[T], name, lens, target, children)
            case TravR(operand, optical, target, children) =>
              emitTraversalUpdate[T](acc.asExprOf[T], operand, optical, target, children)

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

    def emitTraversalUpdate[T: Type]
      ( origin: Expr[T], operand: Term, opticalTerm: Term, targetTpe: TypeRepr,
        children: List[Resolved] )
    :   Expr[T] =
      targetTpe.asType match
        case '[targetT] =>
          // Build the optic by calling `optical.optic(operand)`, then `.modify(origin)
          // { inner => emit(inner, children) }`. We bind the resulting Optic to a val
          // so the typer can dispatch its `modify` method without ambiguity.
          val opticTerm = Apply
                           ( Select.unique(opticalTerm, "optic"), List(operand) )
          val opticTpe = opticTerm.tpe

          val opticSym = Symbol.newVal
                          ( Symbol.spliceOwner, "o$trav", opticTpe, Flags.EmptyFlags,
                            Symbol.noSymbol )
          val opticDef = ValDef(opticSym, Some(opticTerm.changeOwner(opticSym)))
          val opticExpr = Ref(opticSym)
                           . asExprOf[Optic { type Origin = T; type Target = targetT }]

          // Build the inner lambda: (inner: Target) => emit(inner, children)
          val innerLambda = Lambda
                             ( Symbol.spliceOwner,
                               MethodType(List("inner"))
                                ( _ => List(targetTpe), _ => targetTpe ),
                               (lambdaSym, args) =>
                                 val innerRef = args.head.asInstanceOf[Term]
                                                 . asExprOf[targetT]
                                 emit[targetT](innerRef, children).asTerm.changeOwner(lambdaSym) )

          val innerLambdaExpr = innerLambda.asExprOf[targetT => targetT]
          val modifyCall = '{ $opticExpr.modify($origin)($innerLambdaExpr) }.asTerm

          Block(List(opticDef), modifyCall).asExprOf[T]

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
