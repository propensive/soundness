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
package panopticon

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import prepositional.*
import rudiments.*
import vacuous.*

object internal:
  def applyFold[value]
    ( v: value, lambdas: Seq[(Optic from value onto value) => value => value] )
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
    ( using Quotes )
  :   Expr[value] =

    import quotes.reflect.*

    def fallback: Expr[value] =
      '{panopticon.internal.applyFold[value]($valueExpr, $lambdasExpr)}

    sealed trait Step
    case class FieldStep(name: String) extends Step
    case class TraversalStep(operand: Term, operandTpe: TypeRepr) extends Step

    def strip(t: Term): Term = t match
      case Inlined(_, Nil, inner) => strip(inner)
      case Block(Nil, expr)       => strip(expr)
      case Typed(expr, _)         => strip(expr)
      case other                  => other

    def isSingleton(tpe: TypeRepr): Boolean = tpe.widen != tpe

    def matchSelectDynamic(t: Term): Option[(Term, String)] = t match
      case Apply(Apply(Select(recv, "selectDynamic"), List(Literal(StringConstant(name)))), _) =>
        Some((recv, name))

      case Apply(Select(receiver, "selectDynamic"), List(Literal(StringConstant(name)))) =>
        Some((receiver, name))

      case _ => None

    def matchUpdateDynamic(t: Term): Option[(Term, String, Term)] = t match
      case Apply(
              Apply(Apply(Select(receiver, "updateDynamic"),
                          List(Literal(StringConstant(name)))), _),
              List(value)) =>
        Some((receiver, name, value))

      case Apply(
              Apply(Select(receiver, "updateDynamic"), List(Literal(StringConstant(name)))),
              List(value)) =>
        Some((receiver, name, value))

      case _ => None

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

    def matchApplyDynamic(t: Term): Option[(Term, String, Term)] =
      val (core, termArgss) = stripApplyLayers(t)

      core match
        case Select(receiver, "applyDynamic") if termArgss.length >= 3 =>
          (termArgss(0), termArgss(2)) match
            case (List(Literal(StringConstant(name))), List(traversalArg)) =>
              Some((receiver, name, traversalArg))

            case _ => None

        case _ => None

    def matchUpdate(t: Term): Option[(Term, Term, Term)] =
      val (core, termArgss) = stripApplyLayers(t)

      core match
        case Select(receiver, "update") if termArgss.nonEmpty =>
          termArgss.head match
            case List(traversalArg, valueArg) => Some((receiver, traversalArg, valueArg))
            case _                            => None

        case _ => None

    def matchOpticApply(t: Term): Option[(Term, Term)] =
      val (core, termArgss) = stripApplyLayers(t)

      core match
        case Select(receiver, "apply") if termArgss.nonEmpty =>
          termArgss.head match
            case List(traversalArg) => Some((receiver, traversalArg))
            case _                  => None

        case _ => None

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

            case Some(_) =>
              None

            case None =>
              matchOpticApply(s) match
                case Some((receiver, opTerm)) if isSingleton(opTerm.tpe) =>
                  gatherSteps(receiver, paramSym).map: steps =>
                    steps :+ TraversalStep(opTerm, opTerm.tpe)

                case Some(_) => None
                case None    => None

    def parseChain(body: Term, paramSym: Symbol): Option[(List[Step], Term, Boolean)] =
      matchUpdateDynamic(body) match
        case Some((receiver, name, leaf)) =>
          gatherSteps(receiver, paramSym).map: prefix =>
            (prefix :+ FieldStep(name), leaf, true)

        case None => matchUpdate(body) match
          case Some((receiver, opTerm, leaf)) if isSingleton(opTerm.tpe) =>
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

    sealed trait Branch
    case class StepB(step: Step, children: List[Branch]) extends Branch
    case class LeafB(leaf: Term, isCtxFn: Boolean) extends Branch

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

    def stepEq(a: Step, b: Step): Boolean = (a, b) match
      case (FieldStep(n1), FieldStep(n2))               => n1 == n2
      case (TraversalStep(_, t1), TraversalStep(_, t2)) => t1 =:= t2
      case _                                            => false

    def mergeAdjacent(branches: List[Branch]): List[Branch] =
      branches.foldRight[List[Branch]](Nil):
        case (StepB(s, cs), StepB(s2, cs2) :: rest) if stepEq(s, s2) =>
          StepB(s, mergeAdjacent(cs ++ cs2)) :: rest

        case (b, rest) =>
          b :: rest

    def summonLens(name: String, originTpe: TypeRepr): Option[Term] =
      val nameTpe = ConstantType(StringConstant(name))

      val refined =
        Refinement
          ( Refinement(TypeRepr.of[Lens], "Self", TypeBounds(nameTpe, nameTpe)),
            "Origin", TypeBounds(originTpe, originTpe) )

      refined.asType.absolve match
        case '[lens] => Expr.summon[lens].map(_.asTerm)

    def extractTarget(lensTerm: Term): Option[TypeRepr] =
      def unbounds(info: TypeRepr): TypeRepr = info match
        case TypeBounds(_, hi) => hi
        case other             => other

      def walk(t: TypeRepr): Option[TypeRepr] = t.dealias match
        case Refinement(_, "Target", info) => Some(unbounds(info))
        case Refinement(parent, _, _)      => walk(parent)
        case _                             => None

      walk(lensTerm.tpe)

    def summonOptical(operandTpe: TypeRepr, originTpe: TypeRepr): Option[Term] =
      val refined =
        Refinement
          ( Refinement
             ( TypeRepr.of[Optical], "Self",
               TypeBounds(operandTpe, TypeRepr.of[Any]) ),
            "Origin", TypeBounds(originTpe, originTpe) )

      refined.asType.absolve match
        case '[opT] => Expr.summon[opT].map(_.asTerm)

    def resolveAll(branches: List[Branch], originTpe: TypeRepr): Option[List[Resolved]] =
      val opts: List[Option[Resolved]] = branches.map:
        case LeafB(leaf, isCtxFn) => Some(LeafR(leaf, isCtxFn))

        case StepB(FieldStep(name), children) =>
          summonLens(name, originTpe).flatMap: lensTerm =>
            extractTarget(lensTerm).flatMap: targetType =>
              resolveAll(children, targetType).map: resolvedChildren =>
                FieldR(name, lensTerm, targetType, resolvedChildren)

        case StepB(TraversalStep(operand, _), children) =>
          summonOptical(operand.tpe, originTpe).flatMap: opticalTerm =>
            extractTarget(opticalTerm).flatMap: targetType =>
              resolveAll(children, targetType).map: resolvedChildren =>
                TravR(operand, opticalTerm, targetType, resolvedChildren)

      if opts.forall(_.isDefined) then Some(opts.flatten) else None


    def coerce[T: Type](sourceTerm: Term): Expr[T] =
      val sourceTpe = sourceTerm.tpe.widen

      if sourceTpe <:< TypeRepr.of[T] then sourceTerm.asExprOf[T]
      else sourceTpe.asType.absolve match
        case '[source] =>
          val sourceExpr = sourceTerm.asExprOf[source]

          Expr.summon[source is Coercible to T] match
            case Some(coercible) => '{$coercible.coerce($sourceExpr)}

            case None =>
              halt(m"cannot coerce ${sourceTpe.show} to ${TypeRepr.of[T].show} in this assignment")

    def applyLeaf[T: Type](acc: Expr[T], leafTerm: Term, isCtxFn: Boolean): Expr[T] =
      if isCtxFn then
        val sourceTpe = leafTerm.tpe match
          case AppliedType(_, List(_, result)) => result
          case other                           => other

        sourceTpe.asType.absolve match
          case '[source] =>
            val fn = leafTerm.asExprOf[(T `aka` "prior") ?=> source]
            coerce[T]('{$fn(using $acc.aka["prior"])}.asTerm)
      else
        coerce[T](leafTerm)

    def emit[T: Type](origin: Expr[T], branches: List[Resolved]): Expr[T] =
      if branches.nil then origin else
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
            val symbol =
              Symbol.newVal
                ( Symbol.spliceOwner, s"v$$$idx", TypeRepr.of[T], Flags.EmptyFlags,
                  Symbol.noSymbol )

            defs += ValDef(symbol, Some(nextExpr.asTerm.changeOwner(symbol)))
            acc = Ref(symbol)
          else
            acc = nextExpr.asTerm

        if defs.isEmpty then acc.asExprOf[T]
        else Block(defs.toList, acc).asExprOf[T]

    def lensConstructorLambdas(lensTerm: Term): Option[(Term, Term)] =
      def stripWrappers(t: Term): Term = t match
        case Inlined(_, Nil, inner) => stripWrappers(inner)
        case Block(Nil, expr)       => stripWrappers(expr)
        case Typed(expr, _)         => stripWrappers(expr)
        case _                      => t

      stripWrappers(lensTerm) match
        case Apply(applyFn, List(getLambda, setLambda))
          if
            applyFn.symbol.exists && applyFn.symbol.owner == TypeRepr.of[Lens.type].typeSymbol &&
              applyFn.symbol.name == "apply"
        =>
          Some((getLambda, setLambda))

        case _ => None

    def emitFieldUpdate[source: Type]
      ( origin:     Expr[source],
        name:       String,
        lensTerm:   Term,
        targetType: TypeRepr,
        children:   List[Resolved] )
    :   Expr[source] =

      targetType.asType.absolve match
        case '[target] =>
          val (lensPrelude, getTerm, doSet) = lensConstructorLambdas(lensTerm) match
            case Some((getLambda, setLambda)) =>
              val getFn = getLambda.asExprOf[source => target]
              val setFn = setLambda.asExprOf[(source, target) => source]
              val rawGet = '{$getFn($origin)}.asTerm
              val get = Term.betaReduce(rawGet).getOrElse(rawGet)

              val set: Term => Term = (newValue: Term) =>
                val rawSet = '{$setFn($origin, ${newValue.asExprOf[target]})}.asTerm
                Term.betaReduce(rawSet).getOrElse(rawSet)

              (Nil, get, set)

            case None =>
              val lensSym =
                Symbol.newVal
                  ( Symbol.spliceOwner, s"l$$$name", lensTerm.tpe, Flags.EmptyFlags,
                    Symbol.noSymbol )

              val lensDef = ValDef(lensSym, Some(lensTerm.changeOwner(lensSym)))
              val lensExpr = Ref(lensSym).asExprOf[Lens from source onto target]
              val getRaw = '{$lensExpr($origin)}.asTerm

              val set: Term => Term = (newValue: Term) =>
                val newValueExpr = newValue.asExprOf[target]
                '{$lensExpr.update($origin, $newValueExpr)}.asTerm

              (List(lensDef), getRaw, set)

          val inSym =
            Symbol.newVal
              ( Symbol.spliceOwner, s"v$$$name", targetType, Flags.EmptyFlags,
                Symbol.noSymbol )

          val inDef   = ValDef(inSym, Some(getTerm.changeOwner(inSym)))
          val inRef   = Ref(inSym).asExprOf[target]

          val updated = emit[target](inRef, children)

          val outSym =
            Symbol.newVal
              ( Symbol.spliceOwner, s"v$$$name'", targetType, Flags.EmptyFlags,
                Symbol.noSymbol )

          val outDef  = ValDef(outSym, Some(updated.asTerm.changeOwner(outSym)))
          val outRef  = Ref(outSym)

          val rebuilt = doSet(outRef)

          Block(lensPrelude ++ List(inDef, outDef), rebuilt).asExprOf[source]

    def emitTraversalUpdate[T: Type]
      ( origin: Expr[T], operand: Term, opticalTerm: Term, targetType: TypeRepr,
        children: List[Resolved] )
    :   Expr[T] =

      targetType.asType.absolve match
        case '[target] =>
          val opticTerm = Apply(Select.unique(opticalTerm, "optic"), List(operand))
          val opticTpe = opticTerm.tpe

          val opticSym =
            Symbol.newVal(Symbol.spliceOwner, "o$trav", opticTpe, Flags.EmptyFlags, Symbol.noSymbol)

          val opticDef = ValDef(opticSym, Some(opticTerm.changeOwner(opticSym)))
          val opticExpr = Ref(opticSym)
            . asExprOf[Optic from T onto target]

          val innerLambda =
            Lambda
              ( Symbol.spliceOwner,
                MethodType(List("inner"))
                 ( _ => List(targetType), _ => targetType ),
                (lambdaSym, args) =>
                  val innerRef = args.head.asInstanceOf[Term]
                                  . asExprOf[target]
                  emit[target](innerRef, children).asTerm.changeOwner(lambdaSym) )

          val innerLambdaExpr = innerLambda.asExprOf[target => target]
          val modifyCall = '{$opticExpr.modify($origin)($innerLambdaExpr)}.asTerm

          Block(List(opticDef), modifyCall).asExprOf[T]

    def emitTop(branches: List[Resolved]): Expr[value] =
      val rootSym  =
        Symbol.newVal
          ( Symbol.spliceOwner, "v$root", TypeRepr.of[value], Flags.EmptyFlags,
            Symbol.noSymbol )

      val rootDef  = ValDef(rootSym, Some(valueExpr.asTerm.changeOwner(rootSym)))
      val rootRef  = Ref(rootSym).asExprOf[value]
      val resultEx = emit[value](rootRef, branches)
      Block(List(rootDef), resultEx.asTerm).asExprOf[value]

    Varargs.unapply(lambdasExpr) match
      case None => fallback

      case Some(exprs) =>
        if exprs.nil then valueExpr else
          val parsed = exprs.toList.map(parseLambda)

          if parsed.exists(_.nil) then
            if exprs.length == 1 then
              val lambda = exprs.head.asExprOf[(Optic from value onto value) => value => value]

              '{$lambda(Optic.identity[value])($valueExpr)}
            else
              fallback
          else
            val merged = mergeAdjacent(parsed.flatten.map(toBranches))

            resolveAll(merged, TypeRepr.of[value]) match
              case Some(resolved) => emitTop(resolved)
              case None           => fallback
