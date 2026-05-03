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

    sealed trait Branch
    case class FieldB(name: String, children: List[Branch]) extends Branch
    case class LeafB(leaf: Term) extends Branch

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

    // ─── emit ────────────────────────────────────────────────────────────

    def applyLeaf[T: Type](acc: Expr[T], leafTerm: Term): Expr[T] =
      // Leaf is a context function `(T aka "prior") ?=> T`. `aka` is opaque (Tagged), so
      // at runtime tagging is a no-op. We pass `acc` directly via aka to satisfy the typer.
      '{ ${ leafTerm.asExprOf[(T `aka` "prior") ?=> T] }(using $acc.aka["prior"]) }

    def emit[T: Type](origin: Expr[T], branches: List[Branch]): Expr[T] =
      val merged = mergeAdjacent(branches)
      // Each iteration must see `acc` as a cheap reference, otherwise multi-use of the
      // accumulator inside `emitFieldUpdate` (one read + N–1 case-field passthroughs)
      // would cause the previous step's expression to be inlined N times. We bind every
      // non-final intermediate result to a fresh val.
      if merged.isEmpty then origin else
        var acc: Term = origin.asTerm
        val defs = scala.collection.mutable.ListBuffer.empty[Statement]
        val last = merged.length - 1

        merged.zipWithIndex.foreach: (branch, idx) =>
          val nextExpr: Expr[T] = branch match
            case LeafB(leaf)            => applyLeaf[T](acc.asExprOf[T], leaf)
            case FieldB(name, children) => emitFieldUpdate[T](acc.asExprOf[T], name, children)

          if idx < last then
            val sym = Symbol.newVal
                       ( Symbol.spliceOwner, s"v$$$idx", TypeRepr.of[T], Flags.EmptyFlags,
                         Symbol.noSymbol )
            defs += ValDef(sym, Some(nextExpr.asTerm.changeOwner(sym)))
            acc = Ref(sym)
          else acc = nextExpr.asTerm

        if defs.isEmpty then acc.asExprOf[T]
        else Block(defs.toList, acc).asExprOf[T]

    def emitFieldUpdate[T: Type](origin: Expr[T], name: String, children: List[Branch]): Expr[T] =
      val symbol = TypeRepr.of[T].typeSymbol
      val field  = symbol.caseFields.find(_.name == name).getOrElse:
        report.errorAndAbort(s"panopticon.fuse: ${TypeRepr.of[T].show} has no field called $name")

      val make = symbol.companionModule.methodMember("apply").head

      field.info.asType match
        case '[fieldT] =>
          val inSym = Symbol.newVal
                       ( Symbol.spliceOwner, s"v$$$name", TypeRepr.of[fieldT], Flags.EmptyFlags,
                         Symbol.noSymbol )
          val getTerm = origin.asTerm.select(field)
          val inDef   = ValDef(inSym, Some(getTerm.changeOwner(inSym)))
          val inRef   = Ref(inSym).asExprOf[fieldT]

          val updated = emit[fieldT](inRef, children)

          val outSym = Symbol.newVal
                        ( Symbol.spliceOwner, s"v$$$name'", TypeRepr.of[fieldT], Flags.EmptyFlags,
                          Symbol.noSymbol )
          val outDef  = ValDef(outSym, Some(updated.asTerm.changeOwner(outSym)))
          val outRef  = Ref(outSym)

          val params = symbol.caseFields.map: f =>
            if f.name == name then outRef else origin.asTerm.select(f)

          val rebuilt = Ref(symbol.companionModule).select(make).appliedToArgs(params)
          Block(List(inDef, outDef), rebuilt).asExprOf[T]
        case _ =>
          report.errorAndAbort(s"panopticon.fuse: cannot derive type for field $name")

    def emitTop(branches: List[Branch]): Expr[value] =
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
            emitTop(parsed.flatten.map(toBranches))
