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
package prophesy

import scala.collection.immutable as sci
import scala.collection.mutable as scm
import scala.quoted.*
import scala.util.control.NonFatal

import anticipation.*
import stenography.*
import vacuous.*

@unexported
object Elaboration:
  // The resolved given the compiler supplied for one synthesized `using` argument: the head
  // symbol's qualified name (for a derived instance, the derivation method's) and the
  // argument's widened type.
  case class Instance(name: Text, tpe: Syntax)

  // Scans a typed tree for the call sites the typer elaborated beyond what was written,
  // recording each call's inferred type arguments and synthesized `using` arguments. The
  // predicates are chosen to survive TASTy pickling, so the same scan serves a live typer run
  // and unpickled trees alike:
  //
  //  - An inferred type argument carries the whole span of the function tree it was wrapped
  //    around (`wrapInTypeTree`), so `targ.pos == fun.pos`; a written argument always starts
  //    after the callee ends. (`InferredTypeTree`-ness itself does not survive pickling.)
  //  - A synthesized `using` argument is inferred at the application's end position, so it is
  //    zero-extent exactly at the end of the clause's function tree; the clause itself is
  //    recognized by its method type, since `ApplyKind` attachments do not survive pickling.
  //
  // An `Inlined` node contributes its original call and is not descended into: the expansion's
  // trees carry the inline definition's own source positions, which would misattribute its
  // internal calls to this file. The scan never fails: a node that cannot be read is skipped.
  def scan(using quotes: Quotes)(root: quotes.reflect.Tree): List[Elaboration] =
    import quotes.reflect.*

    val calls: scm.LinkedHashMap[(Text, Int, Int), (sci.List[Syntax], sci.List[Instance])] =
      scm.LinkedHashMap()

    def peel(term: Term): Term = term match
      case Apply(fun, _)     => peel(fun)
      case TypeApply(fun, _) => peel(fun)
      case _                 => term

    // The callee's own token position: the name segment of a selection, or the whole
    // identifier. Approximated as the trailing `name.length` characters of a selection, which
    // misplaces backticked and interpolated names; harmless, since a mismatched key attaches to
    // no token.
    def callee(term: Term): Optional[(Text, Int, Int)] = peel(term) match
      case select @ Select(_, name) =>
        val pos = select.pos
        if pos.end <= 0 then Unset else (name.tt, (pos.end - name.length).max(0), pos.end)

      case ident @ Ident(name) =>
        val pos = ident.pos
        if pos.end <= 0 then Unset else (name.tt, pos.start, pos.end)

      case _ =>
        Unset

    def merge(key: (Text, Int, Int), types: sci.List[Syntax], givens: sci.List[Instance]): Unit =
      val (types0, givens0) = calls.getOrElse(key, (sci.List(), sci.List()))
      calls(key) = (types0 ++ types, givens0 ++ givens)

    def record(term: Term): Unit = term match
      case TypeApply(fun, targs) =>
        val inferred = targs.filter: targ =>
          targ.pos.start == fun.pos.start && targ.pos.end == fun.pos.end

        if !inferred.isEmpty then callee(fun).let: key =>
          merge(key, inferred.map { targ => Syntax(targ.tpe) }, sci.List())

      case apply @ Apply(fun, args) =>
        val contextual = fun.tpe.widenTermRefByName match
          case tpe: MethodType => tpe.isImplicit || tpe.isContextual
          case _               => false

        val synthesized = args.filter: arg =>
          arg.pos.start == arg.pos.end && arg.pos.start == fun.pos.end

        if contextual && !synthesized.isEmpty then callee(fun).let: key =>
          val instances = synthesized.map: arg =>
            Instance(peel(arg).symbol.fullName.tt, Syntax(arg.tpe.widenTermRefByName))

          merge(key, sci.List(), instances)

      case _ =>
        ()

    object walker extends TreeAccumulator[Unit]:
      def foldTree(unit: Unit, tree: Tree)(owner: Symbol): Unit = tree match
        case Inlined(call, _, _) =>
          call.foreach(foldTree((), _)(owner))

        case term: Term =>
          try record(term) catch case NonFatal(_) => ()
          foldOverTree((), tree)(owner)

        case _ =>
          foldOverTree((), tree)(owner)

    try walker.foldTree((), root)(root.symbol) catch case NonFatal(_) => ()

    val results = calls.toSeq.map: (key, entry) =>
      Elaboration(key(0), key(1), key(2), List(entry(0)*), List(entry(1)*))

    List(results.sortBy(_.start)*)

// One call site the typer elaborated beyond its source text: `method` is the callee's simple
// name and `start`/`end` its source offsets (the hoverable token), `typeArguments` the
// *inferred* type arguments only, and `givenArguments` the *synthesized* `using` arguments
// only — anything written in the source is never recorded.
case class Elaboration
  ( method:         Text,
    start:          Int,
    end:            Int,
    typeArguments:  List[Syntax],
    givenArguments: List[Elaboration.Instance] )
