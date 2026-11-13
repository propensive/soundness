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
package probably

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

object internal:
  private def handle[test: Type, result: Type]
    ( test:         Expr[Test[test]^],
      predicate:    Expr[test => Boolean],
      action:       Expr[Trial[test] => result],
      discard:      Boolean,
      verified:     Boolean )
  :   Macro[result] =

    import quotes.reflect.*

    val discardExpr: Expr[Boolean] = Expr(discard)
    val verifiedExpr: Expr[Boolean] = Expr(verified)

    Expr.summon[Runner[?]].getOrElse(halt(m"No `Runner` instance is available")).absolve match
      case '{$runner: Runner[report]} =>

        val inclusion = Expr.summon[Inclusion[report, Verdict]].getOrElse:
          halt(m"Can't embed test verdicts in ${Type.of[report].show}")

        val inclusion2 = Expr.summon[Inclusion[report, Verdict.Detail]].getOrElse:
          halt(m"Can't embed test details in ${Type.of[report].show}")

        // Nothing about how a failure would be rendered is resolved here: that would be paid
        // for at every assertion on every compilation, to serve the few which ever fail. The
        // site records only WHERE its predicate is written — the top-level class enclosing
        // it and the predicate's offsets in the source, which are its offsets in that class's
        // TASTy — and the predicate's text. A `Diagnostician` recovers the rest, if the
        // assertion fails (see `Diagnosis`).
        def unwrap(term: Term): Term = term match
          case Inlined(_, Nil, term) => unwrap(term)
          case term                  => term

        val lambda: Term = unwrap(predicate.asTerm)
        val position: Position = lambda.pos

        def enclosing(symbol: Symbol): Symbol =
          if symbol.maybeOwner.isNoSymbol || symbol.maybeOwner.isPackageDef then symbol
          else enclosing(symbol.maybeOwner)

        val owner: Symbol = enclosing(Symbol.spliceOwner)

        // A predicate which is not written at the assertion — `assert()`'s — has no site.
        val located: Boolean =
          owner.isClassDef && owner.pos.exists: ownerPosition =>
            ownerPosition.sourceFile == position.sourceFile && position.end > position.start

        val ownerName: Expr[String] = Expr(if located then owner.fullName else "")
        val start: Expr[Int] = Expr(if located then position.start else -1)
        val end: Expr[Int] = Expr(if located then position.end else -1)
        val source: Expr[String] = Expr(if located then position.sourceCode.getOrElse("") else "")

        // The locals the predicate closes over — values defined in an enclosing method, and
        // so not reachable from anywhere else — are what a diagnosis cannot recover from the
        // class alone. They are gathered by a thunk which only a failure calls, keyed by
        // name: no two free locals of one predicate can share a name. A predicate with none,
        // which is most of them, gets no thunk.
        val locals: scala.collection.mutable.LinkedHashMap[String, Term] =
          scala.collection.mutable.LinkedHashMap()

        def inside(symbol: Symbol): Boolean = symbol.pos.exists: definition =>
          definition.sourceFile == position.sourceFile && definition.start >= position.start &&
            definition.end <= position.end

        def local(symbol: Symbol): Boolean =
          symbol.isTerm && !symbol.flags.is(Flags.Method) && !symbol.flags.is(Flags.Module) &&
            !symbol.flags.is(Flags.Lazy) && !symbol.maybeOwner.isNoSymbol &&
            (symbol.maybeOwner.isDefDef || symbol.maybeOwner.isValDef)

        object gather extends TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case ident@Ident(name) if local(ident.symbol) && !inside(ident.symbol) =>
                if !locals.contains(name) then locals(name) = Ref(ident.symbol)

              case _ =>
                ()

            super.traverseTree(tree)(owner)

        if located then gather.traverseTree(lambda)(Symbol.spliceOwner)

        val names: Expr[String] = Expr(locals.keys.mkString(","))

        val captures: Expr[Optional[() => List[Any]]] =
          if locals.isEmpty then '{Unset} else
            val values: Expr[List[Any]] =
              '{List[Any](${Varargs(locals.values.map(_.asExpr).toSeq)}*)}

            '{() => $values}

        ' {
            assertion[test, report, result]
              ( $runner,
                $test,
                $predicate,
                $action,
                $inclusion,
                $inclusion2,
                Nil,
                $discardExpr,
                $verifiedExpr,
                $ownerName.tt,
                $start,
                $end,
                $source.tt,
                $names.tt,
                $captures )
          }


  // `check` returns its value into subsequent test code, so an unselected `check` still
  // executes (it just records nothing); `assert` results are discardable, so unselected ones
  // never run at all.
  def check[test: Type](test: Expr[Test[test]^], predicate: Expr[test => Boolean]): Macro[test] =
    handle[test, test](test, predicate, '{(t: Trial[test]) => t.get}, false, false)

  // `assert` takes a pure `Test` (see `Test.assert`); `handle` accepts a capturing one, to
  // which a pure one conforms, so the runtime path is shared with `check`. The
  // receiver passes through `pure`, a method that is NOT inline: the capture checker does not
  // hold an inline method's arguments to its parameter types (the call has been expanded by
  // then), so `assert`'s own signature would not reject a capturing test. A call to a plain
  // method inside the expansion does, at the assertion site.
  //
  // The gate only bites where the unit is capture checked. Whether it was is recorded in the
  // expansion (`verified`): a runner defers only assertions the compiler has actually seen to
  // be pure, so a suite compiled without capture checking keeps running inline, in order —
  // its assertions may close over anything, and one queued past its scope could hang a run.
  def assert[test: Type](test: Expr[Test[test]], predicate: Expr[test => Boolean]): Macro[Unit] =
    handle[test, Unit]('{pure[test]($test)}, predicate, '{_ => ()}, true, captureChecked)

  private def captureChecked(using Quotes): Boolean =
    val context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    dotty.tools.dotc.config.Feature.ccEnabled(using context)

  def pure[test](test: Test[test]): Test[test] = test

  def succeed: Any => Boolean = (value: Any) => true


  def assertion[test, report, result]
    ( runner:       Runner[report],
      test:         Test[test]^,
      predicate:    test => Boolean,
      result:       Trial[test] => result,
      inc:          Inclusion[report, Verdict],
      inc2:         Inclusion[report, Verdict.Detail],
      coordinates:  List[(Axis.Spec, Axis.Value)],
      discard:      Boolean,
      verified:     Boolean,
      owner:        Text,
      start:        Int,
      end:          Int,
      source:       Text,
      names:        Text,
      captures:     Optional[() => List[Any]] )
  :   result =

    val selected = !runner.skip(test.id, Entry.Kind.Check, coordinates)

    // Within an `aspirationally` block, every verdict is recorded as an aspiration.
    val aspirational = runner.aspirational

    // What a failure of the predicate on `value` is found to be, when the host asked for
    // failures to be diagnosed: the installed `Diagnostician`'s account of it, or, with no
    // engine to ask (or none that could say anything), the value as it prints itself.
    def diagnose(value: test): Unit =
      val request = runner.diagnosis

      if !request.nil then
        val site = Diagnosis.Site(owner, start, end, source)

        val details: List[Verdict.Detail] =
          try
            Diagnostician().lay(Nil): diagnostician =>
              val locals: List[(Text, Any)] = captures.lay(Nil): thunk =>
                names.cut(",".tt).zip(thunk())

              diagnostician.diagnose(site, value, locals, request)

          catch case error: Throwable => Nil

        val details2: List[Verdict.Detail] =
          if !details.nil then details
          else List(Verdict.Detail.Message(java.lang.String.valueOf(value).nn.tt))

        details2.each: detail => inc2.include(runner.report, test.id, coordinates, detail)

    // A discardable, unselected assertion never runs; an unselected `check` must still run
    // (its value flows onward) but records nothing. The cast is sound: `discard` is true
    // only for `assert` (and spreads), whose `result` is `Unit`.
    //
    def execute(): result =
      runner.run(test).pipe: run =>
        val verdict = run match
          case Trial.Throws(err, duration, map) =>
            val exception: Exception = try err() catch case exc: Exception => exc

            if !map.nil && selected
            then inc2.include(runner.report, test.id, coordinates, Verdict.Detail.Captures(map))

            if aspirational then Verdict.AspireFail(duration)
            else Verdict.Throws(exception, duration)

          case Trial.Returns(value, duration, map) =>
            try
              if predicate(value) then
                if aspirational then Verdict.AspirePass(duration) else Verdict.Pass(duration)
              else
                if selected then
                  diagnose(value)

                  if !map.nil
                  then
                    inc2.include(runner.report, test.id, coordinates, Verdict.Detail.Captures(map))

                if aspirational then Verdict.AspireFail(duration) else Verdict.Fail(duration)
            catch case error: Exception =>
              if selected then diagnose(value)

              if aspirational then Verdict.AspireFail(duration)
              else Verdict.CheckThrows(error, duration)

        if selected then inc.include(runner.report, test.id, coordinates, verdict)
        result(run)

    // A selected, discardable assertion — an `assert`, aspirational or not, whose body the
    // compiler has VERIFIED pure (see `Test.assert` and `internal.assert`) — is QUEUED when
    // the runner has workers: the traversal carries on and a worker runs it later. `check`
    // (its value flows onward) and an assertion from a unit compiled without capture checking
    // always run inline.
    if discard && !selected then ().asInstanceOf[result]
    else if discard && verified && runner.queued then
      runner.defer(test.id, () => execute())
      ().asInstanceOf[result]
    else
      execute()
