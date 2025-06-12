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
┃    Soundness, version 0.33.0.                                                                    ┃
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

import anticipation.*
import chiaroscuro.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object Probably:
  protected def general[test: Type, report: Type, result: Type]
                 (test:      Expr[Test[test]],
                  predicate: Expr[test => Boolean],
                  runner:    Expr[Runner[report]],
                  inc:       Expr[Inclusion[report, Verdict]],
                  inc2:      Expr[Inclusion[report, Verdict.Detail]],
                  action:    Expr[Trial[test] => result])
                 (using Quotes)
  : Expr[result] =

      import quotes.reflect.*

      def decompose(predicate: Expr[Any]): Option[Expr[Any]] = predicate.asTerm match
        case Inlined(_, _, predicate) => decompose(predicate.asExpr)
        case Block(List(DefDef(a1, _, _, Some(expression))), Closure(Ident(a2), _)) if a1 == a2 =>
          expression match
            case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExpr)
            case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExpr)
            case other                                     => None

        case other =>
          None

      val exp: Option[Expr[Any]] = decompose(predicate)

      exp match
        case Some('{type testType >: test; $expr: testType}) =>
          val decomposable: Expr[testType is Decomposable] =
            Expr.summon[testType is Decomposable].getOrElse('{Decomposable.primitive[testType]})

          val contrast = Expr.summon[testType is Contrastable].getOrElse:
            halt(m"Can't find a `Contrastable` instance for ${Type.of[testType]}")

          '{
            assertion[testType, test, report, result]
             ($runner,
              $test,
              $predicate,
              $action,
              $contrast,
              Some($expr),
              $inc,
              $inc2,
              $decomposable) }

        case _ =>
          '{
            assertion[test, test, report, result]
             ($runner,
              $test,
              $predicate,
              $action,
              Contrastable.nothing[test],
              None,
              $inc,
              $inc2,
              Decomposable.primitive[test])
          }


  def check[test: Type, report: Type]
       (test:      Expr[Test[test]],
        predicate: Expr[test => Boolean],
        runner:    Expr[Runner[report]],
        inc:       Expr[Inclusion[report, Verdict]],
        inc2:      Expr[Inclusion[report, Verdict.Detail]])
       (using Quotes)
  : Expr[test] =

      general[test, report, test]
       (test, predicate, runner, inc, inc2, '{ (t: Trial[test]) => t.get })


  def assert[test: Type, report: Type]
       (test:      Expr[Test[test]],
        predicate: Expr[test => Boolean],
        runner:    Expr[Runner[report]],
        inc:       Expr[Inclusion[report, Verdict]],
        inc2:      Expr[Inclusion[report, Verdict.Detail]])
       (using Quotes)
  : Expr[Unit] =

      general[test, report, Unit](test, predicate, runner, inc, inc2, '{ _ => () })


  def aspire[test: Type, report: Type]
       (test:   Expr[Test[test]],
        runner: Expr[Runner[report]],
        inc:    Expr[Inclusion[report, Verdict]],
        inc2:   Expr[Inclusion[report, Verdict.Detail]])
       (using Quotes)
  : Expr[Unit] =

      general[test, report, Unit](test, '{ _ => true }, runner, inc, inc2, '{ _ => () })


  def succeed: Any => Boolean = (value: Any) => true


  def assertion[test, test2 <: test, report, result]
       (runner:       Runner[report],
        test:         Test[test2],
        predicate:    test2 => Boolean,
        result:       Trial[test2] => result,
        contrast:     test is Contrastable,
        exp:          Option[test],
        inc:          Inclusion[report, Verdict],
        inc2:         Inclusion[report, Verdict.Detail],
        decomposable: test is Decomposable)
  : result =

      runner.run(test).pipe: run =>
        val verdict = run match
          case Trial.Throws(err, duration, map) =>
            val exception: Exception = try err() catch case exc: Exception => exc
            if !map.isEmpty then inc2.include(runner.report, test.id, Verdict.Detail.Captures(map))
            Verdict.Throws(exception, duration)

          case Trial.Returns(value, duration, map) =>
            try if predicate(value) then Verdict.Pass(duration) else
              exp match
                case Some(exp) =>
                  inc2.include
                   (runner.report,
                    test.id,
                    Verdict.Detail.Compare
                     (decomposable.decompose(exp).text,
                      decomposable.decompose(value).text,
                      contrast.contrast(exp, value)))
                case None =>
                  // inc2.include(runner.report, test.id, Verdict.Detail.Compare
                  //  (summon[Any is Contrastable].compare(value, 1)))

              if !map.isEmpty
              then inc2.include(runner.report, test.id, Verdict.Detail.Captures(map))

              Verdict.Fail(duration)
            catch case err: Exception => Verdict.CheckThrows(err, duration)

        inc.include(runner.report, test.id, verdict)
        result(run)


  def debug[test: Type](expr: Expr[test], test: Expr[Harness])(using Quotes): Expr[test] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition =>
        pos.lineContent.show.segment(pos.startColumn.z ~ Ordinal.natural(pos.endColumn))

      case _ =>
        t"<unknown>"

    val decomposable: Expr[test is Decomposable] =
      Expr.summon[test is Decomposable].get

    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $decomposable) }
