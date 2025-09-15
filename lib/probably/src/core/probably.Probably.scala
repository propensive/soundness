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
┃    Soundness, version 0.40.0.                                                                    ┃
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
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import dotty.tools.dotc.util as dtdu
import scala.quoted.*

object Probably:
  private def handle[test: Type, result: Type]
               (test:      Expr[Test[test]],
                predicate: Expr[test => Boolean],
                action:    Expr[Trial[test] => result])
  : Macro[result] =

      import quotes.reflect.*

      Expr.summon[Runner[?]].getOrElse(halt(m"No `Runner` instance is available")).absolve match
        case '{ $runner: Runner[report] } =>

          val inclusion = Expr.summon[Inclusion[report, Verdict]].getOrElse:
            halt(m"Can't embed test verdicts in ${Type.of[report]}")

          val inclusion2 = Expr.summon[Inclusion[report, Verdict.Detail]].getOrElse:
            halt(m"Can't embed test details in ${Type.of[report]}")


          def lift(predicate: Expr[Any]): Option[Expr[Any]] = predicate.asTerm match
            case Inlined(_, _, predicate) => lift(predicate.asExpr)
            case Block(List(DefDef(a1, _, _, Some(expression))), Closure(Ident(a2), _)) if a1 == a2 =>
              expression match
                case Apply(Select(Ident(_), "=="), List(term)) => Some(term.asExpr)
                case Apply(Select(term, "=="), List(Ident(_))) => Some(term.asExpr)
                case other                                     => None

            case other =>
              None

          val exp: Option[Expr[Any]] = lift(predicate)

          val analyse = Expr.summon[Autopsy] match
            case None =>
              Unset

            case Some('{ type analyse; $autopsy: (Autopsy { type Analyse = analyse }) }) =>
              TypeRepr.of[analyse].literal[Boolean]

          if analyse.or(false) then exp match
            case Some('{type testType >: test; $expr: testType}) =>
              val decomposable: Expr[testType is Decomposable] =
                Expr.summon[testType is Decomposable].getOrElse('{Decomposable.any[testType]})

              '{  given decompose: testType is Decomposable = $decomposable
                  val contrast = compiletime.summonInline[testType is Contrastable]

                  assertion[testType, test, report, result]
                   ($runner,
                    $test,
                    $predicate,
                    $action,
                    contrast,
                    Some($expr),
                    $inclusion,
                    $inclusion2,
                    decompose)  }

            case _ =>
              '{  ( assertion[test, test, report, result]
                    ($runner,
                      $test,
                      $predicate,
                      $action,
                      Contrastable.nothing[test],
                      None,
                      $inclusion,
                      $inclusion2,
                      Decomposable.any[test]) )  }

          else
            '{  ( assertion[test, test, report, result]
                  ($runner,
                    $test,
                    $predicate,
                    $action,
                    Contrastable.nothing[test],
                    None,
                    $inclusion,
                    $inclusion2,
                    Decomposable.any[test]) )  }

  def check[test: Type](test: Expr[Test[test]], predicate: Expr[test => Boolean]): Macro[test] =
    handle[test, test](test, predicate, '{ (t: Trial[test]) => t.get })

  def assert[test: Type](test: Expr[Test[test]], predicate: Expr[test => Boolean]): Macro[Unit] =
    handle[test, Unit](test, predicate, '{ _ => () })

  def aspire[test: Type](test: Expr[Test[test]]): Macro[Unit] =
    handle[test, Unit](test, '{ _ => true }, '{ _ => () })

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
                     (decomposable.decomposition(exp).text,
                      decomposable.decomposition(value).text,
                      contrast.juxtaposition(exp, value)))
                case None =>
                  // inc2.include(runner.report, test.id, Verdict.Detail.Compare
                  //  (summon[Any is Contrastable].compare(value, 1)))

              if !map.isEmpty
              then inc2.include(runner.report, test.id, Verdict.Detail.Captures(map))

              Verdict.Fail(duration)
            catch case err: Exception => Verdict.CheckThrows(err, duration)

        inc.include(runner.report, test.id, verdict)
        result(run)


  def debug[test: Type](expr: Expr[test], test: Expr[Harness]): Macro[test] =
    import quotes.reflect.*

    val exprName: Text = expr.asTerm.pos match
      case pos: dtdu.SourcePosition =>
        pos.lineContent.show.segment(pos.startColumn.z thru Ordinal.natural(pos.endColumn))

      case _ =>
        t"<unknown>"

    val decomposable: Expr[test is Decomposable] =
      Expr.summon[test is Decomposable].get

    '{ $test.capture(Text(${Expr[String](exprName.s)}), $expr)(using $decomposable) }
