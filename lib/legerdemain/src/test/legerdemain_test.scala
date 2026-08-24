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
package legerdemain

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import denominative.dysasymptotics.linearSize

case class QPerson(name: Text, email: Text) derives CanEqual
case class QTeam(leader: QPerson, title: Text) derives CanEqual

object QProbe:
  @scala.caps.unsafe.untrackedCaptures
  var constructions: Int = 0

// The body statement observes construction: decoding must never construct from garbage
// fallback values, so a decode with any failed parameter must leave the counter untouched.
case class QChecked(name: Text, email: Text) derives CanEqual:
  QProbe.constructions += 1

object Tests extends Suite(m"Legerdemain tests"):

  case class Issues(items: List[(Text, Query.Error)] = Nil)(using Diagnostics)
  extends Error(m"${items.size} query issues"):
    def +(focus: Text, error: Query.Error): Issues = Issues(items :+ (focus, error))

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking, so the decode lambda must beta-reduce away into
  // `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateQuery[result](query: Query)
    (inline decode: Query => result raises Query.Error tracks Pointer)
  :   Issues =
    Validate[Issues, [r] =>> r raises Query.Error, Pointer]
      ( Issues(),
        { case error: Query.Error => accrual + (prior.let(_.text).or(t"?"), error) } )
    . protect(decode(query))

  def run(): Unit =
    suite(m"Query decoding"):
      test(m"A complete query decodes"):
        t"name=Ada&email=a%40b.c".as[Query].as[QPerson]
      . assert(_ == QPerson(t"Ada", t"a@b.c"))

      test(m"A missing parameter aborts under a fail-fast strategy"):
        capture[Query.Error](t"name=Ada".as[Query].as[QPerson]).reason
      . assert(_ == Query.Error.Reason.Missing)

    suite(m"Validation accrual"):
      test(m"Two missing parameters both accrue, with their pointers"):
        validateQuery(t"".as[Query])(_.as[QPerson]).items.map(_(0).s).to[Set]
      . assert(_ == Set("name", "email"))

      test(m"One missing parameter accrues one error; the present one does not"):
        validateQuery(t"name=Ada".as[Query])(_.as[QPerson]).items.map(_(0).s)
      . assert(_ == List("email"))

      test(m"Nested parameters accrue with dotted pointers"):
        validateQuery(t"title=Skunkworks".as[Query])(_.as[QTeam]).items.map(_(0).s).to[Set]
      . assert(_ == Set("leader.name", "leader.email"))

      test(m"A fully-valid query accrues nothing"):
        validateQuery(t"name=Ada&email=a%40b.c".as[Query])(_.as[QPerson]).items.size
      . assert(_ == 0)

    suite(m"Gated construction"):
      test(m"Constructor does not run when any parameter failed"):
        QProbe.constructions = 0
        val issues = validateQuery(t"name=Zoe".as[Query])(_.as[QChecked])
        (issues.items.size, QProbe.constructions)
      . assert(_ == (1, 0))

      test(m"Constructor runs exactly once when all parameters are present"):
        QProbe.constructions = 0
        validateQuery(t"name=Zoe&email=z%40y.x".as[Query])(_.as[QChecked])
        QProbe.constructions
      . assert(_ == 1)
