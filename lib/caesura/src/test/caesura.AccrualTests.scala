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
package caesura

import soundness.*

import proscenium.compat.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import dsvFormats.csvWithHeaderFormat

case class ARecord(name: Text, age: Int, height: Int) derives CanEqual

object AccrualTests extends Suite(m"Caesura multi-error accrual tests"):

  case class Issues(items: List[(Text, DsvError)] = Nil)(using Diagnostics)
  extends Error(m"${items.length} decoding issues"):
    def +(focus: Text, error: DsvError): Issues = Issues(items :+ (focus, error))

  private inline def validateDsv[result](dsv: Dsv)
                                 (inline decode: Dsv => result raises DsvError tracks CellRef)
  :   Issues =
    Validate[Issues, [r] =>> r raises DsvError, CellRef]
      ( Issues(),
        { case error: DsvError =>
            accrual + (prior.let(_.column).or(t"#"), error) } )
    . protect(decode(dsv))

  private def row(text: Text): Dsv = text.read[Sheet].rows.head

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Fully-valid row: no errors accrued"):
        validateDsv(row(t"name,age,height\nAlice,30,170"))(_.as[ARecord]).items.length
      . assert(_ == 0)

      test(m"Single unparseable cell: one error"):
        validateDsv(row(t"name,age,height\nAlice,thirty,170"))(_.as[ARecord]).items.length
      . assert(_ == 1)

      test(m"Single missing cell: one error"):
        validateDsv(row(t"name,age,height\nAlice,30"))(_.as[ARecord]).items.length
      . assert(_ == 1)

    suite(m"Multiple unparseable cells"):
      test(m"Two unparseable cells accrue two errors"):
        validateDsv(row(t"name,age,height\nAlice,thirty,tall"))(_.as[ARecord]).items.length
      . assert(_ == 2)

      test(m"Columns identify the unparseable cells"):
        validateDsv(row(t"name,age,height\nAlice,thirty,tall"))(_.as[ARecord])
         .items.map(_(0).s).to[Set]
      . assert(_ == Set("age", "height"))

      test(m"Each unparseable error has reason Unparseable"):
        validateDsv(row(t"name,age,height\nAlice,thirty,tall"))(_.as[ARecord]).items.all:
          case (_, err) => err.reason match
            case DsvError.Reason.Unparseable(_, _) => true
            case _                                 => false
      . assert(identity)

    suite(m"Multiple missing cells"):
      test(m"Two missing cells accrue two errors"):
        validateDsv(row(t"name,age,height\nAlice"))(_.as[ARecord]).items.length
      . assert(_ == 2)

      test(m"Columns identify the missing cells"):
        validateDsv(row(t"name,age,height\nAlice"))(_.as[ARecord]).items.map(_(0).s).to[Set]
      . assert(_ == Set("age", "height"))

      test(m"Each missing-cell error has reason Absent"):
        validateDsv(row(t"name,age,height\nAlice"))(_.as[ARecord]).items.all:
          case (_, err) => err.reason == DsvError.Reason.Absent
      . assert(identity)

    suite(m"Missing + unparseable mixed"):
      test(m"One unparseable plus one missing: two errors at the right columns"):
        validateDsv(row(t"name,age,height\nAlice,thirty"))(_.as[ARecord]).items.map(_(0).s).to[Set]
      . assert(_ == Set("age", "height"))

    suite(m"Regression: does not abort on the first bad cell"):
      test(m"Both bad cells are reported, not just the first"):
        validateDsv(row(t"name,age,height\nAlice,bad1,bad2"))(_.as[ARecord]).items.length
      . assert(_ > 1)
