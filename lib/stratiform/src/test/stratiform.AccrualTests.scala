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
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package stratiform

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder

case class APerson(name: Text, age: Int, email: Text) derives CanEqual
case class AContact(person: APerson, company: Text) derives CanEqual
case class APair(width: Int, height: Int) derives CanEqual

object AccrualTests extends Suite(m"Stratiform multi-error accrual tests"):

  case class Issues(items: List[(Text, TelError)] = Nil)(using Diagnostics)
  extends Error(m"${items.length} decoding issues"):
    def +(focus: Text, error: TelError): Issues = Issues(items :+ (focus, error))

  private def validateTel[result](tel: Tel)
                                 (decode: Tel => result raises TelError tracks Tel.Focus)
  :   Issues =
    validate[Tel.Focus](Issues()):
      case error: TelError =>
        accrual + (prior.let(_.pointer.encode).or(t"#"), error)
    . within(decode(tel))

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Fully-valid record: no errors accrued"):
        val tel = t"name Alice\nage 30\nemail a@b.c\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.length
      . assert(_ == 0)

      test(m"Single missing field: one error"):
        val tel = t"name Alice\nage 30\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.length
      . assert(_ == 1)

      test(m"Single wrong-type field: one error"):
        val tel = t"width five\nheight 10\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.length
      . assert(_ == 1)

    suite(m"Multiple missing fields"):
      test(m"Two missing fields accrue two errors"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the missing fields"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/age", "#/email"))

      test(m"Each missing-field error has reason Absent"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.all:
          case (_, err) => err.reason == TelError.Reason.Absent
      . assert(identity)

    suite(m"Multiple wrong-type fields"):
      test(m"Two wrong types accrue two errors"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the wrong-type fields"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.map(_(0).s).to(Set)
      . assert(_ == Set("#/width", "#/height"))

      test(m"Wrong-type errors have reason NotScalar"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.all:
          case (_, err) => err.reason match
            case TelError.Reason.NotScalar(_, _) => true
            case _                               => false
      . assert(identity)

    suite(m"Nested case-class errors"):
      test(m"Missing nested case-class field expands per sub-field"):
        val tel = t"company Acme\n".read[Tel]
        validateTel(tel)(_.as[AContact]).items.map(_(0).s).to(Set)
      . assert: paths =>
          paths == Set
           ( "#/person/name",
             "#/person/age",
             "#/person/email" )

    suite(m"Regression: does not abort on the first bad field"):
      test(m"Both wrong-type fields are reported, not just the first"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.length
      . assert(_ > 1)
