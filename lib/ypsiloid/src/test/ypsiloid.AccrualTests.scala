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
package ypsiloid

import soundness.*

import proscenium.compat.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

case class APerson(name: Text, age: Int, email: Text) derives CanEqual
case class AContact(person: APerson, company: Text) derives CanEqual

object AccrualTests extends Suite(m"Ypsiloid multi-error accrual tests"):

  case class Issues(items: List[(Text, YamlError)] = Nil)(using Diagnostics)
  extends Error(m"${items.length} validation issues"):
    def +(focus: Text, error: YamlError): Issues = Issues(items :+ (focus, error))

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking (its honest type is a curried dependent context
  // function, an unimplemented compiler restriction), so the decode lambda must beta-reduce
  // away into `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateYaml[result](yaml: Yaml)
    (inline decode: Yaml => result raises YamlError tracks Yaml.Focus)
  :   Issues =
    Validate[Issues, [r] =>> r raises YamlError, Yaml.Focus]
      ( Issues(),
        { case error: YamlError =>
            accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
    . protect(decode(yaml))

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Fully-valid object: no errors accrued"):
        val yaml = t"name: Alice\nage: 30\nemail: a@b.c\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 0)

      test(m"Single missing field: one error"):
        val yaml = t"name: Alice\nage: 30\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 1)

      test(m"Single wrong-type field: one error"):
        val yaml = t"name: Alice\nage: thirty\nemail: a@b\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 1)

    suite(m"Multiple missing fields"):
      test(m"Two missing primitive fields accrue two errors"):
        val yaml = t"name: Alice\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the missing fields"):
        val yaml = t"name: Alice\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.map(_(0).s).to[Set]
      . assert(_ == Set("#/age", "#/email"))

      test(m"Each missing-field error has reason Absent"):
        val yaml = t"name: Alice\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.all:
          case (_, err) => err.reason == YamlError.Reason.Absent
      . assert(identity)

      test(m"Three missing fields: three errors accrued"):
        val yaml = t"{}".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 3)

    suite(m"Multiple wrong-type fields"):
      test(m"Two wrong types accrue two errors"):
        val yaml = t"name: 42\nage: thirty\nemail: x@y\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify the wrong-type fields"):
        val yaml = t"name: 42\nage: thirty\nemail: x@y\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.map(_(0).s).to[Set]
      . assert(_ == Set("#/name", "#/age"))

      test(m"Wrong-type errors have reason NotType"):
        val yaml = t"name: 42\nage: thirty\nemail: x@y\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.all:
          case (_, err) => err.reason match
            case YamlError.Reason.NotType(_, _) => true
            case _                              => false
      . assert(identity)

    suite(m"Missing + wrong-type mixed"):
      test(m"One wrong-type plus two missing: three errors at the right pointers"):
        val yaml = t"name: 42\n".read[Yaml]
        validateYaml(yaml)(_.as[APerson]).items.map(_(0).s).to[Set]
      . assert(_ == Set("#/name", "#/age", "#/email"))

    suite(m"Nested case-class errors"):
      test(m"Missing nested case-class field expands per sub-field"):
        // Without a `Default[APerson]` (PR 4), a missing nested case
        // class hits the wrong-shape branch of the inner conjunction,
        // which builds against an empty mapping and lets each sub-
        // field raise its own missing-field error.
        val yaml = t"company: Acme\n".read[Yaml]
        validateYaml(yaml)(_.as[AContact]).items.map(_(0).s).to[Set]
      . assert: paths =>
          paths == Set
           ( "#/person/name",
             "#/person/age",
             "#/person/email" )

      test(m"Mixed errors at different depths accrue together"):
        val yaml = t"person:\n  name: D\ncompany: Acme\n".read[Yaml]
        // person is present but missing `age` and `email`; company is
        // present.
        validateYaml(yaml)(_.as[AContact]).items.map(_(0).s).to[Set]
      . assert(_ == Set("#/person/age", "#/person/email"))
