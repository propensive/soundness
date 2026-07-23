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
import discriminables.yamlByTypeDiscriminable

case class DPerson(name: Text, age: Int, email: Text) derives CanEqual
case class DContact(person: DPerson, company: Text) derives CanEqual

enum DShape derives CanEqual:
  case Circle(radius: Int)
  case Square(side: Int)

case class Issues2(items: List[(Text, YamlError)] = Nil)(using Diagnostics)
extends Error(m"${items.length} validation issues"):
  def +(focus: Text, error: YamlError): Issues2 = Issues2(items :+ (focus, error))

object DefaultPersonScope:
  given Default[DPerson] = () => DPerson(t"", 0, t"")

  def run(): Set[String] =
    val yaml = t"company: Acme\n".read[Yaml]
    validate[Yaml.Focus](Issues2()):
      case error: YamlError =>
        accrual + (prior.let(_.pointer.encode).or(t"#"), error)
    . protect(yaml.as[DContact]).items.stdlib.map(_(0).s).pipe(Set.from(_))

object DefaultShapeScope:
  given Default[DShape] = () => DShape.Circle(-1)

  def runIssues(): (Set[String], Int) =
    val yaml = t"type: Triangle\nfoo: bar\n".read[Yaml]
    val issues = validate[Yaml.Focus](Issues2()):
      case error: YamlError =>
        accrual + (prior.let(_.pointer.encode).or(t"#"), error)
    . protect(yaml.as[DShape])
    (issues.items.stdlib.map(_(0).s).pipe(Set.from(_)), issues.items.length)

object DefaultTests extends Suite(m"Ypsiloid Default-driven sentinel tests"):

  // Inline + direct `Validate` construction; see ypsiloid.AccrualTests and rep/DECISIONS.md.
  private inline def validateYaml[result](yaml: Yaml)
    (inline decode: Yaml => result raises YamlError tracks Yaml.Focus)
  :   Issues2 =
    Validate[Issues2, [r] =>> r raises YamlError, Yaml.Focus]
      ( Issues2(),
        { case error: YamlError =>
            accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
    . protect(decode(yaml))

  def run(): Unit =
    suite(m"Default-driven sentinels"):
      test(m"Default[DPerson] collapses a missing nested into one error"):
        DefaultPersonScope.run()
      . assert(_ == Set("#/person"))

      test(m"Default[DShape] handles an unknown discriminator at the top level"):
        DefaultShapeScope.runIssues()
      . assert((paths, count) => count == 1 && paths == Set("#"))

      test(m"Without Default[DPerson], a missing nested still expands"):
        // Confirms the existing (no-Default) PR-3 accrual semantics
        // are unchanged for users who don't opt in.
        val yaml = t"company: Acme\n".read[Yaml]
        validateYaml(yaml)(_.as[DContact]).items.stdlib.map(_(0).s).pipe(Set.from(_))
      . assert: paths =>
          paths == Set
           ( "#/person/name",
             "#/person/age",
             "#/person/email" )

      test(m"Without Default[DShape], unknown discriminator aborts cleanly"):
        // Outside a `Default[DShape]`, the disjunction calls `abort`,
        // which the surrounding `validate` captures as one accrual
        // entry. No `VariantError` punches through.
        val yaml = t"type: Triangle\nfoo: bar\n".read[Yaml]
        validateYaml(yaml)(_.as[DShape]).items.length
      . assert(_ == 1)

      test(m"Without Default[DShape], absent discriminator aborts cleanly"):
        val yaml = t"foo: bar\n".read[Yaml]
        validateYaml(yaml)(_.as[DShape]).items.length
      . assert(_ == 1)
