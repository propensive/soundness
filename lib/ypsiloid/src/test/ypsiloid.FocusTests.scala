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
┃    Soundness, version 0.63.0.                                                                    ┃
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

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

case class FPerson(name: Text, age: Int, email: Text) derives CanEqual
case class FAddress(street: Text, city: Text, zip: Text) derives CanEqual
case class FContact(person: FPerson, address: FAddress) derives CanEqual

object FocusTests extends Suite(m"Ypsiloid focus + position tests"):

  case class Captured
    ( items: List[(Text, Optional[Int], Optional[Int])] = Nil )
    ( using Diagnostics )
  extends Error(m"${items.length} validation issues"):
    def +(focus: Text, line: Optional[Int], column: Optional[Int]): Captured =
      Captured(items :+ (focus, line, column))

  // Inline + direct `Validate` construction; see ypsiloid.AccrualTests and rep/DECISIONS.md.
  private inline def captureFoci[result](yaml: Yaml)
    (inline decode: Yaml => result raises YamlError tracks Yaml.Focus)
  :   List[(Text, Optional[Int], Optional[Int])] =
    Validate[Captured, [r] =>> r raises YamlError, Yaml.Focus]
      ( Captured(),
        { case error: YamlError =>
            val position = prior.let(_.position)
            accrual + ( prior.let(_.pointer.encode).or(t"#"),
                        position.let(_.line),
                        position.let(_.column) ) } )
    . protect(decode(yaml)).items

  def run(): Unit =
    suite(m"Pointer-only focus (untracked Yaml)"):
      test(m"Missing field reports the focus pointer (no position)"):
        val yaml = t"name: Alice\nage: 30".read[Yaml]
        captureFoci(yaml)(_.as[FPerson]).map(_(0).s).to(Set)
      . assert(_ == Set("#/email"))

      test(m"Wrong-type field reports the focus pointer (no position)"):
        val yaml = t"name: Alice\nage: thirty\nemail: a@b".read[Yaml]
        captureFoci(yaml)(_.as[FPerson]).map(_(0).s).to(Set)
      . assert(_ == Set("#/age"))

      test(m"Nested case-class missing field reports root-first path"):
        val yaml = t"""
person:
  name: X
  age: 1
  email: x@y
address:
  street: S
""".read[Yaml]
        // address is missing both `city` and `zip`; primitive
        // decoders raise+yet on the `Unset` sentinel so both accrue
        // their own errors rather than the first one aborting the
        // whole decode.
        captureFoci(yaml)(_.as[FContact]).map(_(0).s).to(Set)
      . assert(_ == Set("#/address/city", "#/address/zip"))

      test(m"Untracked roots leave the focus position Unset"):
        val yaml = t"name: Alice\nage: 30".read[Yaml]
        captureFoci(yaml)(_.as[FPerson]).forall((_, line, _) => line == Unset)
      . assert(identity)

    suite(m"Position-aware focus (tracked Yaml)"):
      given Yaml.Tracking = Yaml.Tracking.On

      test(m"Tracked root: focus pointers are still correct"):
        // The decoder still aborts on first error (raise+yet is a PR 3
        // change), so position population via `as[T]`'s Foci.supplement
        // doesn't fire when an error short-circuits the decode. The
        // focus *path* is registered by the focus block's try/finally
        // either way, so we verify that path here and exercise the
        // `withPosition` plumbing in a separate direct test below.
        val yaml = t"name: Alice\nage: 30".read[Yaml]
        captureFoci(yaml)(_.as[FPerson]).map(_(0).s).to(Set)
      . assert(_ == Set("#/email"))

      test(m"Nested missing field reports root-first path on a tracked root"):
        val source = t"""person:
  name: C
  age: 25
  email: c@x
address:
  street: X
"""
        captureFoci(source.read[Yaml])(_.as[FContact]).map(_(0).s).to(Set)
      . assert(_ == Set("#/address/city", "#/address/zip"))

      test(m"withPosition on a tracked Yaml resolves the pointer to a real position"):
        // Direct exercise of `Yaml.Focus#withPosition`. Even though the
        // current decoder aborts before `as[T]`'s `Foci.supplement` can
        // run, the plumbing is wired correctly — once primitive
        // decoders gain raise+yet sentinels in PR 3, wrong-type errors
        // will land with `position` populated through this same path.
        val source = t"name: Alice\nage: 30\nemail: a@b\n"
        val yaml = source.read[Yaml]
        Yaml.Focus(YamlPath()(t"age")).withPosition(yaml).position.let(_.line)
      . assert(_ == 2)

      test(m"withPosition leaves position Unset when the pointer doesn't resolve"):
        val yaml = t"name: Alice".read[Yaml]
        Yaml.Focus(YamlPath()(t"missing")).withPosition(yaml).position
      . assert(_ == Unset)
