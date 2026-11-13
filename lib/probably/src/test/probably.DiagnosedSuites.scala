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

import scala.language.experimental.captureChecking

import soundness.*

import apparitions.spectral

case class Courtier(name: Text, age: Int)
case class Tragedy(title: Text, hero: Courtier, cast: List[Courtier])

// A type whose rendering is supplied by its companion: the suite which fails on one says
// nothing about how it is to be shown.
case class Ghost(name: Text)

object Ghost:
  given inspectable: Ghost is Inspectable = ghost => t"the ghost of ${ghost.name}"

// Two more, rendered by instances which are NOT in their implicit scope: one imported at the
// top of this file, and one a member of the object its suite is written in.
case class Spectre(name: Text)
case class Witch(name: Text)

object apparitions:
  given spectral: Spectre is Inspectable = spectre => t"the spectre of ${spectre.name}"

object Heath:
  given witching: Witch is Inspectable = witch => t"the weird sister ${witch.name}"

  private def secret: Int = 3

  class Coven extends Suite(m"coven"):
    def run(): Unit =
      test(m"member")(Witch(t"Hecate")).assert(_ == Witch(t"Graymalkin"))
      test(m"private")(2).assert(x => x > secret)

// Suites which fail on purpose, driven through `invoke` by the diagnosis tests: classes, not
// objects, so the suite index does not list them. Capture checked, so the locals a predicate
// closes over are gathered under the checker.
class Dissected extends Suite(m"dissected"):
  def run(): Unit =
    val y: Int = 2
    test(m"arithmetic")(2).assert(x => x*2 < 9 && x*y > 4)

    val hero: Courtier = Courtier(t"Hamlet", 30)

    test(m"equality")(Tragedy(t"Hamlet", Courtier(t"Hamlet", 31), Nil))
    . assert(_ == Tragedy(t"Hamlet", hero, Nil))

    test(m"passing")(1).assert(_ == 1)
    test(m"throwing")(0).assert(x => 1/x > 0)
    test(m"companion")(Ghost(t"Hamlet's father")).assert(_ == Ghost(t"Banquo"))
    test(m"imported")(Spectre(t"Caesar")).assert(_ == Spectre(t"Banquo"))

    // A predicate which fails the first time it is asked — by the assertion — and not the
    // second, when the diagnosis asks again.
    val asked: java.util.concurrent.atomic.AtomicInteger =
      java.util.concurrent.atomic.AtomicInteger(0)

    test(m"fickle")(1).assert(x => x > 0 && asked.getAndIncrement() > 0)

    List(1, 2, 3).each: n =>
      test(m"repeated $n")(n).assert(x => x + y > 10)
