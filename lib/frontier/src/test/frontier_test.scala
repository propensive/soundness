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
package frontier

import soundness.{every as _, *}

object Tests extends Suite(m"Frontier Tests"):
  trait Plug

  object NoPlugs:
    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  object OnePlug:
    given p1: Plug = new Plug {}

    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  object TwoPlugs:
    given p1: Plug = new Plug {}
    given p2: Plug = new Plug {}

    def explicit: Int = every[Plug].values.length
    def viaSummon: Int = summon[Every[Plug]].values.length

  def run(): Unit =
    test(m"every[X] returns empty Every when no givens in scope"):
      NoPlugs.explicit
    . assert(_ == 0)

    test(m"summon[Every[X]] returns empty Every when no givens in scope"):
      NoPlugs.viaSummon
    . assert(_ == 0)

    test(m"every[X] with one given in scope returns 1"):
      OnePlug.explicit
    . assert(_ == 1)

    test(m"summon[Every[X]] with one given in scope returns 1"):
      OnePlug.viaSummon
    . assert(_ == 1)

    test(m"every[X] collects two ambiguous givens in scope"):
      TwoPlugs.explicit
    . assert(_ == 2)

    test(m"summon[Every[X]] collects two ambiguous givens in scope"):
      TwoPlugs.viaSummon
    . assert(_ == 2)

    test(m"every[X] compiles cleanly with givens in scope"):
      demilitarize:
        trait Widget
        given w1: Widget = new Widget {}
        given w2: Widget = new Widget {}
        val all: Every[Widget] = every[Widget]
      . map(_.message)
    . assert(_ == Nil)

    test(m"summon[Every[X]] compiles cleanly with givens in scope"):
      demilitarize:
        trait Widget
        given w1: Widget = new Widget {}
        given w2: Widget = new Widget {}
        val all: Every[Widget] = summon[Every[Widget]]
      . map(_.message)
    . assert(_ == Nil)

    test(m"every[X] compiles cleanly when no givens are in scope"):
      demilitarize:
        trait Widget
        val all: Every[Widget] = every[Widget]
      . map(_.message)
    . assert(_ == Nil)

    test(m"explainMissingContext lists classpath givens for missing implicit"):
      demilitarize:
        import frontier.context.explainMissingContext
        summon[rudiments.DecimalConverter]
      . map(_.message)
    . assert(_.exists(_.contains("decimalFormatters.java")))
