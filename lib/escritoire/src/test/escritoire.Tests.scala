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
package escritoire

import soundness.*

import textMetrics.uniform

object Tests extends Suite(m"Escritoire tests"):
  def run(): Unit =
    test(m"Constrain to full width plus one is single line"):
      Column.constrain(t"the quick brown fox", Breaks.Space, 20).left
    .assert(_ == 1)

    test(m"Constrain to full width is still single line"):
      Column.constrain(t"the quick brown fox", Breaks.Space, 19).left
    .assert(_ == 1)

    test(m"Constrain to narrow column is two lines"):
      Column.constrain(t"the quick brown fox", Breaks.Space, 18).left
    .assert(_ == 2)

    test(m"Constrain to narrow column suggests better max"):
      Column.constrain(t"the quick brown fox", Breaks.Space, 18).right
    .assert(_ == 15)

    test(m"Constrain to very narrow column needs three lines"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 10).left
    .assert(_ == 3)

    test(m"Constrain to very narrow column can shrink slightly further"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 10).right
    .assert(_ == 9)

    test(m"Constrain to narrowest column cannot do better"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 5).right
    .assert(_ == 5)

    test(m"Constrain to narrowest column needs four lines"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 5).left
    .assert(_ == 4)

    test(m"Slightly wider column does not help"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 6).left
    .assert(_ == 4)

    test(m"Even wider column does not help"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 8).left
    .assert(_ == 4)

    test(m"Even wider column does not help and suggests max"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 8).right
    .assert(_ == 5)

    test(m"Even wider column still does help"):
      Column.constrain(t"the quick brown foxes", Breaks.Space, 9).left
    .assert(_ == 3)
