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
package denominative

import soundness.*

object Tests extends Suite(m"Denominative Tests"):
  def run(): Unit =
    suite(m"Interval-construction tests"):
      test(m"subsequent yields an Interval of the requested size"):
        Prim.subsequent(3).size
      . assert(_ == 3)

      test(m"span yields an Interval of the requested size"):
        (Prim span 3).size
      . assert(_ == 3)

      test(m"preceding yields an Interval of the requested size"):
        Sept.preceding(3).size
      . assert(_ == 3)

      test(m"subsequent and span agree on size"):
        val skip = Prim.subsequent(3).size
        val full = (Prim span 3).size
        skip == full
      . assert(identity(_))

    suite(m"Interval-semantics tests"):
      test(m"span preserves the start ordinal"):
        (Sec span 3).start
      . assert(_ == Sec)

      test(m"span sets the inclusive end ordinal"):
        (Sec span 3).end
      . assert(_ == Quat)

      test(m"span sets the exclusive limit ordinal"):
        (Sec span 3).limit
      . assert(_ == Quin)

      test(m"interval contains an interior ordinal"):
        (Sec span 3).contains(Quat)
      . assert(identity(_))

      test(m"interval does not contain the limit ordinal"):
        (Sec span 3).contains(Quin)
      . assert(_ == false)

      test(m"each iterates exactly size times"):
        var count = 0
        (Sec span 3).each: _ =>
          count += 1
        count
      . assert(_ == 3)

    suite(m"Empty-interval tests"):
      test(m"an empty interval has zero size"):
        (Sec till Sec).size
      . assert(_ == 0)

      test(m"an empty interval is nil"):
        (Sec till Sec).nil
      . assert(identity(_))

      test(m"an empty interval contains no ordinals"):
        (Quat till Quat).contains(Quat)
      . assert(_ == false)

      test(m"empty intervals at different positions are equal"):
        (Sec till Sec) == (Sept till Sept)
      . assert(identity(_))

      test(m"the canonical empty interval equals a degenerate range"):
        Interval() == (Quat till Quat)
      . assert(identity(_))
