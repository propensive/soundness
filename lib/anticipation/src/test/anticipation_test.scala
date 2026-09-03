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
package anticipation

import soundness.*

object Tests extends Suite(m"Anticipation Tests"):
  def run(): Unit =
    // The bare-`Long` time givens are named for their unit, and the names are the only thing
    // standing between a call site and a duration a million times too short (issue #1920). These
    // pin each name to its factor. They are referenced rather than imported, because the three
    // duration givens deliberately share a type and cannot coexist in one scope.
    suite(m"Bare-Long durations"):
      test(m"Nanosecond durations are the generic representation"):
        abstractables.nanosecondsAbstractable.genericize(5L)
      . assert(_ == 5L)

      test(m"Microsecond durations scale by a thousand"):
        abstractables.microsecondsAbstractable.genericize(5L)
      . assert(_ == 5_000L)

      test(m"Millisecond durations scale by a million"):
        abstractables.millisecondsAbstractable.genericize(5L)
      . assert(_ == 5_000_000L)

      test(m"Nanosecond durations are instantiated unchanged"):
        instantiables.nanosecondsInstantiable(5L)
      . assert(_ == 5L)

      test(m"Microsecond durations are instantiated by dividing"):
        instantiables.microsecondsInstantiable(5_000L)
      . assert(_ == 5L)

      test(m"Millisecond durations are instantiated by dividing"):
        instantiables.millisecondsInstantiable(5_000_000L)
      . assert(_ == 5L)

    suite(m"Bare-Long instants"):
      test(m"Instants are epoch milliseconds, not nanoseconds"):
        abstractables.epochMillisecondsAbstractable.genericize(5L)
      . assert(_ == 5L)

      test(m"Epoch milliseconds are instantiated unchanged"):
        instantiables.epochMillisecondsInstantiable(5L)
      . assert(_ == 5L)
