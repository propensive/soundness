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
package larceny

import soundness.*

@deprecated("superseded by `current`", "0.1.0")
def obsolete(): Int = 42

object Tests extends Suite(m"Larceny Tests"):
  def run(): Unit =
    suite(m"Error capture"):
      test(m"a type error is captured as an error"):
        demilitarize:
          val int: Int = "not an int"
        . map(_.importance)
      . assert(_ == List(CompileError.Importance.Error))

      test(m"correct code captures nothing"):
        demilitarize:
          val int: Int = 42
      . assert(_ == Nil)

    suite(m"Warning capture"):
      test(m"a deprecation warning is captured"):
        demilitarize:
          obsolete()
        . map(_.importance)
      . assert(_ == List(CompileError.Importance.Warning))

      test(m"a deprecation warning names the deprecated method"):
        demilitarize:
          obsolete()
        . map(_.message)
      . assert(_.exists(_.contains("obsolete")))

      test(m"an unreachable case is captured as a warning"):
        demilitarize:
          (1: Int) match
            case _     => 0
            case other => other
        . map(_.importance)
      . assert(_ == List(CompileError.Importance.Warning))

      test(m"a warning does not stop the code from compiling"):
        demilitarize:
          obsolete()
        . map(_.warning)
      . assert(_ == List(true))

    suite(m"Errors and warnings are distinguishable"):
      test(m"warnings can be filtered out, leaving the errors"):
        demilitarize:
          obsolete()
          val int: Int = "not an int"
        . filter(_.error).map(_.reason)
      . assert(_ == List(CompileError.Reason.TypeMismatch))

      // A block that fails to type reports no late-phase warning. dotc stops its phase pipeline
      // once a unit has errors, and deprecation is checked after typer, so the warning never
      // fires -- blanking the region cannot recover it either, since the error and the warning
      // share a region. The limitation is per-block, not per-file: the deprecation tests above
      // capture their warning from a file in which these blocks are failing to compile.
      test(m"an error in a block suppresses that block's late-phase warnings"):
        demilitarize:
          obsolete()
          val int: Int = "not an int"
        . map(_.importance)
      . assert(_ == List(CompileError.Importance.Error))
