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
package anthology

import org.scalajs.linker.interface.{ModuleKind, StandardConfig}

import soundness.*

// Unexecuted definitions whose successful compilation asserts the variance properties of
// `Compilation`: a portable compilation may be linked as any concrete portable backend.
def portableLinksAsJs(compilation: Compilation[Backend.Portable]): Compilation[Backend.Js] =
  compilation

def portableLinksAsWasi(compilation: Compilation[Backend.Portable]): Compilation[Backend.Wasi] =
  compilation

object Tests extends Suite(m"Anthology Tests"):
  def run(): Unit =
    test(m"A single-type-argument Scalac targets the JVM"):
      val scalac: Scalac[3.6, Backend.Jvm] = Scalac[3.6](Nil)
      scalac.commandLineArguments
    . assert(_ == Nil)

    test(m"Retargeting a Scalac preserves its options"):
      Scalac[3.8](List(scalacOptions.experimental)).targeting[Backend.Portable]
      . commandLineArguments
    . assert(_ == List(t"-experimental"))

    test(m"The JVM backend adds no compiler flags"):
      summon[Backend.Emission[Backend.Jvm]].flags
    . assert(_ == Nil)

    test(m"Every portable backend adds the -scalajs flag"):
      List
        ( summon[Backend.Emission[Backend.Js]].flags,
          summon[Backend.Emission[Backend.Wasm]].flags,
          summon[Backend.Emission[Backend.Wasi]].flags,
          summon[Backend.Emission[Backend.Portable]].flags )
    . assert(_.forall(_ == List(t"-scalajs")))

    test(m"Module-kind options configure the linker"):
      linkerOptions.moduleKind.commonJs.configure(StandardConfig()).moduleKind
    . assert(_ == ModuleKind.CommonJSModule)

    test(m"The JavaScript linkage produces an ES module"):
      summon[Linkage[Backend.Js]].configure(StandardConfig()).moduleKind
    . assert(_ == ModuleKind.ESModule)

    test(m"The browser Wasm linkage enables the WebAssembly backend"):
      summon[Linkage[Backend.Wasm]].configure(StandardConfig()).esFeatures.useWebAssembly
    . assert(_ == true)

    test(m"A module-kind option is not applicable to a Wasm linker"):
      demilitarize:
        Linker[Backend.Wasm](List(linkerOptions.moduleKind.esModule))
    . assert(_.nonEmpty)

    test(m"A WASI linkage is not available without toolchain and WIT world"):
      demilitarize:
        summon[Linkage[Backend.Wasi]]
    . assert(_.nonEmpty)

    test(m"A JVM compilation cannot be linked"):
      demilitarize:
        val compilation: Compilation[Backend.Jvm] = ???
        portableLinksAsJs(compilation)
    . assert(_.nonEmpty)

    test(m"A portable compilation cannot be bundled as an executable JAR"):
      demilitarize:
        val compilation: Compilation[Backend.Portable] = ???
        Bundler.bundle(compilation, Unset, Unset)
    . assert(_.nonEmpty)

    test(m"A JVM compilation cannot be bundled as an sjsir library"):
      demilitarize:
        val compilation: Compilation[Backend.Jvm] = ???
        Bundler.library(compilation, Unset)
    . assert(_.nonEmpty)
