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

import org.scalajs.linker.interface.{ESVersion, ModuleKind, StandardConfig}

object linkerOptions:
  private def sjs[artifact <: Artifact.Sjs](edit: StandardConfig => StandardConfig)
  :   Linker.Option[artifact] =

    Linker.Option(edit)

  object moduleKind:
    val esModule: Linker.Option[Artifact.Js] =
      sjs(_.withModuleKind(ModuleKind.ESModule))

    val commonJs: Linker.Option[Artifact.Js] =
      sjs(_.withModuleKind(ModuleKind.CommonJSModule))

    val noModule: Linker.Option[Artifact.Js] =
      sjs(_.withModuleKind(ModuleKind.NoModule))

  val checkIr: Linker.Option[Artifact.Sjs] = sjs(_.withCheckIR(true))
  val sourceMaps: Linker.Option[Artifact.Sjs] = sjs(_.withSourceMap(true))

  object esVersion:
    private def of(version: ESVersion): Linker.Option[Artifact.Sjs] =
      sjs(_.withESFeatures(_.withESVersion(version)))

    val es2015: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2015)
    val es2016: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2016)
    val es2017: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2017)
    val es2018: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2018)
    val es2019: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2019)
    val es2020: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2020)
    val es2021: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2021)
    val es2022: Linker.Option[Artifact.Sjs] = of(ESVersion.ES2022)

  object optimize:
    val none: Linker.Option[Artifact.Sjs] = sjs(_.withOptimizer(false))
    val fast: Linker.Option[Artifact.Sjs] = sjs(_.withOptimizer(true))
