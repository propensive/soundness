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

import anticipation.*
import galilei.*
import prepositional.*
import serpentine.*

object Linkage:
  given js: Linkage[Backend.Js]:
    private[anthology] def configure(config: StandardConfig): StandardConfig =
      config.withModuleKind(ModuleKind.ESModule)

    private[anthology] def artifact(out: Path on Linux): Path on Linux = out / "main.js"

  given wasm: Linkage[Backend.Wasm]:
    private[anthology] def configure(config: StandardConfig): StandardConfig =
      config
      . withModuleKind(ModuleKind.ESModule)
      . withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true))

    private[anthology] def artifact(out: Path on Linux): Path on Linux = out / "main.wasm"


  given wasi(using toolchain: WasiToolchain, world: WitWorld): Linkage[Backend.Wasi] =
    new Linkage[Backend.Wasi]:
      private[anthology] def configure(config: StandardConfig): StandardConfig =
        config
        . withModuleKind(ModuleKind.WasmComponent)
        . withESFeatures(_.withESVersion(ESVersion.ES2022).withUseWebAssembly(true))
        . withWasmFeatures: features =>
            features
            . withWitDirectory(Some(world.directory.encode.s))
            . withWitWorld(Some(world.world.s))

      private[anthology] def artifact(out: Path on Linux): Path on Linux = out / "main.wasm"

// Determines how each portable backend is linked: the linker configuration it mandates and the
// primary artifact it produces. The `wasi` instance is conditional upon a `WasiToolchain` (the
// native tools, proven present) and a `WitWorld`, so a WASI component link is only expressible
// once its runtime prerequisites are satisfied.
trait Linkage[target <: Backend.Portable]:
  private[anthology] def configure(config: StandardConfig): StandardConfig
  private[anthology] def artifact(out: Path on Linux): Path on Linux
