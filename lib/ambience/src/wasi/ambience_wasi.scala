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
package ambience

import scala.annotation.nowarn

import anticipation.*
import gossamer.*
import hellenism.*
import prepositional.*
import rudiments.*
import soundness.invoke
import vacuous.*
import xenophile.*

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for the function's module id.
type WasiEnvironmentApi = Interface in Wit at "/ambience/environment.wit"
given wasiEnvironmentApi: WasiEnvironmentApi = Interface[Wit](cp"/ambience/environment.wit")

package environments:
  // `inline`, so the `invoke` macro expands at the downstream summoning site: the Wasm Component
  // import (`scala.scalajs.wit.witImportCall`) only materializes in code compiled for a Wasm
  // target. `get-environment` returns `list<tuple<string, string>>`, decoded to
  // `List[(Text, Text)]`.
  //
  // The per-site duplication the compiler warns about is the point: the instance must materialize
  // at the downstream summoning site, and a WASI-linked application summons it once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasiEnvironment: Environment = new Environment:
    def variable(name: Text): Optional[Text] =
      Foreign["environment", Wit].`get-environment`.invoke[List[(Text, Text)]]
        .filter(_._1 == name).prim.let(_._2)

package workingDirectories:
  // The component's initial working directory, from `wasi:cli/environment` `initial-cwd`
  // (`option<string>` → `Optional[Text]`); a component without one falls back to `/`.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasiWorkingDirectory: WorkingDirectory = new WorkingDirectory:
    def directory(): Text =
      Foreign["environment", Wit].`initial-cwd`.invoke[Optional[Text]].or(t"/")
