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
package ziggurat

import anticipation.*
import prepositional.*
import serpentine.*
import vacuous.*

// A complete, declarative description of how to turn an application into a
// distributable. `Packager.pack` is a thin facade over the existing per-platform
// assembly (ethereal) and script wrapping (`Xeq`); each field maps to one of
// those existing mechanisms.
object Packaging:
  // How the per-platform binaries reach the user.
  enum Delivery:
    case EmbedAll                                // every runner embedded in one script (`Xeq.installer`)
    case Download(baseUrl: Text, version: Text)  // downloaded per-platform (`Xeq.multiDownloader`)
    case Native                                  // a single native binary, no wrapper (one target only)

  // Where each platform's bare runner binary comes from.
  enum RunnerSource:
    case LocalResource  // the app self-assembles using `Classpath/"ethereal"/runner-<label>`

    // Each runner is downloaded from `<baseUrl>/runner-<label>[.exe]` and verified
    // against `hashes(label)` (lowercase SHA-256 hex), then assembled in-process.
    case Remote(baseUrl: Text, hashes: Map[Text, Text])

  // The bundled Java runtime *preference* recorded in the ETHRCFG block. Records
  // a preference only — the runner downloads a JRE/JDK at runtime; nothing is
  // embedded in the artifact.
  enum Bundle:
    case Jre, Jdk

  // How the application's classes reach the runtime.
  enum Dependencies:
    case FatJar(jar: Path on Linux)         // the fat jar, appended to the runner as-is
    case BurdockRemote(jar: Path on Linux)  // a macro-built thin jar that fetches deps (Stage C)

  case class JavaPolicy(minimum: Int = 21, preferred: Int = 24, bundle: Bundle = Bundle.Jre)

  // Self-upgrade signing. `Unset` overall disables upgrades (the safe default).
  case class Signing
    ( publicKey:      Optional[Path on Linux] = Unset,  // baked in via `-Dethereal.publicKey`
     seed:           Optional[Path on Linux] = Unset,  // signs post-assembly via `ethereal-sign`
     allowDowngrade: Boolean                 = false )

case class Packaging
  ( name:         Text,
   targets:      List[Text],
   delivery:     Packaging.Delivery,
   dependencies: Packaging.Dependencies,
   output:       Path on Linux,
   runnerSource: Packaging.RunnerSource      = Packaging.RunnerSource.LocalResource,
   java:         Packaging.JavaPolicy        = Packaging.JavaPolicy(),
   signing:      Optional[Packaging.Signing] = Unset,
   buildId:      Long                        = 0L )
