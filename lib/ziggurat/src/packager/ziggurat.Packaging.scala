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
package ziggurat

import anticipation.*
import galilei.*
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
    case EmbedAll
    // Online: the JAR is embedded once and each bare stub is downloaded at runtime from the
    // `RunnerSource.Remote` base URL (so `Download` requires a `Remote` runner source).
    case Download
    case Native

  // Where each platform's bare reusable runner stub comes from. The stubs are published
  // independently (see `make runners-release`); a build never compiles them.
  enum RunnerSource:
    // Read `<directory>/runner-<label>[.exe]` from a local directory (e.g. the output of
    // `make runners-build`). For development and testing — no download, no hash check.
    case Local(directory: Path on Linux)

    // Download each stub from `<baseUrl>/runner-<label>[.exe]` and verify it against
    // `hashes(label)` (lowercase SHA-256 hex, from the committed `etc/runners/<v>.tsv`
    // manifest). The production source.
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
    ( publicKey:      Optional[Path on Linux] = Unset, // baked in via `-Dethereal.publicKey`
     seed:           Optional[Path on Linux] = Unset, // signs post-assembly via `ethereal-sign`
     allowDowngrade: Boolean                 = false )

case class Packaging
  ( name:         Text,
   targets:      List[Text],
   delivery:     Packaging.Delivery,
   dependencies: Packaging.Dependencies,
   output:       Path on Linux,
   runnerSource: Packaging.RunnerSource,
   java:         Packaging.JavaPolicy        = Packaging.JavaPolicy(),
   signing:      Optional[Packaging.Signing] = Unset,
   buildId:      Long                        = 0L )
