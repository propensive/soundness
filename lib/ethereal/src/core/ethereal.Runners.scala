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
package ethereal

import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import rudiments.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*

import errorDiagnostics.emptyDiagnostics
import gastronomy.*, providers.javaStdlibProvider
import httpBackends.virtualMachineHttp
import internetAccess.online
import monotonous.*, alphabets.hexLowerCase

// The reusable native runner stubs are published independently of any application as a
// GitHub release (`runners-<version>`), and verified against the committed
// `etc/runners/<version>.tsv` manifest. The `-Dbuild.executable` self-packaging path
// downloads the one stub it needs from here rather than requiring a local build, so the
// coordinates and per-platform SHA-256 hashes are hard-coded below. Bumping to a newly
// published release is a one-line change to `version` plus a refresh of `hashes`.
object Runners:
  val version: Text = t"0.3"

  val baseUrl: Text =
    t"https://github.com/propensive/soundness/releases/download/runners-$version"

  // Lowercase SHA-256 hex of each published stub, copied verbatim from
  // `etc/runners/$version.tsv`.
  val hashes: Map[Text, Text] =
    Map
      ( t"linux-arm64" -> t"ad8d1aa58d95cd87d6759cac2dddb5320aab23c225fd6c07d193c6964b7ba45d",
       t"linux-x64" -> t"0c49fc5aba44eb46658f09602e23515c7056c2212b63806cb487639087bfc9bf",
       t"macos-arm64" -> t"6bd94bff80766d9d540ca2580dcdfa920a36741f8c42b601cb2ca6681226fd86",
       t"macos-x64" -> t"17d36718a9d90509ab8e33c219b45d479c8f3c3d14ee54b4f333eb1f150d8a23",
       t"windows-x64" -> t"6afae357ac27b892db45d60f61711e1727af03602351c40534561f51ecd00d7e" )

  // The published filename for a platform's bare runner stub (Windows stubs carry `.exe`).
  def runnerName(label: Text): Text =
    if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

  // Download the bare reusable runner stub for `label` from the published release and verify
  // it against the hard-coded SHA-256 manifest.
  def download(label: Text): Data raises Runners.Error =
    val name: Text = runnerName(label)

    val expected: Text =
      hashes(label).lest(Runners.Error(m"There is no published runner stub for platform $label"))

    mitigate:
      case Http.Error(_, _)   => Runners.Error(m"Could not download the stub $name from $baseUrl")
      case Connect.Error(_)    => Runners.Error(m"Could not connect to $baseUrl to download $name")
      case Url.Error(_, _, _) => Runners.Error(m"The runner stub URL for $name is not valid")
      case Truncation.Error(_)     => Runners.Error(m"The download of the stub $name was interrupted")

    . protect:
        val runner: Data = mute[Http.Event](t"$baseUrl/$name".as[HttpUrl].fetch().read[Data])
        val actual: Text = runner.digest[Sha2[256]].serialize[Hex]

        if actual != expected
        then abort:
          Runners.Error(m"The downloaded runner stub $name has the wrong SHA-256 ($actual)")

        runner

  // RunnerError → Runners.Error
  case class Error(detail: Message)(using Diagnostics) extends fulminate.Error(detail)
