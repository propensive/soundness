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
import httpBackends.virtualMachine
import internetAccess.online
import monotonous.*, alphabets.hexLowerCase

case class RunnerError(detail: Message)(using Diagnostics) extends Error(detail)

// The reusable native runner stubs are published independently of any application as a
// GitHub release (`runners-<version>`), and verified against the committed
// `etc/runners/<version>.tsv` manifest. The `-Dbuild.executable` self-packaging path
// downloads the one stub it needs from here rather than requiring a local build, so the
// coordinates and per-platform SHA-256 hashes are hard-coded below. Bumping to a newly
// published release is a one-line change to `version` plus a refresh of `hashes`.
object Runners:
  val version: Text = t"0.2"

  val baseUrl: Text =
    t"https://github.com/propensive/soundness/releases/download/runners-$version"

  // Lowercase SHA-256 hex of each published stub, copied verbatim from
  // `etc/runners/$version.tsv`.
  val hashes: Map[Text, Text] =
    Map
      ( t"linux-arm64" -> t"cac8b3623b5f1131744552607b91a4efef088d8dcce6106ddb9d8fcd14132f5e",
       t"linux-x64" -> t"74bebddce65adddaf259693ff4a4efa198b8f46b1e6b4bd5508a98968a59ccd2",
       t"macos-arm64" -> t"5072d2e92749ef6c39b874977f5d2e5b79504d4351756b104c6a24537cc306a1",
       t"macos-x64" -> t"1a20699428bc7e7680c2749614db26644e87823a186df9c331583966e177da31",
       t"windows-x64" -> t"30dc92dfd8654de42d5368d1ab130d1a48a620f84168f82ef7f1c3b219a387bb" )

  // The published filename for a platform's bare runner stub (Windows stubs carry `.exe`).
  def runnerName(label: Text): Text =
    if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

  // Download the bare reusable runner stub for `label` from the published release and verify
  // it against the hard-coded SHA-256 manifest.
  def download(label: Text): Data raises RunnerError =
    val name: Text = runnerName(label)

    val expected: Text =
      hashes.at(label).lest(RunnerError(m"There is no published runner stub for platform $label"))

    mitigate:
      case HttpError(_, _)   => RunnerError(m"Could not download the stub $name from $baseUrl")
      case ConnectError(_)   => RunnerError(m"Could not connect to $baseUrl to download $name")
      case UrlError(_, _, _) => RunnerError(m"The runner stub URL for $name is not valid")
      case StreamError(_)    => RunnerError(m"The download of the stub $name was interrupted")

    . protect:
        val runner: Data = mute[HttpEvent](t"$baseUrl/$name".as[HttpUrl].fetch().read[Data])
        val actual: Text = runner.digest[Sha2[256]].serialize[Hex]

        if actual != expected
        then abort(RunnerError(m"The downloaded runner stub $name has the wrong SHA-256 ($actual)"))

        runner
