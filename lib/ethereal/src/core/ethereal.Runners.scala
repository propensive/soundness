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
import gastronomy.*, providers.javaBaseProvider
import httpBackends.javaNetHttp
import internetAccess.online
import monotonous.*, alphabets.hexLowerCase

// The reusable native runner stubs are published independently of any application as a
// GitHub release (`runners-<version>`), and verified against the committed
// `etc/runners/<version>.tsv` manifest. The `-Dbuild.executable` self-packaging path
// downloads the one stub it needs from here rather than requiring a local build, so the
// coordinates and per-platform SHA-256 hashes are hard-coded below. Bumping to a newly
// published release is a one-line change to `version` plus a refresh of `hashes`.
object Runners:
  val version: Text = t"0.5"

  val baseUrl: Text =
    t"https://github.com/propensive/soundness/releases/download/runners-$version"

  // Lowercase SHA-256 hex of each published stub, copied verbatim from
  // `etc/runners/$version.tsv`.
  val hashes: Map[Text, Text] =
    Map
      ( t"linux-arm64" -> t"754f5619ad68cec0be32b12ce464456684e2ceefa9bcf131ed7e1bb39975b952",
       t"linux-x64" -> t"f71b2a9c778758617f926df66fdd38fc6b7de8420e96c2527d1bc97c35b9c43b",
       t"macos-arm64" -> t"22dec50412226ce53765bf2f77d897ff44ae39f13f4463b8f26dff5595c4d136",
       t"macos-x64" -> t"3cf5cd1554752c5c77e792d414dde6aa0595d0d6589c69a4b3c471c52c5903c8",
       t"windows-x64" -> t"f6d84f6fbe589419bd32000ed2d8e6ae4f3ed1717eb5712e253cf860bd160a9f" )

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
