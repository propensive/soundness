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
package anthology

import ambience.*
import anticipation.*
import contingency.*
import ethereal.*
import fulminate.*
import galilei.*
import gossamer.*
import parasite.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import ziggurat.*

import errorDiagnostics.emptyDiagnostics

object xeqOptions:
  private def xeq(edit: XeqConfiguration => XeqConfiguration): Setting =
    Setting[XeqConfiguration](_.isInstanceOf[Xeq])(edit)

  // The distributable's basename within the output directory.
  def name(name: Text): Setting = xeq(_.copy(name = name))

  // Adds a target platform label (e.g. `linux-x64`); with none, every platform the runner
  // source names is targeted.
  def target(label: Text): Setting = xeq: config => config.copy(targets = label :: config.targets)

  object runners:
    // The published `runners-<version>` release, verified against its committed manifest.
    def standard: Setting = xeq(_.copy(runners = Packaging.RunnerSource.Remote(Runners.baseUrl, Runners.hashes)))

    // A local directory of prebuilt stubs (e.g. the output of `make runners-build`).
    def local(directory: Path on Linux): Setting =
      xeq(_.copy(runners = Packaging.RunnerSource.Local(directory)))

    def remote(baseUrl: Text, hashes: Map[Text, Text]): Setting =
      xeq(_.copy(runners = Packaging.RunnerSource.Remote(baseUrl, hashes)))

  def java(minimum: Int, preferred: Int): Setting =
    xeq: config =>
      config.copy(java = config.java.copy(minimum = minimum, preferred = preferred))

  object bundle:
    def jre: Setting = xeq: config =>
      config.copy(java = config.java.copy(bundle = Packaging.Bundle.Jre))

    def jdk: Setting = xeq: config =>
      config.copy(java = config.java.copy(bundle = Packaging.Bundle.Jdk))

  def signing
    ( publicKey:      Optional[Path on Linux] = Unset,
      seed:           Optional[Path on Linux] = Unset,
      allowDowngrade: Boolean                 = false )
  :   Setting =

    xeq(_.copy(signing = Packaging.Signing(publicKey, seed, allowDowngrade)))

  def buildId(id: Long): Setting = xeq(_.copy(buildId = id))

// The xeq packaging edges of a toolchain: `Jar` to each delivery mode's bundle, all a thin
// facade over `ziggurat.Packager` — which patches ethereal's reusable runner stubs, appends
// the JAR, and wraps the result in a polyglot script where the delivery calls for one.
object xeqEdges:
  def apply(): List[Edge] =
    List
      ( edge(Packaging.Delivery.EmbedAll),
        edge(Packaging.Delivery.Download),
        edge(Packaging.Delivery.Native) )

  private def edge(delivery: Packaging.Delivery): Edge =
    Edge(Jar, anthology.Xeq(delivery), XeqTool(delivery))

  private case class XeqTool(delivery: Packaging.Delivery) extends Tool:
    type Settings = XeqConfiguration

    def name: Text = anthology.Xeq(delivery).id
    def initial: XeqConfiguration = XeqConfiguration()

    def run
      ( settings:    XeqConfiguration,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], LinkEvent is Loggable )
    :   Deliverable =

      val jar = input.product(anthology.Xeq(delivery))

      val runners = settings.runners.or:
        abort(Link.Error(Link.Error.Reason.MissingSetting(t"runners")))

      // With no explicit targets, target every platform the runner source names; a local
      // directory names none, so explicit targets are required there.
      val targets: List[Text] =
        if !settings.targets.stdlib.isEmpty then settings.targets else runners.absolve match
          case Packaging.RunnerSource.Remote(_, hashes) =>
            List(hashes.stdlib.keys.toSeq.sortBy(_.s)*)

          case Packaging.RunnerSource.Local(_) =>
            abort(Link.Error(Link.Error.Reason.MissingSetting(t"targets")))

      val packaging =
        Packaging
          ( name         = settings.name,
            targets      = targets,
            delivery     = delivery,
            dependencies = Packaging.Dependencies.FatJar(jar),
            output       = unsafely(out / settings.name),
            runnerSource = runners,
            java         = settings.java,
            signing      = settings.signing,
            buildId      = settings.buildId )

      mitigate:
        case error: Packager.Error => Link.Error(Link.Error.Reason.Packaging(error.message.text))

      . protect:
          Deliverable.Product(Packager.pack(packaging))