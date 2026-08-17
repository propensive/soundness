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
package reliquary

import anticipation.*
import aperture.*
import contingency.*
import galilei.*
import gossamer.*
import hellenism.*
import monotonous.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*
import turbulence.*

import Lira.Error.Reason
import alphabets.hexLowerCase
import filesystemBackends.virtualMachineFilesystem
import filesystemOptions.overwritePreexisting.enabled

// Derivation of conventional artifacts (§13.5): from a valid buildpath and a chosen universe,
// each release's section is materialized and serialized as its canonical derivative JAR into a
// content-addressed cache keyed by implementation identity — a cache hit therefore costs no
// decompression — and the classpath is the list of cached JARs.
object Materializer:

  def classpath(liras: List[Lira], universe: Text, cache: Path on Linux)
  :   LocalClasspath raises Lira.Error raises Io.Error raises Truncation.Error =

    // `host` sections are never materialized onto any artifact path (§13.5): a host contract's
    // content describes the environment and joins nothing.
    if universe == t"host"
    then abort(Lira.Error(Reason.BadHostContract(t"the host realm derives no artifacts")))

    val path = Buildpath(liras.map(_.manifest))
    val (assignment, _) = path.resolved(universe)

    // §13.5: one artifact set *per universe of the target*. A release serving a joined universe
    // (named by a `serves` record, §13.2) belongs to that universe's derivation, not this one.
    val serving = liras.stdlib.filter: lira =>
      path.serving(universe, lira.manifest.module) == universe

    val jars = serving.map: lira =>
      val identity = lira.manifest.payload.hash.serialize[Hex]
      val integration = assignment(lira.manifest.module)

      // The cache slot is per (release, universe, integration): integrations share a payload
      // hash by definition, so keying on universe alone would let the first one materialized
      // stand in for every other one, permanently.
      val slot = integration.let { id => t"$universe.$id.jar" }.or(t"$universe.jar")

      // The segments are a hex digest and a filename, admissible on any filesystem.
      val target = unsafely((cache / identity / slot).on[Linux])

      if !target.existent() then
        val report = Verification.install(lira)

        val tree = report.tree(universe, integration) match
          case tree: Lira.Tree => tree

          // §13.5 calls this a validation-time error, and it is closure's kind (L113): a
          // release with no section serving the target universe fails the buildpath exactly as
          // an absent module does. The manifest itself is not at fault.
          case _ =>
            abort(Lira.Error(Reason.AbsentDependency(lira.manifest.module)))

        val data = Derivative.jar(tree, report.blobstore)

        target.create[File](CreateFlag.Parents, CreateFlag.Replace): handle ?=>
          handle.write(Chain(data))

      Classpath.Entry.Jar(target.encode)

    LocalClasspath(jars*)
