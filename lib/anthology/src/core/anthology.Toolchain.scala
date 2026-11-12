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

// Deliberate stdlib opt-out for the graph algorithms below: queue-and-visited-set code over
// adjacency maps, bridged from the opaque collections through the greppable `stdlib` boundary.
import scala.collection.immutable as sci
import scala.collection.mutable as scm
import scala.util.control as suc

import ambience.*
import anticipation.*
import denominative.nil
import murmuration.{exists, fold, map}
import rudiments.each
import contingency.*
import digression.*
import fulminate.*
import galilei.*
import nomenclature.*
import parasite.*
import prepositional.*
import serpentine.*

object Toolchain:
  // Assembles a toolchain from any number of edge groups, as the providers supply them:
  // `Toolchain(jarEdges(), dexEdges(), apkEdges())`.
  def apply(edges: List[Edge]*): Toolchain raises Link.Error =
    // Deliberate stdlib opt-out, as noted above: Kahn's algorithm below is stdlib queue-and-map
    // code.
    val all = List.iterator(edges).flatMap(_.stdlib).toList

    all.groupBy { edge => (edge.source, edge.target) }.foreach: (pair, group) =>
      if group.length > 1
      then abort(Link.Error(Link.Error.Reason.DuplicateEdge(pair(0).id, pair(1).id)))

    // Kahn's algorithm: a toolchain whose edges cannot be fully peeled from its indegree-zero
    // nodes contains a cycle.
    val nodes = all.flatMap { edge => sci.List[Artifact](edge.source, edge.target) }.distinct
    val outgoing = all.groupBy(_.source)
    val indegree = scm.HashMap[Artifact, Int]().withDefaultValue(0)

    all.foreach: edge => indegree(edge.target) = indegree(edge.target) + 1

    var queue: sci.List[Artifact] = nodes.filter(indegree(_) == 0)
    var remaining: Int = nodes.length

    while !queue.isEmpty do
      val node = queue.head
      queue = queue.tail
      remaining -= 1

      outgoing.getOrElse(node, sci.Nil).foreach: edge =>
        indegree(edge.target) = indegree(edge.target) - 1
        if indegree(edge.target) == 0 then queue = edge.target :: queue

    if remaining > 0 then abort(Link.Error(Link.Error.Reason.CyclicToolchain))

    new Toolchain(all.to(List))

  // ToolchainError → Toolchain.Error
  case class Error(tool: Text)(using Diagnostics)
  extends fulminate.Error(586, 0)(m"the native tool $tool is not available on the PATH")

  object Setting:
    def apply[settings](applies: Artifact -> Boolean)(edit0: settings -> settings): Setting =
      new Setting:
        def appliesTo(format: Artifact): Boolean = applies(format)
        def edit(format: Artifact, settings0: Any): Any = edit0(settings0.asInstanceOf[settings])

  // A tool setting, addressed to the formats whose production it configures: a `Toolchain`
  // applies it, in order, to the initial settings of every path edge whose target satisfies
  // `appliesTo`. Contract: `appliesTo(format)` implies `edit(format, _)` accepts and returns the
  // settings type of every edge targeting `format`. A setting spanning formats with different
  // settings types (an Android API level configures both dexing and packaging) dispatches on its
  // `format` argument.
  trait Setting:
    def appliesTo(format: Artifact): Boolean
    def edit(format: Artifact, settings: Any): Any

  object Triple:
    import anticipation.*
    import gossamer.*
    import vacuous.*

    // The build host's target, read from the JVM's `os.name` and `os.arch` properties, or
    // `Unset` on a platform outside the enumeration.
    def host: Optional[Triple] =
      val os = java.lang.System.getProperty("os.name").nn.toLowerCase.nn
      val arch = java.lang.System.getProperty("os.arch").nn.toLowerCase.nn
      val arm = arch.contains("aarch64") || arch.contains("arm64")

      if os.contains("mac") then (if arm then Arm64MacOs else X64MacOs)
      else if os.contains("linux") then (if arm then Arm64Linux else X64Linux)
      else if os.contains("windows") then (if arm then Arm64Windows else X64Windows)
      else Unset

  // A target platform for a native link, rendered as an LLVM target triple. Targets beyond the
  // build host require a C toolchain (and sysroot) capable of cross-compilation.
  enum Triple:
    import anticipation.*
    import gossamer.*
    import vacuous.*

    case Arm64MacOs, X64MacOs, Arm64Linux, X64Linux, Arm64Windows, X64Windows

    def text: Text = this match
      case Arm64MacOs   => t"arm64-apple-darwin"
      case X64MacOs     => t"x86_64-apple-darwin"
      case Arm64Linux   => t"aarch64-unknown-linux-gnu"
      case X64Linux     => t"x86_64-unknown-linux-gnu"
      case Arm64Windows => t"aarch64-pc-windows-msvc"
      case X64Windows   => t"x86_64-pc-windows-msvc"

  object Universe:
    import anticipation.*
    import gossamer.*
    import serpentine.*

    type Classfile = Classfile.type
    type Sjsir = Sjsir.type
    type Nir = Nir.type

    // Determines the additional compiler flags each universe's emission requires.
    trait Emission[universe <: Universe]:
      def flags: List[Text]

    given classfile: Emission[Classfile]:
      def flags: List[Text] = Nil

    given sjsir: Emission[Sjsir]:
      def flags: List[Text] = List(t"-scalajs")

    // NIR is emitted by the Scala Native compiler plugin rather than by a backend built into the
    // compiler, so compiling into the NIR universe requires evidence of the plugin's location.
    given nir(using plugin: NirPlugin): Emission[Nir] =
      new Emission[Nir]:
        def flags: List[Text] = List(t"-Xplugin:${plugin.jar.encode}")

  // The universe a compilation inhabits: the intermediate representation it emits, and hence the
  // ecosystem of library artifacts it can link with. `Classfile` is JVM classfiles; `Sjsir` is
  // Scala.js IR, whose linked representation (JavaScript, browser Wasm or a WASI component) is
  // chosen at link time; `Nir` is Scala Native IR, linked to machine code through LLVM. Each
  // universe is an intermediate-representation node of a `Toolchain`.
  // The `id` is a parameter rather than a match on the case: matching a case compares formats with
  // `equals`, which compares their `id`s.
  enum Universe(val id: Text) extends Artifact.Ir:
    import anticipation.*
    import gossamer.*
    import serpentine.*

    case Classfile extends Universe("classfile".tt)
    case Sjsir extends Universe("sjsir".tt)
    case Nir extends Universe("nir".tt)

    // The label of the LIRA section holding this universe's content.
    def section: Text = this match
      case Classfile => t"jvm"
      case Sjsir     => t"sjsir"
      case Nir       => t"nir"

    // The filename suffixes of this universe's stored representations: each universe's binary
    // form, plus the TASTy that carries its interface.
    def suffixes: List[Text] = this match
      case Classfile => List(t".class", t".tasty")
      case Sjsir     => List(t".sjsir", t".tasty")
      case Nir       => List(t".nir", t".tasty")

  // A toolchain edge's implementation: a compiler, linker or packager, together with the settings
  // type configuring it and its default configuration. Tools are inert values: the capabilities
  // their execution needs (asynchrony, system properties, a working directory) flow through
  // `run`'s context rather than being captured at construction.
  trait Tool:
    import ambience.*
    import anticipation.*
    import contingency.*
    import galilei.*
    import parasite.*
    import prepositional.*
    import serpentine.*

    type Settings

    def name: Text
    def initial: Settings

    def run
      ( settings:    Settings,
        input:       Deliverable,
        entryPoints: List[EntryPoint],
        out:         Path on Linux )
      ( using Monitor, System, WorkingDirectory )
      ( using Tactic[Link.Error], (LinkEvent is Loggable)^ )
    :   Deliverable

// An in-memory toolchain: a directed acyclic graph whose nodes are formats and whose edges are
// tools. Producing one format from another is path search: the unique shortest edge sequence
// between the two nodes determines the tools to run, in order. The node set is open—registering
// an edge for a new format extends the graph—and a format two or more edges away is reached
// through each intermediate product in turn, so multi-stage pipelines (classfiles dexed then
// packaged as an APK; a WASI component wrapped as an OCI artifact) need no bespoke composition.
case class Toolchain private (edges: List[Edge]):

  // The unique shortest path between two formats. No route raises `NoPath`; two or more
  // shortest routes raise `AmbiguousPath`, resolved by producing an explicit intermediate
  // format in a separate step.
  def path(source: Artifact, target: Artifact): List[Edge] raises Link.Error =
    if source == target then Nil else
      // Deliberate stdlib opt-out, as noted above: the BFS below is stdlib queue-and-map code.
      val adjacency = edges.stdlib.groupBy(_.source)

      // Level-synchronous breadth-first search, counting shortest routes (capped at two) to
      // detect ambiguity, and recording each node's discovering edge for reconstruction.
      val discovery = scm.HashMap[Artifact, Edge]()
      val routes = scm.HashMap[Artifact, Int](source -> 1)
      var frontier: sci.List[Artifact] = sci.List(source)
      var visited: sci.Set[Artifact] = sci.Set(source)

      while !frontier.isEmpty && !visited.contains(target) do
        val next = scm.LinkedHashMap[Artifact, Int]()

        frontier.foreach: node =>
          adjacency.getOrElse(node, sci.Nil).foreach: edge =>
            if !visited.contains(edge.target) then
              if !next.contains(edge.target) then discovery(edge.target) = edge
              next(edge.target) = (next.getOrElse(edge.target, 0) + routes(node)).min(2)

        next.foreach: (node, count) => routes(node) = count
        visited = visited ++ next.keySet
        frontier = next.keys.to(sci.List)

      if !visited.contains(target)
      then abort(Link.Error(Link.Error.Reason.NoPath(source.id, target.id)))

      if routes(target) > 1
      then abort(Link.Error(Link.Error.Reason.AmbiguousPath(source.id, target.id)))

      var route: List[Edge] = Nil
      var current: Artifact = target

      while current != source do
        val edge = discovery(current)
        route = edge :: route
        current = edge.source

      route

  // Produces `target`-format output from `source`-format input by running each tool on the
  // path between them: each intermediate product is written below `out` under its format's
  // `id`, and the final product to `out` itself. Each setting configures every path edge whose
  // target it applies to; a setting applying to none is rejected as inapplicable.
  def produce
    ( input:       Deliverable,
      source:      Artifact,
      target:      Artifact,
      out:         Path on Linux,
      settings:    List[Toolchain.Setting] = Nil,
      entryPoints: List[EntryPoint] = Nil )
    ( using Monitor, System, WorkingDirectory )
    ( using Tactic[Link.Error], (LinkEvent is Loggable)^ )
  :   Path on Linux =

    val route = path(source, target)

    settings.each: setting =>
      if !route.map(_.target).exists(setting.appliesTo(_))
      then abort(Link.Error(Link.Error.Reason.InapplicableSetting))

    Log.info(LinkEvent.Start)

    def step(edge: Edge, current: Deliverable, destination: Path on Linux): Deliverable =
      val tool: edge.tool.type = edge.tool

      val configured: tool.Settings =
        settings.fold(tool.initial): (settings0, setting) =>
          if setting.appliesTo(edge.target) then
            try setting.edit(edge.target, settings0).asInstanceOf[tool.Settings]
            catch case suc.NonFatal(error) =>
              abort(Link.Error(Link.Error.Reason.Failed(error.stackTrace)))
          else
            settings0

      tool.run(configured, current, entryPoints, destination)

    def walk(remaining: List[Edge], current: Deliverable): Deliverable = remaining match
      case edge :: rest =>
        if rest.nil then step(edge, current, out)
        else
          // A declared artifact's id is the user's; one that is not a file name is reported.
          val directory =
            mitigate:
              case error: Name.Error =>
                Link.Error(Link.Error.Reason.Packaging(error.message.text))(using error.diagnostics)

            . protect(out / edge.target.id)

          walk(rest, step(edge, current, directory))

      case _ =>
        current

    walk(route, input) match
      case Deliverable.Product(file) =>
        Log.info(LinkEvent.Linked(file.encode))
        file

      case Deliverable.Emission(directory, _) =>
        Log.info(LinkEvent.Linked(directory.encode))
        directory

      case _ =>
        abort(Link.Error(Link.Error.Reason.UnexpectedInput(target.id)))
