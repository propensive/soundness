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
import rudiments.reverse
import contingency.*
import digression.*
import fulminate.*
import galilei.*
import parasite.*
import prepositional.*
import serpentine.*

object Toolchain:
  // Assembles a toolchain from any number of edge groups, as the providers supply them:
  // `Toolchain(jarEdges(), dexEdges(), apkEdges())`.
  def apply(edges: List[Edge]*): Toolchain raises Link.Error =
    val all = edges.toList.flatMap(_.stdlib)

    all.groupBy { edge => (edge.source, edge.target) }.foreach: (pair, group) =>
      if group.length > 1
      then abort(Link.Error(Link.Error.Reason.DuplicateEdge(pair(0).id, pair(1).id)))

    // Kahn's algorithm: a toolchain whose edges cannot be fully peeled from its indegree-zero
    // nodes contains a cycle.
    val nodes = all.flatMap { edge => sci.List[Format](edge.source, edge.target) }.distinct
    val outgoing = all.groupBy(_.source)
    val indegree = scm.HashMap[Format, Int]().withDefaultValue(0)

    all.foreach: edge => indegree(edge.target) = indegree(edge.target) + 1

    var queue: sci.List[Format] = nodes.filter(indegree(_) == 0)
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
    def apply[settings](applies: Format -> Boolean)(edit0: settings -> settings): Setting =
      new Setting:
        def appliesTo(format: Format): Boolean = applies(format)
        def edit(format: Format, settings0: Any): Any = edit0(settings0.asInstanceOf[settings])

  // A tool setting, addressed to the formats whose production it configures: a `Toolchain`
  // applies it, in order, to the initial settings of every path edge whose target satisfies
  // `appliesTo`. Contract: `appliesTo(format)` implies `edit(format, _)` accepts and returns the
  // settings type of every edge targeting `format`. A setting spanning formats with different
  // settings types (an Android API level configures both dexing and packaging) dispatches on its
  // `format` argument.
  trait Setting:
    def appliesTo(format: Format): Boolean
    def edit(format: Format, settings: Any): Any

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
  def path(source: Format, target: Format): List[Edge] raises Link.Error =
    if source == target then Nil else
      val adjacency = edges.stdlib.groupBy(_.source)

      // Level-synchronous breadth-first search, counting shortest routes (capped at two) to
      // detect ambiguity, and recording each node's discovering edge for reconstruction.
      val discovery = scm.HashMap[Format, Edge]()
      val routes = scm.HashMap[Format, Int](source -> 1)
      var frontier: sci.List[Format] = sci.List(source)
      var visited: sci.Set[Format] = sci.Set(source)

      while !frontier.isEmpty && !visited.contains(target) do
        val next = scm.LinkedHashMap[Format, Int]()

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
      var current: Format = target

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
      source:      Format,
      target:      Format,
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
        else walk(rest, step(edge, current, unsafely(out / edge.target.id)))

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
