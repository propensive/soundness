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
package acyclicity

// Deliberate stdlib opt-out: the layering's working structures are stdlib collections — this
// import shadows the opaque collections for the whole file, greppably, as `acyclicity.Dag.scala`
// does — and what it reads from the `Dag` is converted at the boundary.
import scala.collection.immutable.{List, Map, Nil, Set}
import scala.collection.mutable as scm

object Ranking:
  // The companion default, outranked by a `rankings` given imported by name.
  given default: Ranking = Ranking.LongestPath

// How `Dag#layered` assigns each node to a layer. `LongestPath` places every node one layer
// below its deepest dependency, so sources share layer 0; `Balanced` then pulls every node
// with dependents down to the layer above its shallowest dependent, shortening long edges.
enum Ranking:
  case LongestPath, Balanced

object Layering:
  // A slot in a layer: either a node of the graph, or the point at which an edge spanning
  // several layers passes through this one, so every link joins adjacent layers.
  enum Vertex[node]:
    case Real(value: node)
    case Virtual(source: node, target: node)

  private type Adjacency[node] = scm.LinkedHashMap[Vertex[node], List[Vertex[node]]]

  // How many sweeps the crossing reduction runs before settling for the best ordering it has
  // seen; dot uses the same bound.
  private val iterations: Int = 24

  // Whether the links `a` and `b`, as (upper, lower) positions, cross.
  private def crossed(a: (Int, Int), b: (Int, Int)): Boolean =
    (a(0) < b(0) && a(1) > b(1)) || (a(0) > b(0) && a(1) < b(1))

  // A node's successors on the stdlib `Set` the arithmetic below runs on.
  private def successors[node](dag: Dag[node], node: node): Set[node] =
    proscenium.Set.iterator(dag.successors(node)).to(Set)

  private[acyclicity] def crossingsAmong(links: List[(Int, Int)]): Int =
    links.zipWithIndex.map { (a, i) => links.drop(i + 1).count(crossed(a, _)) }.sum

  // A `Dag` is acyclic by construction, so there is nothing here that can fail.
  private[acyclicity] def apply[node](dag: Dag[node], ranking: Ranking): Layering[node] =
    val order: List[node] = proscenium.List.iterator(dag.linearized).to(List)

    if order.isEmpty then Layering(Nil, Nil) else
      val topo: Map[node, Int] = order.zipWithIndex.to(Map)
      val dependents: Dag[node] = dag.invert
      val rank: scm.HashMap[node, Int] = scm.HashMap()

      order.foreach: n => rank(n) = successors(dag, n).map(rank).maxOption.fold(0)(_ + 1)

      if ranking == Ranking.Balanced then order.reverse.foreach: n =>
        val below = successors(dependents, n)
        if below.nonEmpty then rank(n) = below.map(rank).min - 1

      val depth: Int = rank.values.max + 1

      // Adjacency of the working graph: `down(i)` maps each vertex of layer `i` to its
      // neighbours in layer `i + 1`, `up(i + 1)` the reverse, both in a deterministic order.
      val down: scala.Array[Adjacency[node]] = scala.Array.fill(depth)(scm.LinkedHashMap())
      val up: scala.Array[Adjacency[node]] = scala.Array.fill(depth)(scm.LinkedHashMap())

      def link(layer: Int, upper: Vertex[node], lower: Vertex[node]): Unit =
        down(layer)(upper) = lower :: down(layer).getOrElse(upper, Nil)
        up(layer + 1)(lower) = upper :: up(layer + 1).getOrElse(lower, Nil)

      // Every vertex, in the order the layers are first populated.
      val members: scala.Array[scm.LinkedHashSet[Vertex[node]]] =
        scala.Array.fill(depth)(scm.LinkedHashSet())

      order.foreach: child =>
        members(rank(child)) += Vertex.Real(child)

        successors(dag, child).to(List).sortBy(topo).foreach: parent =>
          val between: List[(Int, Vertex[node])] =
            (rank(parent) + 1 until rank(child)).to(List).map(_ -> Vertex.Virtual(parent, child))

          val ends = (rank(parent) -> Vertex.Real(parent), rank(child) -> Vertex.Real(child))
          val chain: List[(Int, Vertex[node])] = ends(0) :: between ::: List(ends(1))

          chain.foreach: (layer, vertex) => members(layer) += vertex

          chain.zip(chain.tail).foreach:
            case ((layer, upper), (_, lower)) => link(layer, upper, lower)

      // The neighbour lists were built by prepending; restore first-seen order.
      down.foreach(_.mapValuesInPlace { (_, vs) => vs.reverse })
      up.foreach(_.mapValuesInPlace { (_, vs) => vs.reverse })

      // The current ordering of each layer, and each vertex's position within it.
      val layers: scala.Array[scm.ArrayBuffer[Vertex[node]]] =
        scala.Array.fill(depth)(scm.ArrayBuffer())

      val position: scala.Array[scm.HashMap[Vertex[node], Int]] =
        scala.Array.fill(depth)(scm.HashMap())

      def reindex(layer: Int): Unit =
        position(layer).clear()

        layers(layer).zipWithIndex.foreach: (v, i) => position(layer)(v) = i

      // Initial order: layer 0 in topological order, each later layer by first reach from
      // the layer above, with anything unreached (a source sunk by `Balanced`) appended.
      layers(0) ++= members(0)
      reindex(0)

      (1 until depth).foreach: layer =>
        val reached = scm.LinkedHashSet[Vertex[node]]()

        layers(layer - 1).foreach: v => reached ++= down(layer - 1).getOrElse(v, Nil)

        reached ++= members(layer)
        layers(layer) ++= reached
        reindex(layer)

      // The weighted median of a vertex's neighbours' positions in the fixed adjacent layer,
      // or -1 for a vertex with no neighbours there, which keeps its place.
      def median(neighbours: List[Vertex[node]], fixed: Int): Double =
        val sorted = neighbours.map(position(fixed)).sorted.to(scala.IndexedSeq)
        val count = sorted.length
        val middle = count/2

        if count == 0 then -1.0
        else if count%2 == 1 then sorted(middle).toDouble
        else if count == 2 then (sorted(0) + sorted(1))/2.0
        else
          val left = (sorted(middle - 1) - sorted(0)).toDouble
          val right = (sorted(count - 1) - sorted(middle)).toDouble
          (sorted(middle - 1)*right + sorted(middle)*left)/(left + right)

      // Reorders a layer by its vertices' medians in the fixed adjacent layer, leaving the
      // vertices with no neighbours there where they are.
      def reorder(layer: Int, adjacency: Adjacency[node], fixed: Int): Unit =
        val medians: Map[Vertex[node], Double] =
          layers(layer).map { v => v -> median(adjacency.getOrElse(v, Nil), fixed) }.to(Map)

        val anchored: Set[Int] =
          layers(layer).zipWithIndex.collect { case (v, i) if medians(v) < 0 => i }.to(Set)

        val movable: scm.Queue[Vertex[node]] =
          scm.Queue.from(layers(layer).filter(medians(_) >= 0).sortBy(medians))

        val next = layers(layer).zipWithIndex.map: (v, i) =>
          if anchored(i) then v else movable.dequeue()

        layers(layer).clear()
        layers(layer) ++= next
        reindex(layer)

      // Crossings between the links of `left` and `right`, in that order, with both adjacent
      // layers fixed; comparing this against the reverse order decides a transposition.
      def crossingsOf(layer: Int, left: Vertex[node], right: Vertex[node]): Int =
        def count(adjacency: Adjacency[node], fixed: Int): Int =
          val lefts = adjacency.getOrElse(left, Nil).map(position(fixed))
          val rights = adjacency.getOrElse(right, Nil).map(position(fixed))
          lefts.map { a => rights.count(_ < a) }.sum

        val above = if layer > 0 then count(up(layer), layer - 1) else 0
        val below = if layer < depth - 1 then count(down(layer), layer + 1) else 0
        above + below

      // Swaps adjacent vertices while a swap removes crossings, settling by recursion.
      def transpose(layer: Int): Unit =
        val improved = (0 until layers(layer).length - 1).foldLeft(false): (state, i) =>
          val left = layers(layer)(i)
          val right = layers(layer)(i + 1)

          if crossingsOf(layer, left, right) > crossingsOf(layer, right, left) then
            layers(layer)(i) = right
            layers(layer)(i + 1) = left
            reindex(layer)
            true
          else
            state

        if improved then transpose(layer)

      def linksBelow(layer: Int): List[(Int, Int)] =
        layers(layer).to(List).flatMap: u =>
          down(layer).getOrElse(u, Nil).map: l => position(layer)(u) -> position(layer + 1)(l)

      def crossings: Int = (0 until depth - 1).map { i => crossingsAmong(linksBelow(i)) }.sum

      var best: Int = crossings
      var bestLayers: List[List[Vertex[node]]] = layers.iterator.map(_.to(List)).to(List)

      // Alternating sweeps: downward fixes the layer above, upward the layer below; the
      // search stops when the drawing is planar or two sweeps in a row find nothing.
      def sweep(iteration: Int, stale: Int): Unit =
        if best > 0 && iteration < iterations && stale < 2 then
          if iteration%2 == 0 then
            (1 until depth).foreach: layer => reorder(layer, up(layer), layer - 1)
          else
            (depth - 2 to 0 by -1).foreach: layer => reorder(layer, down(layer), layer + 1)

          (0 until depth).foreach(transpose)

          val current = crossings

          if current < best then
            best = current
            bestLayers = layers.iterator.map(_.to(List)).to(List)
            sweep(iteration + 1, 0)
          else
            sweep(iteration + 1, stale + 1)

      sweep(0, 0)

      val finalPosition: List[Map[Vertex[node], Int]] = bestLayers.map(_.zipWithIndex.to(Map))

      val links: List[List[(Int, Int)]] =
        bestLayers.zipWithIndex.init.map: (layer, i) =>
          layer.flatMap: u =>
            down(i).getOrElse(u, Nil).map: l => finalPosition(i)(u) -> finalPosition(i + 1)(l)

      Layering(bestLayers, links)

// The result of `Dag#layered`: the nodes arranged in layers, dependencies above dependents,
// each layer ordered to reduce the crossings between the links joining it to its neighbours.
// `links(i)` joins layer `i` to layer `i + 1` as pairs of positions within each.
case class Layering[node]
  ( layers: List[List[Layering.Vertex[node]]],
    links:  List[List[(Int, Int)]] ):

  import Layering.Vertex

  lazy val rank: Map[node, Int] =
    layers.zipWithIndex.flatMap: (layer, i) =>
      layer.collect { case Vertex.Real(n) => n -> i }

    . to(Map)

  lazy val position: Map[node, Int] =
    layers.flatMap(_.zipWithIndex.collect { case (Vertex.Real(n), p) => n -> p }).to(Map)

  lazy val crossings: Int = links.map(Layering.crossingsAmong).sum
