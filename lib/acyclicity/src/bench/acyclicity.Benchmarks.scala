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

import java.lang.Integer

import scala.collection.immutable.{List, Map, Nil, Set, ::}
import scala.collection.mutable as scm
import scala.jdk.CollectionConverters.*
import scala.quoted.*

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import quantitative.*
import rudiments.*
import sedentary.*
import superlunary.embeddings.automaticEmbedding
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge, DirectedAcyclicGraph, EdgeReversedGraph}
import com.google.common.graph.{GraphBuilder, Graphs, MutableGraph, Traverser}

// The shape of the input. Node `i` depends only on nodes below it, so every generated graph is
// acyclic by construction, and each shape stresses something different: depth, fan-in, diamonds
// (and so the size of the closure), density, or the two-to-four dependencies of a real build.
enum Shape:
  case Chain, Tree, Layered, BuildSystem, Sparse, Dense

// The implementations under comparison: `Current` is `acyclicity.Dag` as it is in core; the next
// four are the candidates in this module; `Stdlib` is the hand-rolled baseline; the last two are
// the rival libraries, written as their own users write them.
enum Engine:
  case Current, Repaired, Mirrored, Frozen, Workspace, Stdlib, JGraphT, Guava

// One staged tree serves every cell: the operation, engine and shape travel as ordinals, so that
// the compiled body is shared and only the dispatch — two `match`es, nanoseconds against
// microseconds and up — differs between cells.
enum Operation:
  case Construct, ConstructReversed, Build, Copy, Sorted, Reachable, Sources, Sinks, Closure,
      Reduction, Invert, AddBatch, Bypass

object Benchmarks extends Suite(m"Acyclicity benchmarks"):
  given decimalizer: Decimalizer     = Decimalizer(2)
  given device:      BenchmarkDevice = LocalhostDevice

  // ─── input data ───────────────────────────────────────────────────────────

  // `from` depends on `to`, pairwise. Built once per (shape, size) and cached; the lookup is
  // inside the timed region, identical for every engine.
  case class Edges(count: Int, from: scala.IArray[Int], to: scala.IArray[Int])

  private val datasets: scm.HashMap[(Int, Int), Edges] = scm.HashMap()

  def edges(shape: Int, size: Int): Edges =
    datasets.getOrElseUpdate((shape, size), generate(Shape.fromOrdinal(shape), size))

  // A fixed multiplier rather than a random source, so every run generates the same graphs.
  private def generate(shape: Shape, size: Int): Edges =
    val from = scm.ArrayBuffer[Int]()
    val to = scm.ArrayBuffer[Int]()
    val seen = scm.HashSet[Long]()
    var state = 0x9e3779b97f4a7c15L

    def random(bound: Int): Int =
      state = state*6364136223846793005L + 1442695040888963407L
      ((state >>> 33)%bound).toInt

    def edge(source: Int, target: Int): Unit =
      if target < source && seen.add((source.toLong << 32) | target.toLong) then
        from += source
        to += target

    val width = scala.math.sqrt(size.toDouble).toInt.max(1)
    var node = 1

    while node < size do
      shape match
        case Shape.Chain => edge(node, node - 1)
        case Shape.Tree  => edge(node, (node - 1)/2)

        case Shape.Layered =>
          val layer = node/width
          if layer > 0 then
            var k = 0
            while k < 3 do
              edge(node, (layer - 1)*width + random(width))
              k += 1

        case Shape.BuildSystem =>
          val dependencies = 2 + random(3)
          var k = 0
          while k < dependencies do
            edge(node, node - 1 - random(node.min(32)))
            k += 1

        case Shape.Sparse =>
          var k = 0
          while k < 2 do
            edge(node, random(node))
            k += 1

        case Shape.Dense =>
          var k = 0
          while k < node/4 do
            edge(node, random(node))
            k += 1

      node += 1

    Edges(size, scala.IArray.from(from), scala.IArray.from(to))

  // The built graph of each engine, per (shape, size), so the query cells time the query. The
  // workspace is exclusive and cannot be held here; its cells build inside the timed body, with
  // a `Build`-only cell to subtract.
  private val graphs: scm.HashMap[(Int, Int, Int), AnyRef] = scm.HashMap()

  private def cached[graph](engine: Engine, shape: Int, size: Int)(build: => graph): graph =
    graphs.getOrElseUpdate((engine.ordinal, shape, size), build.asInstanceOf[AnyRef]).asInstanceOf[graph]

  // ─── builders ─────────────────────────────────────────────────────────────

  // `Dag` shares its representation with `AdjacencyDag`, so it is built from that map and has
  // no construction cell of its own; what differs between them is every algorithm.
  def buildCurrent(data: Edges): Dag[Int] = Dag(buildRepaired(data).adjacency)
  def buildRepaired(data: Edges): AdjacencyDag[Int] = AdjacencyDag(data.count, data.from, data.to)
  def buildMirrored(data: Edges): MirroredDag[Int] = MirroredDag(data.count, data.from, data.to)
  def buildFrozen(data: Edges): FrozenDag[Int] = buildRepaired(data).freeze
  def buildStdlib(data: Edges): StdlibDag = StdlibDag(data.count, data.from, data.to)

  inline def box(value: Int): Integer = Integer.valueOf(value).nn

  def buildJGraphT(data: Edges): DirectedAcyclicGraph[Integer, DefaultEdge] =
    val graph = new DirectedAcyclicGraph[Integer, DefaultEdge](classOf[DefaultEdge])
    var index = 0

    while index < data.count do
      graph.addVertex(box(index))
      index += 1

    index = 0

    while index < data.from.length do
      graph.addEdge(box(data.from(index)), box(data.to(index)))
      index += 1

    graph

  def buildJGraphTReversed(data: Edges): DirectedAcyclicGraph[Integer, DefaultEdge] =
    val graph = new DirectedAcyclicGraph[Integer, DefaultEdge](classOf[DefaultEdge])
    var index = data.count - 1

    while index >= 0 do
      graph.addVertex(box(index))
      index -= 1

    index = data.from.length - 1

    while index >= 0 do
      graph.addEdge(box(data.from(index)), box(data.to(index)))
      index -= 1

    graph

  def copyJGraphT(graph: DirectedAcyclicGraph[Integer, DefaultEdge])
  :   DirectedAcyclicGraph[Integer, DefaultEdge] =
    val copy = new DirectedAcyclicGraph[Integer, DefaultEdge](classOf[DefaultEdge])
    org.jgrapht.Graphs.addGraph(copy, graph)
    copy

  def buildGuava(data: Edges): MutableGraph[Integer] =
    val graph = GraphBuilder.directed().nn.allowsSelfLoops(false).nn.build[Integer]().nn
    var index = 0

    while index < data.count do
      graph.addNode(box(index))
      index += 1

    index = 0

    while index < data.from.length do
      graph.putEdge(box(data.from(index)), box(data.to(index)))
      index += 1

    graph

  // ─── the measured operations ──────────────────────────────────────────────

  private def current(shape: Int, size: Int): Dag[Int] =
    cached(Engine.Current, shape, size)(buildCurrent(edges(shape, size)))

  private def repaired(shape: Int, size: Int): AdjacencyDag[Int] =
    cached(Engine.Repaired, shape, size)(buildRepaired(edges(shape, size)))

  private def mirrored(shape: Int, size: Int): MirroredDag[Int] =
    cached(Engine.Mirrored, shape, size)(buildMirrored(edges(shape, size)))

  private def frozen(shape: Int, size: Int): FrozenDag[Int] =
    cached(Engine.Frozen, shape, size)(buildFrozen(edges(shape, size)))

  private def stdlib(shape: Int, size: Int): StdlibDag =
    cached(Engine.Stdlib, shape, size)(buildStdlib(edges(shape, size)))

  private def jgrapht(shape: Int, size: Int): DirectedAcyclicGraph[Integer, DefaultEdge] =
    cached(Engine.JGraphT, shape, size)(buildJGraphT(edges(shape, size)))

  private def guava(shape: Int, size: Int): MutableGraph[Integer] =
    cached(Engine.Guava, shape, size)(buildGuava(edges(shape, size)))

  // The entry point of every staged body. Each arm answers an `Int` derived from its result, so
  // that nothing it computes is dead.
  def measure(operation: Int, engine: Int, shape: Int, size: Int): Int =
    val which = Engine.fromOrdinal(engine)

    Operation.fromOrdinal(operation) match
      case Operation.Construct         => construct(which, shape, size)
      case Operation.ConstructReversed => constructReversed(which, shape, size)
      case Operation.Build             => build(which, shape, size)
      case Operation.Copy              => copy(which, shape, size)
      case Operation.Sorted            => sorted(which, shape, size)
      case Operation.Reachable         => reachable(which, shape, size)
      case Operation.Sources           => sources(which, shape, size)
      case Operation.Sinks             => sinks(which, shape, size)
      case Operation.Closure           => closure(which, shape, size)
      case Operation.Reduction         => reduction(which, shape, size)
      case Operation.Invert            => invert(which, shape, size)
      case Operation.AddBatch          => addBatch(which, shape, size)
      case Operation.Bypass            => bypass(which, shape, size)

  // Construction from the edge arrays to a queryable graph: for the workspace, that includes
  // freezing; `Build` below is the workspace alone.
  def construct(engine: Engine, shape: Int, size: Int): Int =
    val data = edges(shape, size)

    engine match
      case Engine.Repaired => buildRepaired(data).size
      case Engine.Mirrored => buildMirrored(data).size
      case Engine.Frozen   => buildFrozen(data).size
      case Engine.Stdlib   => buildStdlib(data).size
      case Engine.JGraphT  => buildJGraphT(data).vertexSet().nn.size
      case Engine.Guava    => buildGuava(data).nodes().nn.size

      case Engine.Workspace =>
        val workspace: Workspace[Int]^ = Workspace(data.count, data.from, data.to)
        FrozenDag(workspace).size

      case Engine.Current  => 0

  // The edges in reverse order, so the two engines that keep a topological order as edges
  // arrive (Pearce–Kelly in both) have to repair it; the persistent form is the scale.
  def constructReversed(engine: Engine, shape: Int, size: Int): Int =
    val data = edges(shape, size)

    engine match
      case Engine.Repaired => buildRepaired(data).size
      case Engine.JGraphT  => buildJGraphTReversed(data).vertexSet().nn.size

      case Engine.Workspace =>
        val workspace: Workspace[Int]^ = Workspace.reversed(data.count, data.from, data.to)
        workspace.size

      case _ => 0

  def build(engine: Engine, shape: Int, size: Int): Int =
    val data = edges(shape, size)

    engine match
      case Engine.Workspace =>
        val workspace: Workspace[Int]^ = Workspace(data.count, data.from, data.to)
        workspace.size

      case _ => 0

  // The copy that the mutable engines' editing cells pay before they edit.
  def copy(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Workspace =>
      val workspace: Workspace[Int]^ = Workspace(repaired(shape, size))
      workspace.size

    case Engine.Stdlib  => stdlib(shape, size).copy.size
    case Engine.JGraphT => copyJGraphT(jgrapht(shape, size)).vertexSet().nn.size
    case Engine.Guava   => Graphs.copyOf(guava(shape, size)).nn.nodes().nn.size
    case _              => 0

  def sorted(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).sorted.length
    case Engine.Repaired => repaired(shape, size).sorted.get.length
    case Engine.Mirrored => mirrored(shape, size).sorted.get.length
    case Engine.Frozen   => frozen(shape, size).sorted.length
    case Engine.Stdlib   => stdlib(shape, size).sorted.get.length

    case Engine.JGraphT =>
      val iterator = new org.jgrapht.traverse.TopologicalOrderIterator(jgrapht(shape, size))
      var count = 0

      while iterator.hasNext do
        iterator.next()
        count += 1

      count

    case Engine.Guava =>
      val graph = guava(shape, size)
      val iterator = Traverser.forGraph(graph).nn.depthFirstPostOrder(graph.nodes().nn).nn.iterator().nn
      var count = 0

      while iterator.hasNext do
        iterator.next()
        count += 1

      count

    case _ => 0

  def reachable(engine: Engine, shape: Int, size: Int): Int =
    val node = size/2

    engine match
      case Engine.Current  => current(shape, size).reachable(node).size
      case Engine.Repaired => repaired(shape, size).reachable(node).size
      case Engine.Mirrored => mirrored(shape, size).reachable(node).size
      case Engine.Frozen   => frozen(shape, size).reachable(node).size
      case Engine.Stdlib   => stdlib(shape, size).reachable(node).size
      case Engine.JGraphT  => jgrapht(shape, size).getDescendants(box(node)).nn.size + 1
      case Engine.Guava    => Graphs.reachableNodes(guava(shape, size), box(node)).nn.size
      case _               => 0

  def sources(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).sources.size
    case Engine.Repaired => repaired(shape, size).sources.size
    case Engine.Mirrored => mirrored(shape, size).sources.size
    case Engine.Frozen   => frozen(shape, size).sources.size
    case Engine.Stdlib   => stdlib(shape, size).sources.size

    case Engine.JGraphT =>
      val graph = jgrapht(shape, size)
      graph.vertexSet().nn.iterator().nn.asScala.count(graph.outDegreeOf(_) == 0)

    case Engine.Guava =>
      val graph = guava(shape, size)
      graph.nodes().nn.iterator().nn.asScala.count(graph.outDegree(_) == 0)

    case _ => 0

  def sinks(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).invert.sources.size
    case Engine.Repaired => repaired(shape, size).sinks.size
    case Engine.Mirrored => mirrored(shape, size).sinks.size
    case Engine.Frozen   => frozen(shape, size).sinks.size
    case Engine.Stdlib   => stdlib(shape, size).sinks.size

    case Engine.JGraphT =>
      val graph = jgrapht(shape, size)
      graph.vertexSet().nn.iterator().nn.asScala.count(graph.inDegreeOf(_) == 0)

    case Engine.Guava =>
      val graph = guava(shape, size)
      graph.nodes().nn.iterator().nn.asScala.count(graph.inDegree(_) == 0)

    case _ => 0

  def closure(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).closure.edges.size
    case Engine.Repaired => repaired(shape, size).closure.edgeCount
    case Engine.Mirrored => mirrored(shape, size).closure.edgeCount
    case Engine.Frozen   => frozen(shape, size).closure.valuesIterator.map(_.size).sum
    case Engine.Stdlib   => stdlib(shape, size).closure.valuesIterator.map(_.size).sum

    case Engine.JGraphT =>
      val copy = copyJGraphT(jgrapht(shape, size))
      org.jgrapht.alg.TransitiveClosure.INSTANCE.nn.closeDirectedAcyclicGraph(copy)
      copy.edgeSet().nn.size

    case Engine.Guava =>
      Graphs.transitiveClosure(guava(shape, size)).nn.edges().nn.size

    case _ => 0

  def reduction(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).reduction.edges.size
    case Engine.Repaired => repaired(shape, size).reduction.edgeCount
    case Engine.Mirrored => mirrored(shape, size).reduction.edgeCount
    case Engine.Frozen   => frozen(shape, size).reduction.valuesIterator.map(_.size).sum
    case Engine.Stdlib   => stdlib(shape, size).reduction.valuesIterator.map(_.size).sum

    case Engine.JGraphT =>
      val copy = copyJGraphT(jgrapht(shape, size))
      org.jgrapht.alg.TransitiveReduction.INSTANCE.nn.reduce(copy)
      copy.edgeSet().nn.size

    case _ => 0

  // Every edge reversed, materialised: the rivals' views are copied into fresh graphs so each
  // cell does the same work.
  def invert(engine: Engine, shape: Int, size: Int): Int = engine match
    case Engine.Current  => current(shape, size).invert.keys.size
    case Engine.Repaired => repaired(shape, size).invert.size
    case Engine.Mirrored => mirrored(shape, size).invert.size
    case Engine.Frozen   => frozen(shape, size).invert.size
    case Engine.Stdlib   => stdlib(shape, size).invert.size

    case Engine.JGraphT =>
      val copy = new DefaultDirectedGraph[Integer, DefaultEdge](classOf[DefaultEdge])
      org.jgrapht.Graphs.addGraph(copy, new EdgeReversedGraph(jgrapht(shape, size)))
      copy.vertexSet().nn.size

    case Engine.Guava =>
      Graphs.copyOf(Graphs.transpose(guava(shape, size))).nn.nodes().nn.size

    case _ => 0

  // A hundred edges from the highest nodes to nodes seventeen below them: none can close a
  // cycle, and none is already present. Persistent engines thread the value; mutable ones copy
  // first (the `Copy` cell measures that copy alone).
  private val batch: Int = 100
  private val stride: Int = 17

  def addBatch(engine: Engine, shape: Int, size: Int): Int =
    val top = size - 1

    engine match
      case Engine.Current =>
        var dag = current(shape, size)
        var k = 0
        while k < batch do
          dag = dag.add(top - k, top - k - stride)
          k += 1
        dag.keys.size

      case Engine.Repaired =>
        var dag = repaired(shape, size)
        var k = 0
        while k < batch do
          dag = dag.add(top - k, top - k - stride)
          k += 1
        dag.size

      case Engine.Mirrored =>
        var dag = mirrored(shape, size)
        var k = 0
        while k < batch do
          dag = dag.add(top - k, top - k - stride)
          k += 1
        dag.size

      case Engine.Workspace =>
        val workspace: Workspace[Int]^ = Workspace(repaired(shape, size))
        var k = 0
        while k < batch do
          workspace.add(top - k, top - k - stride)
          k += 1
        workspace.edgeCount

      case Engine.Stdlib =>
        val dag = stdlib(shape, size).copy
        var k = 0
        while k < batch do
          dag.add(top - k, top - k - stride)
          k += 1
        dag.size

      case Engine.JGraphT =>
        val graph = copyJGraphT(jgrapht(shape, size))
        var k = 0
        while k < batch do
          graph.addEdge(box(top - k), box(top - k - stride))
          k += 1
        graph.edgeSet().nn.size

      case Engine.Guava =>
        val graph = Graphs.copyOf(guava(shape, size)).nn
        var k = 0
        while k < batch do
          graph.putEdge(box(top - k), box(top - k - stride))
          k += 1
        graph.edges().nn.size

      case _ => 0

  // Ten interior nodes removed with their dependants rerouted to their dependencies. The rivals
  // offer no such operation, so they have no cell.
  private val removals: Int = 10

  def bypass(engine: Engine, shape: Int, size: Int): Int =
    val first = size/2

    engine match
      case Engine.Current =>
        var dag = current(shape, size)
        var k = 0
        while k < removals do
          dag = dag.remove(first + k)
          k += 1
        dag.keys.size

      case Engine.Repaired =>
        var dag = repaired(shape, size)
        var k = 0
        while k < removals do
          dag = dag.bypass(first + k)
          k += 1
        dag.size

      case Engine.Mirrored =>
        var dag = mirrored(shape, size)
        var k = 0
        while k < removals do
          dag = dag.bypass(first + k)
          k += 1
        dag.size

      case Engine.Workspace =>
        val workspace: Workspace[Int]^ = Workspace(repaired(shape, size))
        var k = 0
        while k < removals do
          workspace.bypass(first + k)
          k += 1
        workspace.size

      case Engine.Stdlib =>
        val dag = stdlib(shape, size).copy
        var k = 0
        while k < removals do
          dag.bypass(first + k)
          k += 1
        dag.size

      case _ => 0

  // ─── agreement ────────────────────────────────────────────────────────────

  private def pairs(graph: org.jgrapht.Graph[Integer, DefaultEdge]): Set[(Int, Int)] =
    graph.edgeSet().nn.iterator().nn.asScala.map: edge =>
      (graph.getEdgeSource(edge).nn.intValue, graph.getEdgeTarget(edge).nn.intValue)
    . toSet

  private def pairs(graph: com.google.common.graph.Graph[Integer]): Set[(Int, Int)] =
    graph.edges().nn.iterator().nn.asScala.map: edge =>
      (edge.source().nn.intValue, edge.target().nn.intValue)
    . filter(_ != _)
    . toSet

  private def edgeSet(adjacency: Map[Int, Set[Int]]): Set[(Int, Int)] =
    adjacency.iterator.flatMap { (from, targets) => targets.iterator.map(from -> _) }.toSet

  // Every dependency before its dependant, over the edges whose ends are both present, and
  // exactly `expected` nodes.
  private def validOrder(data: Edges, order: List[Int], expected: Int): Boolean =
    val position = scm.HashMap[Int, Int]()
    order.iterator.zipWithIndex.foreach { (node, at) => position(node) = at }
    var index = 0
    var valid = order.length == expected

    while valid && index < data.from.length do
      (position.get(data.from(index)), position.get(data.to(index))) match
        case (Some(from), Some(to)) => valid = to < from
        case _                      => ()

      index += 1

    valid

  private def validOrder(data: Edges, order: List[Int]): Boolean = validOrder(data, order, data.count)

  // Which engines disagree with `Repaired` on the sample graph, and whether each detects the
  // cycle that `Dag.hasCycle` misses. Run before anything is timed.
  def agreement(): List[Text] =
    val shape = Shape.BuildSystem.ordinal
    val size = 2000
    val data = edges(shape, size)
    val failures = scm.ListBuffer[Text]()

    def check(name: String)(condition: => Boolean): Unit =
      if !condition then failures += name.tt

    val reference = repaired(shape, size)
    val order = reference.sorted.get
    check("repaired: order")(validOrder(data, order))
    check("current: order")(validOrder(data, current(shape, size).sorted))
    check("mirrored: order")(validOrder(data, mirrored(shape, size).sorted.get))
    check("frozen: order")(validOrder(data, frozen(shape, size).sorted))
    check("stdlib: order")(validOrder(data, stdlib(shape, size).sorted.get))

    val workspace: Workspace[Int]^ = Workspace(data.count, data.from, data.to)
    check("workspace: order")(validOrder(data, workspace.linearized))
    check("workspace reversed: order"):
      val reversed: Workspace[Int]^ = Workspace.reversed(data.count, data.from, data.to)
      validOrder(data, reversed.linearized)

    val jgraphtOrder =
      val iterator = new org.jgrapht.traverse.TopologicalOrderIterator(jgrapht(shape, size))
      val buffer = scm.ListBuffer[Int]()
      while iterator.hasNext do buffer += iterator.next().nn.intValue
      buffer.toList

    check("jgrapht: order")(validOrder(data, jgraphtOrder))

    val guavaOrder =
      val graph = guava(shape, size)
      Traverser.forGraph(graph).nn.depthFirstPostOrder(graph.nodes().nn).nn.iterator().nn.asScala
      . map(_.intValue).toList

    check("guava: order")(validOrder(data, guavaOrder))

    var sample = 0

    while sample < 50 do
      val node = sample*(size/50)
      val expected = reference.reachable(node)
      check("current: reachable " + node)(current(shape, size).reachable(node) == expected)
      check("mirrored: reachable " + node)(mirrored(shape, size).reachable(node) == expected)
      check("frozen: reachable " + node)(frozen(shape, size).reachable(node) == expected)
      check("stdlib: reachable " + node)(stdlib(shape, size).reachable(node) == expected)

      check("jgrapht: reachable " + node):
        jgrapht(shape, size).getDescendants(box(node)).nn.iterator().nn.asScala.map(_.intValue).toSet
        + node == expected

      check("guava: reachable " + node):
        Graphs.reachableNodes(guava(shape, size), box(node)).nn.iterator().nn.asScala.map(_.intValue)
        . toSet == expected

      sample += 1

    val closed = reference.closure.edges
    check("current: closure")(current(shape, size).closure.edges == closed)
    check("mirrored: closure")(mirrored(shape, size).closure.edges == closed)
    check("frozen: closure")(edgeSet(frozen(shape, size).closure) == closed)
    check("stdlib: closure")(edgeSet(stdlib(shape, size).closure) == closed)

    check("jgrapht: closure"):
      val copy = copyJGraphT(jgrapht(shape, size))
      org.jgrapht.alg.TransitiveClosure.INSTANCE.nn.closeDirectedAcyclicGraph(copy)
      pairs(copy) == closed

    check("guava: closure")(pairs(Graphs.transitiveClosure(guava(shape, size)).nn) == closed)

    val reduced = reference.reduction.edges
    check("current: reduction")(current(shape, size).reduction.edges == reduced)
    check("mirrored: reduction")(mirrored(shape, size).reduction.edges == reduced)
    check("frozen: reduction")(edgeSet(frozen(shape, size).reduction) == reduced)
    check("stdlib: reduction")(edgeSet(stdlib(shape, size).reduction) == reduced)

    check("jgrapht: reduction"):
      val copy = copyJGraphT(jgrapht(shape, size))
      org.jgrapht.alg.TransitiveReduction.INSTANCE.nn.reduce(copy)
      pairs(copy) == reduced

    val expectedSources = reference.sources
    val expectedSinks = reference.sinks
    check("current: sources")(current(shape, size).sources == expectedSources)
    check("mirrored: sources")(mirrored(shape, size).sources == expectedSources)
    check("frozen: sources")(frozen(shape, size).sources == expectedSources)
    check("workspace: sources")(workspace.sources == expectedSources)
    check("stdlib: sources")(stdlib(shape, size).sources == expectedSources)
    check("mirrored: sinks")(mirrored(shape, size).sinks == expectedSinks)
    check("frozen: sinks")(frozen(shape, size).sinks == expectedSinks)
    check("workspace: sinks")(workspace.sinks == expectedSinks)
    check("stdlib: sinks")(stdlib(shape, size).sinks == expectedSinks)

    // Rerouting agrees, and the workspace survives it with a valid order.
    val bypassed = reference.bypass(size/2).bypass(size/2 + 1)
    check("mirrored: bypass")(mirrored(shape, size).bypass(size/2).bypass(size/2 + 1).edges == bypassed.edges)

    // Mutated outside the check: a closure sees the exclusive workspace read-only.
    workspace.bypass(size/2)
    workspace.bypass(size/2 + 1)
    val workspaceEdges = workspace.snapshot.edges
    val workspaceOrder = workspace.linearized
    check("workspace: bypass")(workspaceEdges == bypassed.edges && validOrder(data, workspaceOrder, size - 2))

    check("stdlib: bypass"):
      val copy = stdlib(shape, size).copy
      copy.bypass(size/2)
      copy.bypass(size/2 + 1)
      copy.edges == bypassed.edges

    // The cycle `Dag.hasCycle` misses: x -> a, x -> b, b -> a, a -> c, c -> b, with
    // x = 0, a = 1, b = 2, c = 3.
    val cycleFrom = scala.IArray(0, 0, 2, 1, 3)
    val cycleTo = scala.IArray(1, 2, 1, 3, 2)
    check("repaired: cycle")(AdjacencyDag(4, cycleFrom, cycleTo).cycle.isDefined)
    check("mirrored: cycle")(MirroredDag(4, cycleFrom, cycleTo).cycle.isDefined)
    check("stdlib: cycle")(StdlibDag(4, cycleFrom, cycleTo).sorted.isEmpty)

    check("workspace: cycle"):
      val cyclic: Workspace[Int]^ = Workspace()
      cyclic.add(0, 1) && cyclic.add(0, 2) && cyclic.add(2, 1) && cyclic.add(1, 3) && !cyclic.add(3, 2)

    check("jgrapht: cycle"):
      val graph = new DirectedAcyclicGraph[Integer, DefaultEdge](classOf[DefaultEdge])
      var index = 0
      while index < 4 do
        graph.addVertex(box(index))
        index += 1
      try
        index = 0
        while index < cycleFrom.length do
          graph.addEdge(box(cycleFrom(index)), box(cycleTo(index)))
          index += 1
        false
      catch case _: IllegalArgumentException => true

    check("guava: cycle"):
      val graph = GraphBuilder.directed().nn.allowsSelfLoops(false).nn.build[Integer]().nn
      var index = 0
      while index < cycleFrom.length do
        graph.putEdge(box(cycleFrom(index)), box(cycleTo(index)))
        index += 1
      Graphs.hasCycle(graph)

    check("current: cycle")(Dag(AdjacencyDag(4, cycleFrom, cycleTo).adjacency).hasCycle(0))

    failures.toList

  // ─── benchmarks ───────────────────────────────────────────────────────────

  private val sizes = scala.Seq(100, 1000, 10000, 100000)

  // `Dag`'s `sorted` is roughly cubic and its `reach` recurses to the depth of the longest
  // path, so it is measured only where that finishes; the closure's bit matrix and the rivals'
  // closures are quadratic in memory or time; the dense shape has n²/8 edges.
  private val currentLimit = 1000
  private val closureLimit = 10000
  private val denseLimit = 2000

  def run(): Unit =
    import turbulence.stdios.javaLangSystemStdio
    import termcapDefinitions.basicTermcap

    val failures = agreement()

    turbulence.Out.println:
      if failures.isEmpty then t"engines agree on the sample graph"
      else ("engines disagree: " + failures.map(_.s).mkString(", ")).tt

    val bench = Bench()

    def sweep(operation: Operation, shape: Shape, name: Message)(defined: (Engine, Int) -> Boolean)
    :   Unit =

      val operation0 = operation.ordinal
      val shape0 = shape.ordinal

      bench(name)(target = 250*Milli(Second), baseline = Engine.Repaired)
      . over(Axis(Engine), Axis(t"size")(sizes*)):
          case (engine, size) if defined(engine, size) && (shape != Shape.Dense || size <= denseLimit) =>
            val engine0 = engine.ordinal
            '{ acyclicity.Benchmarks.measure($operation0, $engine0, $shape0, $size) }

    def notCurrent(engine: Engine, size: Int): Boolean = engine != Engine.Current
    def currentCapped(engine: Engine, size: Int): Boolean = engine != Engine.Current || size <= currentLimit

    def shapes(operation: Operation, name: Message)(defined: (Engine, Int) -> Boolean): Unit =
      Shape.values.foreach: shape =>
        sweep(operation, shape, m"$name, ${shape.toString.tt}")(defined)

    suite(m"Construction"):
      shapes(Operation.Construct, m"from edge arrays")(notCurrent)

      shapes(Operation.ConstructReversed, m"from edge arrays in reverse order"): (engine, _) =>
        engine == Engine.Repaired || engine == Engine.Workspace || engine == Engine.JGraphT

      sweep(Operation.Build, Shape.BuildSystem, m"workspace alone, unfrozen")((engine, _) => engine == Engine.Workspace)

    suite(m"Whole-graph queries"):
      shapes(Operation.Sorted, m"topological order")(currentCapped)

      shapes(Operation.Closure, m"transitive closure"): (engine, size) =>
        currentCapped(engine, size) && size <= closureLimit

      shapes(Operation.Reduction, m"transitive reduction"): (engine, size) =>
        currentCapped(engine, size) && size <= closureLimit && engine != Engine.Guava

      sweep(Operation.Invert, Shape.BuildSystem, m"inversion, materialised")(currentCapped)

    suite(m"Point queries"):
      sweep(Operation.Reachable, Shape.BuildSystem, m"reachable set of one node")(currentCapped)
      sweep(Operation.Sources, Shape.BuildSystem, m"sources")((_, _) => true)
      sweep(Operation.Sinks, Shape.BuildSystem, m"sinks")(currentCapped)

    suite(m"Editing"):
      sweep(Operation.Copy, Shape.BuildSystem, m"the copy the mutable engines pay"): (engine, _) =>
        engine == Engine.Workspace || engine == Engine.Stdlib || engine == Engine.JGraphT || engine == Engine.Guava

      sweep(Operation.AddBatch, Shape.BuildSystem, m"a hundred added edges"): (engine, size) =>
        currentCapped(engine, size) && engine != Engine.Frozen

      sweep(Operation.Bypass, Shape.BuildSystem, m"ten nodes bypassed"): (engine, size) =>
        currentCapped(engine, size) && engine != Engine.Frozen && engine != Engine.JGraphT && engine != Engine.Guava
