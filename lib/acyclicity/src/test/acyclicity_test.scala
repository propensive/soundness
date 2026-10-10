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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import dysasymptotics.{linearScan, linearSize}

object Tests extends Suite(m"Acyclicity Tests"):
  def run(): Unit =
    // Divisibility poset on the divisors of 12: `a ≤ b` iff `a` divides `b`.
    val divisors = Set(1, 2, 3, 4, 6, 12)
    def divides(a: Int, b: Int): Boolean = b%a == 0

    suite(m"Hasse construction"):
      val hasse = Hasse(divisors)(divides)

      test(m"immediate supertypes are the covering multiples"):
        hasse.parents(2)
      . assert(_ == Set(4, 6))

      test(m"immediate subtypes are the covering divisors"):
        hasse.children(6)
      . assert(_ == Set(2, 3))

      test(m"the top element covers its two predecessors"):
        hasse.children(12)
      . assert(_ == Set(4, 6))

      test(m"a transitive edge is not a cover"):
        hasse.children(12).has(2)
      . assert(_ == false)

      test(m"the maximum is the whole set's top"):
        hasse.maxima
      . assert(_ == Set(12))

      test(m"the minimum is the whole set's bottom"):
        hasse.minima
      . assert(_ == Set(1))

      // The covering relation is a graph: each element points at the elements it covers.
      test(m"a Hasse diagram is a graph whose sources are its minima"):
        hasse.sources
      . assert(_ == Set(1))

      test(m"a Hasse diagram is a graph whose sinks are its maxima"):
        hasse.sinks
      . assert(_ == Set(12))

      test(m"a Hasse diagram linearizes from its bottom to its top"):
        (hasse.linearized.stdlib.head, hasse.linearized.stdlib.last)
      . assert(_ == (1, 12))

      test(m"inverting a Hasse diagram swaps covering and covered"):
        hasse.invert.children(2)
      . assert(_ == Set(4, 6))

      test(m"the Hasse diagram's Dag has the same edges"):
        hasse.dag.edges
      . assert(_ == hasse.edges)

    suite(m"Deferred bottom"):
      val hasse = Hasse(divisors)(divides).bottom(0)

      test(m"the bottom becomes the sole minimum"):
        hasse.minima
      . assert(_ == Set(0))

      test(m"the former minimum now covers the bottom"):
        hasse.children(1)
      . assert(_ == Set(0))

      test(m"the bottom's parents are the former minima"):
        hasse.parents(0)
      . assert(_ == Set(1))

    suite(m"Comparison frugality"):
      test(m"construction compares fewer than all ordered pairs"):
        var count = 0
        Hasse(divisors): (a, b) =>
          count += 1
          divides(a, b)

        count
      . assert(_ < divisors.size*(divisors.size - 1))

    suite(m"Partial orders"):
      given Int is PartiallyOrdered = divides(_, _)

      test(m"a poset's Dag points each element at those immediately above it"):
        Poset(1, 2, 3, 4, 6, 12).dag.successors(2)
      . assert(_ == Set(4, 6))

    // A diamond: `a` depends on `b` and `c`, both of which depend on `d`.
    val diamond = Dag(Set(t"a", t"b", t"c", t"d")):
      case t"a" => Set(t"b", t"c")
      case t"b" => Set(t"d")
      case t"c" => Set(t"d")
      case _    => Set()

    // A graph that cannot be a `Dag`: it has a cycle, so it stays a `Digraph`.
    val cyclic = Digraph(Set(t"x", t"y", t"z")):
      case t"x" => Set(t"y")
      case t"y" => Set(t"z")
      case _    => Set(t"x")

    suite(m"Dag structure"):
      test(m"the nodes are every node given"):
        diamond.nodes
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"a node's successors are its dependencies"):
        diamond.successors(t"a")
      . assert(_ == Set(t"b", t"c"))

      test(m"an absent node has no successors"):
        diamond.successors(t"zz")
      . assert(_ == Set())

      test(m"a present node is reported present"):
        diamond.has(t"b")
      . assert(_ == true)

      test(m"an absent node is reported absent"):
        diamond.has(t"zz")
      . assert(_ == false)

      test(m"the edges are one pair per dependency"):
        diamond.edges
      . assert(_ == Set((t"a", t"b"), (t"a", t"c"), (t"b", t"d"), (t"c", t"d")))

      test(m"the sources are the nodes depending on nothing"):
        diamond.sources
      . assert(_ == Set(t"d"))

      test(m"the sinks are the nodes nothing depends on"):
        diamond.sinks
      . assert(_ == Set(t"a"))

      test(m"two graphs with the same adjacency are equal"):
        Dag(t"a" -> t"b", t"a" -> t"c", t"b" -> t"d", t"c" -> t"d")
      . assert(_ == diamond)

    suite(m"Linearization"):
      test(m"every node is linearized"):
        diamond.linearized.size
      . assert(_ == 4)

      test(m"each node is linearized after everything it depends on"):
        val order = diamond.linearized.stdlib

        diamond.edges.stdlib.forall: (from, to) =>
          order.indexOf(to) < order.indexOf(from)

      . assert(_ == true)

      test(m"the only source comes first"):
        diamond.linearized.stdlib.head
      . assert(_ == t"d")

      test(m"the only sink comes last"):
        diamond.linearized.stdlib.last
      . assert(_ == t"a")

      test(m"the order is the order nodes were given, where that is valid"):
        Dag(1 -> 0, 2 -> 0, 3 -> 0).linearized
      . assert(_ == List(0, 1, 2, 3))

      test(m"a cyclic graph cannot become a Dag"):
        capture[Dag.Error](cyclic.acyclic).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

      test(m"an acyclic Digraph becomes a Dag"):
        diamond.digraph.acyclic
      . assert(_ == diamond)

    suite(m"Cycle detection"):
      test(m"an acyclic graph has no cycle"):
        diamond.digraph.cycle.absent
      . assert(_ == true)

      test(m"a cyclic graph has a cycle"):
        cyclic.cycle.present
      . assert(_ == true)

      test(m"the witness is a closed walk along edges"):
        val witness = cyclic.cycle.or(List()).stdlib

        witness.head == witness.last && witness.sliding(2).forall:
          case scala.List(from, to) => cyclic.successors(from).has(to)
          case _                    => false

      . assert(_ == true)

      // The cycle `Dag.hasCycle` once missed: the search finished `a` before the longer path
      // through `c` closed the cycle through `b`.
      test(m"a cycle closed through an already-finished node is found"):
        Digraph(0 -> 1, 0 -> 2, 2 -> 1, 1 -> 3, 3 -> 2).cycle.present
      . assert(_ == true)

      test(m"such a graph cannot become a Dag"):
        capture[Dag.Error](Dag(0 -> 1, 0 -> 2, 2 -> 1, 1 -> 3, 3 -> 2)).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

    suite(m"Reachability"):
      test(m"a node reaches itself and everything below it"):
        diamond.reachable(t"a")
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"reachability follows only the dependency direction"):
        diamond.reachable(t"b")
      . assert(_ == Set(t"b", t"d"))

      test(m"a source reaches only itself"):
        diamond.reachable(t"d")
      . assert(_ == Set(t"d"))

      test(m"an absent node is not reachable"):
        capture[Dag.Error](diamond.reachable(t"zz")).reason
      . assert(_ == Dag.Error.Reason.NodeMissing(t"zz"))

      // Reachability is well defined on a cyclic graph, so it terminates rather than recursing.
      test(m"reachability on a cyclic graph is total"):
        cyclic.reachable(t"x")
      . assert(_ == Set(t"x", t"y", t"z"))

      test(m"descendants keep the reachable subgraph"):
        diamond.descendants(t"b").nodes
      . assert(_ == Set(t"b", t"d"))

      test(m"ancestors keep the subgraph that reaches the node"):
        diamond.ancestors(t"d").nodes
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"lineage keeps both directions"):
        diamond.lineage(t"b").nodes
      . assert(_ == Set(t"a", t"b", t"d"))

      test(m"a deep chain does not overflow the stack"):
        val chain = Dag(Set.from(0 until 100000))(n => if n == 0 then Set() else Set(n - 1))
        (chain.linearized.size, chain.reachable(99999).size)
      . assert(_ == (100000, 100000))

    suite(m"Inversion"):
      test(m"inverting reverses every edge"):
        diamond.invert.edges
      . assert(_ == Set((t"b", t"a"), (t"c", t"a"), (t"d", t"b"), (t"d", t"c")))

      test(m"a source becomes a node with successors"):
        diamond.invert.successors(t"d")
      . assert(_ == Set(t"b", t"c"))

      test(m"inverting twice restores the edges"):
        diamond.invert.invert
      . assert(_ == diamond)

      test(m"a Map inverts to a Digraph"):
        Map(1 -> Set(2), 2 -> Set[Int]()).invert.edges
      . assert(_ == Set((2, 1)))

      test(m"a Map answers reachability"):
        Map(1 -> Set(2), 2 -> Set[Int]()).reachable(1)
      . assert(_ == Set(1, 2))

    suite(m"Closure and reduction"):
      // A transitive triangle: `a -> c` is implied by `a -> b -> c`.
      val triangle = Dag(t"a" -> t"b", t"a" -> t"c", t"b" -> t"c")

      test(m"the closure of a node is everything below it, excluding itself"):
        triangle.closure.successors(t"a")
      . assert(_ == Set(t"b", t"c"))

      test(m"the closure adds the implied edges of a diamond"):
        diamond.closure.successors(t"a")
      . assert(_ == Set(t"b", t"c", t"d"))

      test(m"the reduction drops the transitive edge"):
        triangle.reduction.edges
      . assert(_ == Set((t"a", t"b"), (t"b", t"c")))

      test(m"the reduction of a diamond changes nothing"):
        diamond.reduction
      . assert(_ == diamond)

      // Implied by a path of three edges, which the old reduction never saw.
      test(m"an edge implied by a longer path is dropped"):
        Dag(1 -> 2, 2 -> 3, 3 -> 4, 1 -> 4).reduction.edges
      . assert(_ == Set((1, 2), (2, 3), (3, 4)))

      test(m"the closure of a cyclic graph is total"):
        cyclic.closure.successors(t"x")
      . assert(_ == Set(t"y", t"z"))

      // Generated graphs: node `i` depends on two or three of the nodes below it.
      def generated(seed: Int): Dag[Int] =
        var state = seed.toLong*6364136223846793005L + 1442695040888963407L

        def random(bound: Int): Int =
          state = state*6364136223846793005L + 1442695040888963407L
          ((state >>> 33)%bound).toInt

        Dag(Set.from(0 until 30)): node =>
          if node == 0 then Set() else Set.from((0 until 2 + random(2)).map(_ => random(node)))

      test(m"reduction then closure gives the closure"):
        (1 to 40).forall { seed => generated(seed).reduction.closure == generated(seed).closure }
      . assert(_ == true)

      test(m"closure then reduction gives the reduction"):
        (1 to 40).forall { seed => generated(seed).closure.reduction == generated(seed).reduction }
      . assert(_ == true)

      test(m"a reduction keeps only existing edges"):
        (1 to 40).forall: seed =>
          val dag = generated(seed)
          dag.reduction.edges.stdlib.subsetOf(dag.edges.stdlib)
      . assert(_ == true)

    suite(m"Frozen topologies"):
      val frozen: Topology[Text]^{} = diamond.freeze

      test(m"a frozen graph linearizes identically"):
        frozen.linearized
      . assert(_ == diamond.linearized)

      test(m"a frozen graph has the same edges"):
        frozen.edges
      . assert(_ == diamond.edges)

      test(m"a frozen graph knows its predecessors"):
        frozen.predecessors(t"d")
      . assert(_ == Set(t"b", t"c"))

      test(m"a frozen graph answers reachability in O(1)"):
        (frozen.reaches(t"a", t"d"), frozen.reaches(t"d", t"a"))
      . assert(_ == (true, false))

      test(m"a frozen graph's sources and sinks agree"):
        (frozen.sources, frozen.sinks)
      . assert(_ == (diamond.sources, diamond.sinks))

      test(m"a frozen graph snapshots to the same Dag"):
        frozen.snapshot
      . assert(_ == diamond)

      test(m"a frozen graph thaws to an editable copy with the same edges"):
        val thawed: Topology[Text]^ = Topology(frozen)
        thawed.edges
      . assert(_ == diamond.edges)

      test(m"a frozen graph inverts in place, reversing its order"):
        frozen.invert.linearized.stdlib
      . assert(_ == diamond.linearized.stdlib.reverse)

      test(m"a frozen inversion snapshots to the inverted Dag"):
        frozen.invert.snapshot
      . assert(_ == diamond.invert)

      test(m"the closure of a long chain fits"):
        val chain = Dag(Set.from(0 until 5000))(n => if n == 0 then Set() else Set(n - 1))
        chain.freeze.closure.successors(4999).size
      . assert(_ == 4999)

    suite(m"Editing"):
      test(m"removing a node drops its incident edges"):
        (diamond - t"b").edges
      . assert(_ == Set((t"a", t"c"), (t"c", t"d")))

      test(m"removing a node leaves a graph that linearizes"):
        (diamond - t"b").linearized.size
      . assert(_ == 3)

      test(m"bypassing a node reroutes its dependants to its dependencies"):
        diamond.bypass(t"b").successors(t"a")
      . assert(_ == Set(t"c", t"d"))

      test(m"bypassing a node drops it"):
        diamond.bypass(t"b").nodes
      . assert(_ == Set(t"a", t"c", t"d"))

      // Two adjacent nodes dropped: the path through both must survive.
      test(m"bypassing adjacent nodes preserves the path through them"):
        Dag(1 -> 2, 2 -> 3, 3 -> 4).bypassAll(Set(2, 3)).edges
      . assert(_ == Set((1, 4)))

      test(m"removing a single edge leaves the node in place"):
        diamond.remove(t"a", t"b").successors(t"a")
      . assert(_ == Set(t"c"))

      test(m"adding an edge extends the dependencies"):
        diamond.add(t"d", t"e").successors(t"d")
      . assert(_ == Set(t"e"))

      test(m"adding an edge makes its target a node"):
        diamond.add(t"d", t"e").linearized.stdlib.head
      . assert(_ == t"e")

      test(m"adding an edge that would close a cycle is refused"):
        capture[Dag.Error](diamond.add(t"d", t"a")).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

      test(m"an edge that may close a cycle gives a Digraph"):
        (diamond + (t"d" -> t"a")).cycle.present
      . assert(_ == true)

      test(m"a subgraph keeps only the nodes asked for"):
        diamond.subgraph(Set(t"a", t"b")).edges
      . assert(_ == Set((t"a", t"b")))

      test(m"mapping renames both ends of every edge"):
        diamond.map(_.upper).edges
      . assert(_ == Set((t"A", t"B"), (t"A", t"C"), (t"B", t"D"), (t"C", t"D")))

      test(m"a non-injective map unites the edges of merged nodes"):
        Dag(1 -> 3, 2 -> 4).map(n => if n < 3 then t"lo" else t"hi").edges
      . assert(_ == Set((t"lo", t"hi")))

      test(m"joining two graphs unions their edges"):
        (diamond ++ Dag(t"a" -> t"d")).successors(t"a")
      . assert(_ == Set(t"b", t"c", t"d"))

    suite(m"Traversal"):
      test(m"a traversal sees each node's dependencies already computed"):
        val depth = diamond.traversal[Int]: (below, node) =>
          if below.nil then 0 else below.stdlib.max + 1

        (depth(t"d"), depth(t"b"), depth(t"a"))
      . assert(_ == (0, 1, 2))

    suite(m"Edge targets"):
      test(m"an edge target is made a node"):
        Dag(t"a" -> t"b").nodes
      . assert(_ == Set(t"a", t"b"))

      test(m"a target with no edges of its own is a source"):
        Dag(8 -> 4, 4 -> 2).sources
      . assert(_ == Set(2))

      test(m"a graph built from edges alone linearizes"):
        Dag(8 -> 4, 4 -> 2).linearized
      . assert(_ == List(2, 4, 8))

      test(m"a successor outside the node set is made a node"):
        Dag(Set(t"a"))(_ => Set(t"b")).nodes
      . assert(_ == Set(t"a", t"b"))

      test(m"a Map is a graph, and its targets become nodes"):
        Map(1 -> Set(2)).digraph.nodes
      . assert(_ == Set(1, 2))

      test(m"a Map that is acyclic becomes a Dag"):
        Map(1 -> Set(2), 2 -> Set[Int]()).acyclic.linearized
      . assert(_ == List(2, 1))

    suite(m"Exploration"):
      test(m"exploring materialises the graph beneath a node"):
        12.explore(n => (1 until n).filter(n%_ == 0)).acyclic.linearized.stdlib.last
      . assert(_ == 12)

    // The mutable form is driven here, in the suite body, and its results captured as values:
    // a closure sees the exclusive handle read-only, so no `test` block may edit it.
    val topology: Topology[Int]^ = Topology()
    topology.add(1, 2)
    topology.add(1, 3)
    topology.add(2, 4)
    topology.add(3, 4)
    val topologyOrder = topology.linearized
    val topologySources = topology.sources
    val topologySinks = topology.sinks
    val topologyEdges = topology.snapshot.edges

    // Adding the reverse of a path is refused and leaves the graph unchanged.
    val refused = try { topology.add(4, 1); false } catch case error: Dag.Error => true
    val unchangedEdges = topology.snapshot.edges

    // An edge arriving against the current order is accepted, and the order repaired.
    topology.add(5, 1)
    val repairedOrder = topology.linearized

    topology.bypass(2)
    val bypassedEdges = topology.snapshot.edges
    val bypassedOrder = topology.linearized

    val frozenFromTopology: Topology[Int]^{} = Topology.freeze(topology)

    suite(m"Topology"):
      test(m"a topology keeps a valid order as edges arrive"):
        val order = topologyOrder.stdlib
        topologyEdges.stdlib.forall { (from, to) => order.indexOf(to) < order.indexOf(from) }
      . assert(_ == true)

      test(m"a topology's sources and sinks are maintained"):
        (topologySources, topologySinks)
      . assert(_ == (Set(4), Set(1)))

      test(m"an edge that would close a cycle is refused"):
        refused
      . assert(_ == true)

      test(m"a refused edge leaves the graph unchanged"):
        unchangedEdges
      . assert(_ == topologyEdges)

      test(m"an edge against the order has the order repaired"):
        val order = repairedOrder.stdlib
        order.indexOf(1) < order.indexOf(5)
      . assert(_ == true)

      test(m"bypassing reroutes and keeps the order valid"):
        val order = bypassedOrder.stdlib
        bypassedEdges.has((1, 4)) && !order.contains(2)
        && bypassedEdges.stdlib.forall { (from, to) => order.indexOf(to) < order.indexOf(from) }
      . assert(_ == true)

      test(m"a consumed topology freezes to the same edges"):
        frozenFromTopology.edges
      . assert(_ == bypassedEdges)

    suite(m"Dot serialization"):
      test(m"a single directed edge serializes to a digraph"):
        Dag(t"a" -> t"b").dot.serialize
      . assert(_ == t"\ndigraph {\n  \"a\" -> \"b\"\n}")

      test(m"an undirected edge uses the undirected operator"):
        unsafely(Dot.graph(Name[Dot.Id](t"g"), Name[Dot.Id](t"a") -- Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\ngraph g {\n  \"a\" -- \"b\"\n}")

      test(m"a strict graph is marked strict"):
        unsafely(Dot.strictDigraph(Name[Dot.Id](t"g"), Name[Dot.Id](t"a") --> Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\nstrict digraph g {\n  \"a\" -> \"b\"\n}")

      test(m"node attributes are emitted in brackets"):
        unsafely(Dot.digraph(Name[Dot.Id](t"a")(t"color" -> t"red"))).serialize
      . assert(_ == t"\ndigraph {\n  \"a\" [ color=\"red\" ]\n}")

      test(m"an assignment serializes as a quoted pair"):
        unsafely(Dot.digraph(Name[Dot.Id](t"a") := Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\ndigraph {\n  \"a\" = \"b\"\n}")

      test(m"adding a statement extends the graph"):
        val graph = unsafely:
          Dot.digraph(Name[Dot.Id](t"a") --> Name[Dot.Id](t"b"))
          . add(Name[Dot.Id](t"b") --> Name[Dot.Id](t"c"))

        graph.serialize
      . assert(_ == t"\ndigraph {\n  \"a\" -> \"b\"\n  \"b\" -> \"c\"\n}")

      test(m"a subgraph nests its statements"):
        unsafely(Dot.digraph(Dot.subgraph(Name[Dot.Id](t"a") --> Name[Dot.Id](t"b")))).serialize
      . assert(_ == t"\ndigraph {\n  subgraph {\n    \"a\" -> \"b\"\n  }\n}")

      test(m"an identifier containing a quote is not a valid DOT id"):
        capture[Name.Error](Name[Dot.Id](t"a\"b")).message.show
      . assert(_ == t"the name a\"b is not valid because it must be a valid DOT identifier")

      test(m"an empty identifier is not a valid DOT id"):
        demilitarize:
          val id: Name[Dot.Id] = n""
      . assert(_.nonEmpty)


    suite(m"Layering"):
      test(m"a chain has one node per layer"):
        val dag = Dag(t"a" -> Set(), t"b" -> Set(t"a"), t"c" -> Set(t"b"))
        dag.layered.layers.map(_.map { case Layering.Vertex.Real(n) => n; case other => t"?" })
      . assert(_ == List(List(t"a"), List(t"b"), List(t"c")))

      test(m"the diamond's middle layer holds both siblings"):
        diamond.layered.layers.map(_.length)
      . assert(_ == List(1, 2, 1))

      test(m"ranks are the longest path from a source"):
        diamond.layered.rank
      . assert(_ == Map(t"d" -> 0, t"b" -> 1, t"c" -> 1, t"a" -> 2))

      // In first-reach order `c` precedes `d`, which crosses `a → d` over `b → c`; swapping
      // them is planar.
      test(m"a crossing in the initial order is removed"):
        val dag = Dag(t"a" -> Set(), t"b" -> Set(), t"c" -> Set(t"a", t"b"), t"d" -> Set(t"a"))
        val layering = dag.layered
        (layering.crossings, layering.position(t"d") < layering.position(t"c"))
      . assert(_ == (0, true))

      test(m"an edge spanning two layers passes through a virtual vertex"):
        val dag = Dag(t"a" -> Set(), t"b" -> Set(t"a"), t"c" -> Set(t"a", t"b"))
        dag.layered.layers(1).contains(Layering.Vertex.Virtual(t"a", t"c"))
      . assert(_ == true)

      test(m"links join every adjacent pair of layers"):
        val dag = Dag(t"a" -> Set(), t"b" -> Set(t"a"), t"c" -> Set(t"a", t"b"))
        dag.layered.links.map(_.length)
      . assert(_ == List(2, 2))

      // `b` feeds only `d`, two layers down: `Balanced` sinks it to the layer above `d`.
      test(m"balanced ranking pulls a source toward its dependent"):
        val dag =
          Dag(t"a" -> Set(), t"b" -> Set(), t"c" -> Set(t"a"), t"d" -> Set(t"b", t"c"))

        import rankings.balancedRanking
        dag.layered.rank(t"b")
      . assert(_ == 1)

      test(m"longest-path ranking keeps every source in the first layer"):
        val dag =
          Dag(t"a" -> Set(), t"b" -> Set(), t"c" -> Set(t"a"), t"d" -> Set(t"b", t"c"))

        dag.layered.rank(t"b")
      . assert(_ == 0)

      test(m"an empty graph has no layers"):
        Dag[Text]().layered.layers
      . assert(_ == Nil)
