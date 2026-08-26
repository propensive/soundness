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
        hasse.children(12).contains(2)
      . assert(_ == false)

      test(m"the maximum is the whole set's top"):
        hasse.maxima
      . assert(_ == Set(12))

      test(m"the minimum is the whole set's bottom"):
        hasse.minima
      . assert(_ == Set(1))

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

    // A diamond: `a` depends on `b` and `c`, both of which depend on `d`. Built through the
    // `keys`-and-dependencies factory so that every node is a key; the varargs edge factory
    // records only the source of each edge (see "Dangling targets" below).
    val diamond = Dag(Set(t"a", t"b", t"c", t"d")):
      case t"a" => Set(t"b", t"c")
      case t"b" => Set(t"d")
      case t"c" => Set(t"d")
      case _    => Set()

    val cyclic = Dag(Set(t"x", t"y", t"z")):
      case t"x" => Set(t"y")
      case t"y" => Set(t"z")
      case _    => Set(t"x")

    suite(m"Dag structure"):
      test(m"the keys are every node given"):
        diamond.keys
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"applying a node gives its dependencies"):
        diamond(t"a")
      . assert(_ == Set(t"b", t"c"))

      test(m"applying an absent node gives no dependencies"):
        diamond(t"zz")
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

    suite(m"Topological sorting"):
      test(m"every node is sorted"):
        diamond.sorted.size
      . assert(_ == 4)

      test(m"each node is sorted after everything it depends on"):
        val order = diamond.sorted

        diamond.edges.forall: (from, to) =>
          order.indexOf(to) < order.indexOf(from)

      . assert(_ == true)

      test(m"the only source sorts first"):
        diamond.sorted.head
      . assert(_ == t"d")

      test(m"the only sink sorts last"):
        diamond.sorted.last
      . assert(_ == t"a")

      test(m"a cyclic graph cannot be sorted"):
        capture[Dag.Error](cyclic.sorted).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

    suite(m"Cycle detection"):
      test(m"an acyclic graph has no cycle"):
        diamond.hasCycle(t"a")
      . assert(_ == false)

      test(m"a cyclic graph has a cycle"):
        cyclic.hasCycle(t"x")
      . assert(_ == true)

      test(m"a cycle is found from any node on it"):
        cyclic.hasCycle(t"z")
      . assert(_ == true)

      test(m"an absent node cannot be searched"):
        capture[Dag.Error](diamond.hasCycle(t"zz")).reason
      . assert(_ == Dag.Error.Reason.NodeMissing(t"zz"))

    suite(m"Reachability"):
      test(m"a node reaches itself and everything below it"):
        diamond.reachable(t"a")
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"reachability follows only the dependency direction"):
        diamond.reachable(t"b")
      . assert(_ == Set(t"b", t"d"))

      test(m"a sink reaches only itself"):
        diamond.reachable(t"d")
      . assert(_ == Set(t"d"))

      test(m"an absent node is not reachable"):
        capture[Dag.Error](diamond.reachable(t"zz")).reason
      . assert(_ == Dag.Error.Reason.NodeMissing(t"zz"))

      test(m"descendants keep the reachable subgraph"):
        diamond.descendants(t"b").keys
      . assert(_ == Set(t"b", t"d"))

      test(m"ancestors keep the subgraph that reaches the node"):
        diamond.ancestors(t"d").keys
      . assert(_ == Set(t"a", t"b", t"c", t"d"))

      test(m"lineage keeps both directions"):
        diamond.lineage(t"b").keys
      . assert(_ == Set(t"a", t"b", t"d"))

    suite(m"Inversion"):
      test(m"inverting reverses every edge"):
        diamond.invert.edges
      . assert(_ == Set((t"b", t"a"), (t"c", t"a"), (t"d", t"b"), (t"d", t"c")))

      test(m"a sink becomes a node with dependants"):
        diamond.invert(t"d")
      . assert(_ == Set(t"b", t"c"))

      test(m"inverting twice restores the edges"):
        diamond.invert.invert.edges
      . assert(_ == diamond.edges)

    suite(m"Closure and reduction"):
      // A transitive triangle: `a -> c` is implied by `a -> b -> c`, so a transitive reduction
      // must drop it, and a transitive closure must keep it.
      val triangle = Dag(Set(t"a", t"b", t"c")):
        case t"a" => Set(t"b", t"c")
        case t"b" => Set(t"c")
        case _    => Set()

      test(m"the closure of a node is everything below it, excluding itself"):
        triangle.closure(t"a")
      . assert(_ == Set(t"b", t"c"))

      test(m"the closure adds the implied edges of a diamond"):
        diamond.closure(t"a")
      . assert(_ == Set(t"b", t"c", t"d"))

      test(m"the reduction drops the transitive edge"):
        triangle.reduction.edges
      . assert(_ == Set((t"a", t"b"), (t"b", t"c")))

      test(m"the reduction of a diamond changes nothing"):
        diamond.reduction.edges
      . assert(_ == diamond.edges)

    suite(m"Editing"):
      test(m"removing a key drops it without rerouting"):
        (diamond - t"b").keys
      . assert(_ == Set(t"a", t"c", t"d"))

      test(m"removing an element reroutes its dependants to its dependencies"):
        diamond.remove(t"b")(t"a")
      . assert(_ == Set(t"c", t"d"))

      test(m"removing an element drops it from the keys"):
        diamond.remove(t"b").keys
      . assert(_ == Set(t"a", t"c", t"d"))

      test(m"removing a single edge leaves the node in place"):
        diamond.remove(t"a", t"b")(t"a")
      . assert(_ == Set(t"c"))

      test(m"adding an edge extends the dependencies"):
        diamond.add(t"d", t"a")(t"d")
      . assert(_ == Set(t"a"))

      test(m"a subgraph keeps only the nodes asked for"):
        diamond.subgraph(Set(t"a", t"b")).keys
      . assert(_ == Set(t"a", t"b"))

      test(m"filtering reroutes through the nodes it drops"):
        diamond.filter(_ != t"b")(t"a")
      . assert(_ == Set(t"c", t"d"))

      test(m"mapping renames both keys and dependencies"):
        diamond.map(_.upper).edges
      . assert(_ == Set((t"A", t"B"), (t"A", t"C"), (t"B", t"D"), (t"C", t"D")))

      test(m"joining two graphs unions their dependencies"):
        (diamond ++ Dag(Set(t"a"))(_ => Set(t"d")))(t"a")
      . assert(_ == Set(t"b", t"c", t"d"))

    suite(m"Traversal"):
      test(m"a traversal sees each node's dependencies already computed"):
        val depth = diamond.traversal[Int]: (below, node) =>
          if below.isEmpty then 0 else below.max + 1

        (depth(t"d"), depth(t"b"), depth(t"a"))
      . assert(_ == (0, 1, 2))

    suite(m"Dangling targets"):
      // The varargs edge factory records only the source of each edge, so an edge target that
      // is never itself a source is not a key. `sorted` then finds no node whose dependencies
      // are all satisfied and reports a cycle, rather than a missing node.
      test(m"an edge target is not made a key"):
        Dag(t"a" -> t"b").keys
      . assert(_ == Set(t"a"))

      test(m"a dangling target is reported as a cycle, not as a missing node"):
        capture[Dag.Error](Dag(t"a" -> t"b").sorted).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

    suite(m"Dot serialization"):
      test(m"a single directed edge serializes to a digraph"):
        Dag(t"a" -> t"b").dot.serialize
      . assert(_ == t"\ndigraph {\n  \"a\" -> \"b\"\n}")

      test(m"an undirected edge uses the undirected operator"):
        unsafely(Dot.Graph(None, false, Name[Dot.Id](t"a") -- Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\ngraph {\n  \"a\" -- \"b\"\n}")

      test(m"a strict graph is marked strict"):
        unsafely(Dot.Digraph(None, true, Name[Dot.Id](t"a") --> Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\nstrict digraph {\n  \"a\" -> \"b\"\n}")

      test(m"node attributes are emitted in brackets"):
        unsafely(Dot.Digraph(None, false, Name[Dot.Id](t"a")(t"color" -> t"red"))).serialize
      . assert(_ == t"\ndigraph {\n  \"a\" [ color=\"red\" ]\n}")

      test(m"an assignment serializes as a quoted pair"):
        unsafely(Dot.Digraph(None, false, Name[Dot.Id](t"a") := Name[Dot.Id](t"b"))).serialize
      . assert(_ == t"\ndigraph {\n  \"a\" = \"b\"\n}")

      test(m"adding a statement extends the graph"):
        val graph = unsafely:
          Dot.Digraph(None, false, Name[Dot.Id](t"a") --> Name[Dot.Id](t"b"))
          . add(Name[Dot.Id](t"b") --> Name[Dot.Id](t"c"))

        graph.serialize
      . assert(_ == t"\ndigraph {\n  \"a\" -> \"b\"\n  \"b\" -> \"c\"\n}")

      test(m"an identifier containing a quote is not a valid DOT id"):
        capture[Name.Error](Name[Dot.Id](t"a\"b")).message.show
      . assert(_ == t"the name a\"b is not valid because it must be a valid DOT identifier")

      test(m"an empty identifier is not a valid DOT id"):
        demilitarize:
          val id: Name[Dot.Id] = n""
      . assert(_.nonEmpty)

