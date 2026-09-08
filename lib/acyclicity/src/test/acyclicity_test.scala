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
      case "a" => Set("b", "c")
      case "b" => Set("d")
      case "c" => Set("d")
      case _    => Set()

    val cyclic = Dag(Set(t"x", t"y", t"z")):
      case "x" => Set("y")
      case "y" => Set("z")
      case _    => Set("x")

    suite(m"Dag structure"):
      test(m"the keys are every node given"):
        diamond.keys
      . assert(_ == Set("a", "b", "c", "d"))

      test(m"applying a node gives its dependencies"):
        diamond("a")
      . assert(_ == Set("b", "c"))

      test(m"applying an absent node gives no dependencies"):
        diamond("zz")
      . assert(_ == Set())

      test(m"a present node is reported present"):
        diamond.has("b")
      . assert(_ == true)

      test(m"an absent node is reported absent"):
        diamond.has("zz")
      . assert(_ == false)

      test(m"the edges are one pair per dependency"):
        diamond.edges
      . assert(_ == Set(("a", "b"), ("a", "c"), ("b", "d"), ("c", "d")))

      test(m"the sources are the nodes depending on nothing"):
        diamond.sources
      . assert(_ == Set("d"))

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
      . assert(_ == "d")

      test(m"the only sink sorts last"):
        diamond.sorted.last
      . assert(_ == "a")

      test(m"a cyclic graph cannot be sorted"):
        capture[Dag.Error](cyclic.sorted).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

    suite(m"Cycle detection"):
      test(m"an acyclic graph has no cycle"):
        diamond.hasCycle("a")
      . assert(_ == false)

      test(m"a cyclic graph has a cycle"):
        cyclic.hasCycle("x")
      . assert(_ == true)

      test(m"a cycle is found from any node on it"):
        cyclic.hasCycle("z")
      . assert(_ == true)

      test(m"an absent node cannot be searched"):
        capture[Dag.Error](diamond.hasCycle("zz")).reason
      . assert(_ == Dag.Error.Reason.NodeMissing("zz"))

    suite(m"Reachability"):
      test(m"a node reaches itself and everything below it"):
        diamond.reachable("a")
      . assert(_ == Set("a", "b", "c", "d"))

      test(m"reachability follows only the dependency direction"):
        diamond.reachable("b")
      . assert(_ == Set("b", "d"))

      test(m"a sink reaches only itself"):
        diamond.reachable("d")
      . assert(_ == Set("d"))

      test(m"an absent node is not reachable"):
        capture[Dag.Error](diamond.reachable("zz")).reason
      . assert(_ == Dag.Error.Reason.NodeMissing("zz"))

      test(m"descendants keep the reachable subgraph"):
        diamond.descendants("b").keys
      . assert(_ == Set("b", "d"))

      test(m"ancestors keep the subgraph that reaches the node"):
        diamond.ancestors("d").keys
      . assert(_ == Set("a", "b", "c", "d"))

      test(m"lineage keeps both directions"):
        diamond.lineage("b").keys
      . assert(_ == Set("a", "b", "d"))

    suite(m"Inversion"):
      test(m"inverting reverses every edge"):
        diamond.invert.edges
      . assert(_ == Set(("b", "a"), ("c", "a"), ("d", "b"), ("d", "c")))

      test(m"a sink becomes a node with dependants"):
        diamond.invert("d")
      . assert(_ == Set("b", "c"))

      test(m"inverting twice restores the edges"):
        diamond.invert.invert.edges
      . assert(_ == diamond.edges)

    suite(m"Closure and reduction"):
      // A transitive triangle: `a -> c` is implied by `a -> b -> c`, so a transitive reduction
      // must drop it, and a transitive closure must keep it.
      val triangle = Dag(Set(t"a", t"b", t"c")):
        case "a" => Set("b", "c")
        case "b" => Set("c")
        case _    => Set()

      test(m"the closure of a node is everything below it, excluding itself"):
        triangle.closure("a")
      . assert(_ == Set("b", "c"))

      test(m"the closure adds the implied edges of a diamond"):
        diamond.closure("a")
      . assert(_ == Set("b", "c", "d"))

      test(m"the reduction drops the transitive edge"):
        triangle.reduction.edges
      . assert(_ == Set(("a", "b"), ("b", "c")))

      test(m"the reduction of a diamond changes nothing"):
        diamond.reduction.edges
      . assert(_ == diamond.edges)

    suite(m"Editing"):
      test(m"removing a key drops it without rerouting"):
        (diamond - "b").keys
      . assert(_ == Set("a", "c", "d"))

      test(m"removing an element reroutes its dependants to its dependencies"):
        diamond.remove("b")("a")
      . assert(_ == Set("c", "d"))

      test(m"removing an element drops it from the keys"):
        diamond.remove("b").keys
      . assert(_ == Set("a", "c", "d"))

      test(m"removing a single edge leaves the node in place"):
        diamond.remove("a", "b")("a")
      . assert(_ == Set("c"))

      test(m"adding an edge extends the dependencies"):
        diamond.add("d", "a")("d")
      . assert(_ == Set("a"))

      test(m"a subgraph keeps only the nodes asked for"):
        diamond.subgraph(Set("a", "b")).keys
      . assert(_ == Set("a", "b"))

      test(m"filtering reroutes through the nodes it drops"):
        diamond.filter(_ != "b")("a")
      . assert(_ == Set("c", "d"))

      test(m"mapping renames both keys and dependencies"):
        diamond.map(_.upper).edges
      . assert(_ == Set(("A", "B"), ("A", "C"), ("B", "D"), ("C", "D")))

      test(m"joining two graphs unions their dependencies"):
        (diamond ++ Dag(Set(t"a"))(_ => Set(t"d")))(t"a")
      . assert(_ == Set("b", "c", "d"))

    suite(m"Traversal"):
      test(m"a traversal sees each node's dependencies already computed"):
        val depth = diamond.traversal[Int]: (below, node) =>
          if below.isEmpty then 0 else below.max + 1

        (depth("d"), depth("b"), depth("a"))
      . assert(_ == (0, 1, 2))

    suite(m"Dangling targets"):
      // The varargs edge factory records only the source of each edge, so an edge target that
      // is never itself a source is not a key. `sorted` then finds no node whose dependencies
      // are all satisfied and reports a cycle, rather than a missing node.
      test(m"an edge target is not made a key"):
        Dag(t"a" -> t"b").keys
      . assert(_ == Set("a"))

      test(m"a dangling target is reported as a cycle, not as a missing node"):
        capture[Dag.Error](Dag(t"a" -> t"b").sorted).reason
      . assert(_ == Dag.Error.Reason.Cyclic)

    suite(m"Dot serialization"):
      test(m"a single directed edge serializes to a digraph"):
        Dag(t"a" -> t"b").dot.serialize
      . assert(_ == "\ndigraph {\n  \"a\" -> \"b\"\n}")

      test(m"an undirected edge uses the undirected operator"):
        unsafely(Dot.Graph(None, false, Name[Dot.Id]("a") -- Name[Dot.Id]("b"))).serialize
      . assert(_ == "\ngraph {\n  \"a\" -- \"b\"\n}")

      test(m"a strict graph is marked strict"):
        unsafely(Dot.Digraph(None, true, Name[Dot.Id]("a") --> Name[Dot.Id]("b"))).serialize
      . assert(_ == "\nstrict digraph {\n  \"a\" -> \"b\"\n}")

      test(m"node attributes are emitted in brackets"):
        unsafely(Dot.Digraph(None, false, Name[Dot.Id]("a")("color" -> "red"))).serialize
      . assert(_ == "\ndigraph {\n  \"a\" [ color=\"red\" ]\n}")

      test(m"an assignment serializes as a quoted pair"):
        unsafely(Dot.Digraph(None, false, Name[Dot.Id]("a") := Name[Dot.Id]("b"))).serialize
      . assert(_ == "\ndigraph {\n  \"a\" = \"b\"\n}")

      test(m"adding a statement extends the graph"):
        val graph = unsafely:
          Dot.Digraph(None, false, Name[Dot.Id]("a") --> Name[Dot.Id]("b"))
          . add(Name[Dot.Id]("b") --> Name[Dot.Id]("c"))

        graph.serialize
      . assert(_ == "\ndigraph {\n  \"a\" -> \"b\"\n  \"b\" -> \"c\"\n}")

      test(m"an identifier containing a quote is not a valid DOT id"):
        capture[Name.Error](Name[Dot.Id]("a\"b")).message.show
      . assert(_ == "the name a\"b is not valid because it must be a valid DOT identifier")

      test(m"an empty identifier is not a valid DOT id"):
        demilitarize:
          val id: Name[Dot.Id] = n""
      . assert(_.nonEmpty)

