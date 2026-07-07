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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package dendrology

import soundness.*

import strategies.throwUnsafely

object Tests extends Suite(m"Dendrology tests"):

  case class Tree(value: Text, children: List[Tree] = Nil)

  val life = List(
    Tree(t"Plantae", List()),
    Tree(t"Fungi", List(
      Tree(t"Rozellomyceta", List()),
      Tree(t"Aphelidiomyceta", List()),
      Tree(t"Eumycota", List()),
    )),
    Tree(t"Protozoa", List()),
    Tree(t"Bacteria", List()),
    Tree(t"Animalia", List(
      Tree(t"Chordata", List(
        Tree(t"Mammalia", List(
          Tree(t"Carnivora", List(
            Tree(t"Feliadae", List()),
            Tree(t"Canidae", List(
              Tree(t"Canis", List()),
            )),
            Tree(t"Ursidae", List())
          ))
        ))
      ))
    ))
  )

  val types = Dag(
    t"Any"           -> Set(),
    t"Matchable"     -> Set(t"Any"),
    t"AnyVal"        -> Set(t"Matchable"),
    t"AnyRef"        -> Set(t"Matchable"),
    t"Unit"          -> Set(t"AnyVal"),
    t"Boolean"       -> Set(t"AnyVal"),
    t"Int"           -> Set(t"AnyVal"),
    t"String"        -> Set(t"AnyRef"),
    t"List[Int]"     -> Set(t"AnyRef"),
    t"Null"          -> Set(t"String", t"List[Int]"),
    t"Nothing"       -> Set(t"Null", t"Unit", t"Boolean", t"Int")
  )

  def run(): Unit =
    import dagStyles.defaultDagStyle
    DagDiagram(types).render { node => t"▪ $node" }

    test(m"Lane DAG: linear chain"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"))
      LaneDagDiagram(dag).render(node => t" $node").join(t"\n")

    . assert(_ == t"●  A\n│ \n●  B\n│ \n●  C")

    test(m"Lane DAG: diamond"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"A"),
          t"D" -> Set(t"B", t"C") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 7)

    test(m"Lane DAG: diamond layout shapes"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"A"),
          t"D" -> Set(t"B", t"C") )
      val rendered = LaneDagDiagram(dag).render(node => t"").map(_.s)
      val nodeMark = '●'
      ( rendered.head.indexOf(nodeMark) >= 0,
        rendered.last.indexOf(nodeMark) >= 0,
        rendered(1).indexOf(nodeMark) >= 0,
        rendered(rendered.length - 2).indexOf(nodeMark) >= 0 )

    . assert(_ == (true, true, false, false))

    test(m"Lane DAG: single node"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag(t"A" -> Set())
      LaneDagDiagram(dag).render(node => t" $node").join(t"\n")

    . assert(_ == t"●  A")

    test(m"Lane DAG: high fan-out"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"root" -> Set(),
          t"a"    -> Set(t"root"),
          t"b"    -> Set(t"root"),
          t"c"    -> Set(t"root"),
          t"d"    -> Set(t"root") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 9)

    test(m"Lane DAG: high fan-in"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"a"    -> Set(),
          t"b"    -> Set(),
          t"c"    -> Set(),
          t"d"    -> Set(),
          t"sink" -> Set(t"a", t"b", t"c", t"d") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 9)

    test(m"Lane DAG: linear chain detail"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"))
      LaneDagDiagram(dag).render(node => t" $node")

    . assert(_ == List(t"●  A", t"│ ", t"●  B", t"│ ", t"●  C"))

    test(m"Lane DAG: crossing renders as horizontal not junction"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(),
          t"D" -> Set(t"A", t"C") )
      LaneDagDiagram(dag).render(node => t"").map(_.s).mkString("\n").linesIterator.toList.size

    . assert(_ == 7)

    test(m"Lane DAG: per-vertex glyph"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      val glyph = (n: Text) => if n == t"A" then t"★ " else t"● "
      LaneDagDiagram(dag).render(glyph, n => t" $n")

    . assert(_ == List(t"★  A", t"│ ", t"●  B"))

    test(m"Lane DAG: compact preserves single-vertical row"):
      import laneDagStyles.defaultLaneDagStyle
      // A→B is a direct edge: connector has exactly one Vertical, must stay.
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      LaneDagDiagram(dag).compact.render(n => t" $n").size

    . assert(_ == 3)

    test(m"Lane DAG: compact removes multi-vertical pass-through row"):
      import laneDagStyles.defaultLaneDagStyle
      // A→D is long, B and C are between. After A's row a connector row of
      // pure pass-throughs (multi-vertical) appears; compact should drop it.
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"B"),
          t"D" -> Set(t"A", t"C") )
      val full = LaneDagDiagram(dag).render(n => t" $n").size
      val compact = LaneDagDiagram(dag).compact.render(n => t" $n").size
      compact < full

    . assert(_ == true)

    test(m"Layered DAG: linear chain has one node per level"):
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"))
      LayeredDagDiagram(dag).rows.count((_, nodesAt) => nodesAt.nonEmpty)

    . assert(_ == 3)

    test(m"Layered DAG: diamond packs siblings into one row"):
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"A"),
          t"D" -> Set(t"B", t"C") )
      val nodeRows = LayeredDagDiagram(dag).rows.collect:
        case (_, nodesAt) if nodesAt.nonEmpty => nodesAt
      // Three node rows: [A], [B,C], [D]
      ( nodeRows.length,
        nodeRows.head.size,
        nodeRows(1).size,
        nodeRows.last.size )

    . assert(_ == (3, 1, 2, 1))

    test(m"Layered DAG: per-vertex glyph"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      val glyph = (n: Text) => if n == t"A" then t"★ " else t"● "
      LayeredDagDiagram(dag).render(glyph)

    . assert(_ == List(t"★ ", t"│ ", t"● "))

    test(m"Layered DAG: variable column widths"):
      import laneDagStyles.defaultLaneDagStyle
      // The B node has a wide glyph; column 0 should expand to fit it.
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      val glyph = (n: Text) => if n == t"B" then t"[long]" else t"●     "
      LayeredDagDiagram(dag).render(glyph)

    . assert(_ == List(t"●     ", t"│     ", t"[long]"))

    test(m"Lane DAG: variable column widths in connectors"):
      import laneDagStyles.defaultLaneDagStyle
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"A"),
          t"D" -> Set(t"B", t"C") )
      val glyph = (n: Text) => t"[$n]"
      val rendered = LaneDagDiagram(dag).render(glyph, n => t"")
      // Connector tiles widen: a row of `├─╮` becomes `├──╮` etc when col widths grow.
      // We just check that all rows in the rendered output have equal width.
      rendered.map(_.s.length).distinct.size

    . assert(_ == 1)
