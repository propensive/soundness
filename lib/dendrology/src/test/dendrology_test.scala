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
┃    Soundness, version 0.54.0.                                                                    ┃
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
    import dagStyles.default
    DagDiagram(types).render { node => t"▪ $node" }

    test(m"Lane DAG: linear chain"):
      import laneDagStyles.default
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"))
      LaneDagDiagram(dag).render(node => t" $node").join(t"\n")

    . assert(_ == t"●  A\n│ \n●  B\n│ \n●  C")

    test(m"Lane DAG: diamond"):
      import laneDagStyles.default
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(t"A"),
          t"D" -> Set(t"B", t"C") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 7)

    test(m"Lane DAG: diamond layout shapes"):
      import laneDagStyles.default
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
      import laneDagStyles.default
      val dag = Dag(t"A" -> Set())
      LaneDagDiagram(dag).render(node => t" $node").join(t"\n")

    . assert(_ == t"●  A")

    test(m"Lane DAG: high fan-out"):
      import laneDagStyles.default
      val dag = Dag
        ( t"root" -> Set(),
          t"a"    -> Set(t"root"),
          t"b"    -> Set(t"root"),
          t"c"    -> Set(t"root"),
          t"d"    -> Set(t"root") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 9)

    test(m"Lane DAG: high fan-in"):
      import laneDagStyles.default
      val dag = Dag
        ( t"a"    -> Set(),
          t"b"    -> Set(),
          t"c"    -> Set(),
          t"d"    -> Set(),
          t"sink" -> Set(t"a", t"b", t"c", t"d") )
      LaneDagDiagram(dag).render(node => t" $node").size

    . assert(_ == 9)

    test(m"Lane DAG: linear chain detail"):
      import laneDagStyles.default
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"))
      LaneDagDiagram(dag).render(node => t" $node")

    . assert(_ == List(t"●  A", t"│ ", t"●  B", t"│ ", t"●  C"))

    test(m"Lane DAG: crossing renders as horizontal not junction"):
      import laneDagStyles.default
      val dag = Dag
        ( t"A" -> Set(),
          t"B" -> Set(t"A"),
          t"C" -> Set(),
          t"D" -> Set(t"A", t"C") )
      LaneDagDiagram(dag).render(node => t"").map(_.s).mkString("\n").linesIterator.toList.size

    . assert(_ == 7)

    test(m"Lane DAG: per-vertex glyph"):
      import laneDagStyles.default
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      val glyph = (n: Text) => if n == t"A" then t"★ " else t"● "
      LaneDagDiagram(dag).render(glyph, n => t" $n")

    . assert(_ == List(t"★  A", t"│ ", t"●  B"))

    test(m"Lane DAG: compact preserves single-vertical row"):
      import laneDagStyles.default
      // A→B is a direct edge: connector has exactly one Vertical, must stay.
      val dag = Dag(t"A" -> Set(), t"B" -> Set(t"A"))
      LaneDagDiagram(dag).compact.render(n => t" $n").size

    . assert(_ == 3)

    test(m"Lane DAG: compact removes multi-vertical pass-through row"):
      import laneDagStyles.default
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
