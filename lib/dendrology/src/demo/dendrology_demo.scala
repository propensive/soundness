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
package dendrology

import soundness.*

import dendrology.laneDagStyles.boxDrawingLaneDagStyle
import environments.javaBaseEnvironment
import stdios.fileDescriptorStdio
import strategies.throwUnsafely
import termcaps.environmentTermcap

@main
def laneDemo(): Unit =
  def show(name: Text, dag: Dag[Text]): Unit =
    Out.println(t"=== $name ===")
    LaneDagDiagram(dag).render: node => t" $node"
    . each(Out.println(_))

    Out.println("")

  def showCompact(name: Text, dag: Dag[Text]): Unit =
    Out.println(t"=== $name (compact) ===")
    LaneDagDiagram(dag).compact.render{ node => t" $node" }.each(Out.println(_))
    Out.println("")

  def showHighlighted(name: Text, dag: Dag[Text], highlight: Text): Unit =
    Out.println(t"=== $name (highlight $highlight) ===")
    val glyph = (node: Text) => if node == highlight then "★ " else "● "
    LaneDagDiagram(dag).render(glyph, node => t" $node").each(Out.println(_))
    Out.println("")

  def showLayered(name: Text, dag: Dag[Text]): Unit =
    Out.println(t"=== $name (layered) ===")
    LayeredDagDiagram(dag).render{ node => t"● $node  " }.each(Out.println(_))
    Out.println("")

  show
    ( "linear chain",
      Dag("A" -> Set(), "B" -> Set("A"), "C" -> Set("B"), "D" -> Set("C")) )

  show
    ( "diamond",
      Dag
       ( "A" -> Set(),
         "B" -> Set("A"),
         "C" -> Set("A"),
         "D" -> Set("B", "C") ) )

  show
    ( "high fan-out",
      Dag
       ( "root" -> Set(),
         "a"    -> Set("root"),
         "b"    -> Set("root"),
         "c"    -> Set("root"),
         "d"    -> Set("root") ) )

  show
    ( "high fan-in",
      Dag
       ( "a"    -> Set(),
         "b"    -> Set(),
         "c"    -> Set(),
         "d"    -> Set(),
         "sink" -> Set("a", "b", "c", "d") ) )

  show
    ( "long edge over rows",
      Dag
       ( "A" -> Set(),
         "B" -> Set("A"),
         "C" -> Set("B"),
         "D" -> Set("C"),
         "E" -> Set("A", "D") ) )

  show
    ( "two independent branches",
      Dag
       ( "A" -> Set(),
         "B" -> Set("A"),
         "C" -> Set(),
         "D" -> Set("C") ) )

  val scalaTypes =
    Dag
      ( "Any"        -> Set(),
        "Matchable"  -> Set("Any"),
        "AnyVal"     -> Set("Matchable"),
        "AnyRef"     -> Set("Matchable"),
        "Unit"       -> Set("AnyVal"),
        "Boolean"    -> Set("AnyVal"),
        "Int"        -> Set("AnyVal"),
        "String"     -> Set("AnyRef"),
        "List[Int]"  -> Set("AnyRef"),
        "Null"       -> Set("String", "List[Int]"),
        "Nothing"    -> Set("Null", "Unit", "Boolean", "Int") )

  show("Scala types", scalaTypes)

  showCompact("Scala types", scalaTypes)

  showHighlighted("Scala types", scalaTypes, "AnyVal")

  showLayered("diamond",
    Dag
     ( "A" -> Set(),
       "B" -> Set("A"),
       "C" -> Set("A"),
       "D" -> Set("B", "C") ))

  showLayered("Scala types", scalaTypes)
