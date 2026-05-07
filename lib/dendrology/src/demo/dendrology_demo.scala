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

import dendrology.laneDagStyles.default
import environments.java
import stdios.virtualMachine
import termcaps.environment

@main
def laneDemo(): Unit =
  def show(name: Text, dag: Dag[Text]): Unit =
    Out.println(t"=== $name ===")
    LaneDagDiagram(dag).render(node => t" $node").each(Out.println(_))
    Out.println(t"")

  def showCompact(name: Text, dag: Dag[Text]): Unit =
    Out.println(t"=== $name (compact) ===")
    LaneDagDiagram(dag).compact.render(node => t" $node").each(Out.println(_))
    Out.println(t"")

  def showHighlighted(name: Text, dag: Dag[Text], highlight: Text): Unit =
    Out.println(t"=== $name (highlight $highlight) ===")
    val glyph = (node: Text) => if node == highlight then t"★ " else t"● "
    LaneDagDiagram(dag).render(glyph, node => t" $node").each(Out.println(_))
    Out.println(t"")

  show
    ( t"linear chain",
      Dag(t"A" -> Set(), t"B" -> Set(t"A"), t"C" -> Set(t"B"), t"D" -> Set(t"C")) )

  show
    ( t"diamond",
      Dag
       ( t"A" -> Set(),
         t"B" -> Set(t"A"),
         t"C" -> Set(t"A"),
         t"D" -> Set(t"B", t"C") ) )

  show
    ( t"high fan-out",
      Dag
       ( t"root" -> Set(),
         t"a"    -> Set(t"root"),
         t"b"    -> Set(t"root"),
         t"c"    -> Set(t"root"),
         t"d"    -> Set(t"root") ) )

  show
    ( t"high fan-in",
      Dag
       ( t"a"    -> Set(),
         t"b"    -> Set(),
         t"c"    -> Set(),
         t"d"    -> Set(),
         t"sink" -> Set(t"a", t"b", t"c", t"d") ) )

  show
    ( t"long edge over rows",
      Dag
       ( t"A" -> Set(),
         t"B" -> Set(t"A"),
         t"C" -> Set(t"B"),
         t"D" -> Set(t"C"),
         t"E" -> Set(t"A", t"D") ) )

  show
    ( t"two independent branches",
      Dag
       ( t"A" -> Set(),
         t"B" -> Set(t"A"),
         t"C" -> Set(),
         t"D" -> Set(t"C") ) )

  val scalaTypes = Dag
    ( t"Any"        -> Set(),
      t"Matchable"  -> Set(t"Any"),
      t"AnyVal"     -> Set(t"Matchable"),
      t"AnyRef"     -> Set(t"Matchable"),
      t"Unit"       -> Set(t"AnyVal"),
      t"Boolean"    -> Set(t"AnyVal"),
      t"Int"        -> Set(t"AnyVal"),
      t"String"     -> Set(t"AnyRef"),
      t"List[Int]"  -> Set(t"AnyRef"),
      t"Null"       -> Set(t"String", t"List[Int]"),
      t"Nothing"    -> Set(t"Null", t"Unit", t"Boolean", t"Int") )

  show(t"Scala types", scalaTypes)

  showCompact(t"Scala types", scalaTypes)

  showHighlighted(t"Scala types", scalaTypes, t"AnyVal")
