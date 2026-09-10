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
package polyvinyl

import anticipation.*
import gossamer.*
import prepositional.*
import rudiments.*

// A `Specification` over `Tree`, exercising every kind of member polyvinyl supports: scalar
// fields with and without parameters, and nested and repeated records.
object TreeBlueprint:
  // How many times the `"counted"` field has been evaluated: records evaluate a field on every
  // access, whereas a tuple evaluates each field once, when it is built.
  val evaluations: java.util.concurrent.atomic.AtomicInteger =
    java.util.concurrent.atomic.AtomicInteger(0)

  private def leaf(tree: Tree): Text = tree match
    case Tree.Leaf(text) => text
    case _               => t""

  given text: ("text" is Intensional in TreeBlueprint from Tree to Text) =
    TreeBlueprint.intensional(leaf(_))

  // A result type which is neither the origin type nor `Text`
  given length: ("length" is Intensional in TreeBlueprint from Tree to Int) =
    TreeBlueprint.intensional(leaf(_).s.length)

  given flag: ("flag" is Intensional in TreeBlueprint from Tree to Boolean) =
    TreeBlueprint.intensional(_ != Tree.Absent)

  given tree: ("tree" is Intensional in TreeBlueprint from Tree to Tree) =
    TreeBlueprint.intensional: tree => tree

  // Returns the member's parameters verbatim, to show that they reach the instance
  given params: ("params" is Intensional in TreeBlueprint from Tree to List[Text]) =
    new Intensional:
      type Self = "params"
      type Origin = Tree
      type Form = TreeBlueprint
      type Result = List[Text]

      def transform(tree: Tree, params: List[Text]): List[Text] = params

  given counted: ("counted" is Intensional in TreeBlueprint from Tree to Int) =
    TreeBlueprint.intensional: tree => evaluations.incrementAndGet()

  given node: ("node" is Structural[[value] =>> value] in TreeBlueprint from Tree) =
    new Structural[[value] =>> value]:
      type Self = "node"
      type Origin = Tree
      type Form = TreeBlueprint

      def transform[value](tree: Tree, make: Tree => value): value = make(tree)

  given items: ("items" is Structural[List] in TreeBlueprint from Tree) =
    new Structural[List]:
      type Self = "items"
      type Origin = Tree
      type Form = TreeBlueprint

      def transform[value](tree: Tree, make: Tree => value): List[value] = tree match
        case Tree.Items(items) => items.map(make)
        case _                 => List()

  // A pure function (`->`): the instance retains it, and a capturing accessor would make the
  // instance itself a capability, which its pure self type forbids.
  def intensional[name <: Label, value](accessor: Tree -> value)
  :   name is Intensional in TreeBlueprint from Tree to value =

    new Intensional:
      type Self = name
      type Origin = Tree
      type Form = TreeBlueprint
      type Result = value

      def transform(tree: Tree, params: List[Text]): value = accessor(tree)

  def record(data0: Tree, access0: Text -> Tree -> Any): Record = new Record:
    type Origin = Tree
    val data: Tree = data0
    def access: Text -> Tree -> Any = access0

abstract class TreeBlueprint(val fields: List[(Text, Member)]) extends Specification:
  type Origin = Tree
  type Form = TreeBlueprint

  def access(name: Text, tree: Tree): Tree = tree match
    case Tree.Node(children) if Map.defines(children, name) => Map.at(children, name)
    case _                                                  => Tree.Absent

  def build(data: Tree, access: Text -> Tree -> Any): Record = TreeBlueprint.record(data, access)
