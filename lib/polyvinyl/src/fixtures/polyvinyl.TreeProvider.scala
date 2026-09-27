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

// A `Specification` over `Tree`, exercising every kind of member polyvinyl supports: scalar
// fields with and without parameters, under each multiplicity, and nested records.
object TreeProvider:
  // How many times the `"counted"` field has been evaluated: records evaluate a field on every
  // access, whereas a tuple evaluates each field once, when it is built.
  val evaluations: java.util.concurrent.atomic.AtomicInteger =
    java.util.concurrent.atomic.AtomicInteger(0)

  private def leaf(tree: Tree): Text = tree match
    case Tree.Leaf(text) => text
    case _               => t""

  given text: ("text" is Intensional in TreeProvider from Tree to Text) = Intensional(leaf(_))

  // A result type which is neither the origin type nor `Text`
  given length: ("length" is Intensional in TreeProvider from Tree to Int) =
    Intensional(leaf(_).s.length)

  given flag: ("flag" is Intensional in TreeProvider from Tree to Boolean) =
    Intensional(_ != Tree.Absent)

  given tree: ("tree" is Intensional in TreeProvider from Tree to Tree) = Intensional(identity)

  // Returns the member's parameters verbatim, to show that they reach the instance
  given params: ("params" is Intensional in TreeProvider from Tree to List[Text]) =
    Intensional.parametric { (tree, params) => params }

  given counted: ("counted" is Intensional in TreeProvider from Tree to Int) =
    Intensional { tree => evaluations.incrementAndGet() }

abstract class TreeProvider(val fields: List[(Text, Member)]) extends Specification:
  type Origin = Tree
  type Form = TreeProvider

  def access(name: Text, tree: Tree): Tree = tree match
    case Tree.Node(children) if Map.defines(children, name) => Map.at(children, name)
    case _                                                  => Tree.Absent

  def absent(tree: Tree): Boolean = tree == Tree.Absent

  // A repeated field's values are the items of an `Items` node; a single value is a one-element
  // list, and an absent field has none.
  def repeated(name: Text, tree: Tree): List[Tree] = elements(access(name, tree))

  override def elements(tree: Tree): List[Tree] = tree match
    case Tree.Items(items) => items
    case Tree.Absent       => List()
    case other             => List(other)

  override def kind(tree: Tree): Text = tree match
    case Tree.Leaf(_)  => t"leaf"
    case Tree.Node(_)  => t"node"
    case Tree.Items(_) => t"items"
    case Tree.Absent   => t"absent"

  override def pairs(tree: Tree): List[(Text, Tree)] = tree match
    case Tree.Node(children) => List.from(children.stdlib.toList)
    case _                   => List()

  override def entries(name: Text, tree: Tree): List[(Text, Tree)] = pairs(access(name, tree))
