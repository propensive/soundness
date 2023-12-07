/*
    Dendrology, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package dendrology

import probably.*
import rudiments.*
import gossamer.*
import anticipation.*
import turbulence.*, stdioSources.jvm

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Dendrology tests"):

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

  def run(): Unit =
    import treeStyles.rounded
    drawTree[Tree, Text](_.children, tree => t"● ${tree.value}")(life).foreach(Out.println(_))
