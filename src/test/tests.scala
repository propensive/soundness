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
import acyclicity.*
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
    import treeStyles.default
    import dagStyles.default
    DagDiagram(types).render { node => t"▪ $node" }.foreach(println(_))
