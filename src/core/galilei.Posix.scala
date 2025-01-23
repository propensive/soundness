/*
    Galilei, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contingency.*
import denominative.*
import gossamer.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import vacuous.*

object Posix:
  abstract class Root() extends serpentine.Root(t"/", t"/", Case.Sensitive):
    type Platform = Posix

  object RootSingleton extends Root()

  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given radical: Tactic[PathError] => Posix is Radical from (Root on Posix) = new Radical:
    type Self = Posix
    type Source = Root on Posix

    def rootLength(path: Text): Int = 1
    def rootText(root: Source): Text = t"/"

    def root(path: Text): Source =
      if path.at(Prim) == '/' then Posix.RootSingleton
      else raise(PathError(PathError.Reason.InvalidRoot, path)) yet Posix.RootSingleton

  given navigable: Tactic[NameError] => Posix is Navigable by Name[Posix] under Rules =
    new Navigable:
      type Self = Posix
      type Operand = Name[Posix]
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[Posix] = Name(element)
      def elementText(element: Name[Posix]): Text = element.text
      def caseSensitivity: Case = Case.Sensitive

erased trait Posix extends Filesystem
