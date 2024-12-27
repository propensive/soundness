/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

object MacOs:
  object Root

  abstract class Root() extends serpentine.Root(t"/", t"/", Case.Preserving):
    type Platform = MacOs

  object RootSingleton extends Root()

  type Rules = MustNotContain["/"] & MustNotEqual["."] & MustNotEqual[".."] & MustNotEqual[""]

  given (using Tactic[PathError]) => MacOs is Radical from Root as radical = new Radical:
    type Self = MacOs
    type Source = Root

    def rootLength(path: Text): Int = 1
    def rootText(root: Source): Text = t"/"

    def root(path: Text): Source = if path.at(Prim) == '/' then $ else
      raise(PathError(PathError.Reason.InvalidRoot, path)) yet $

  given (using Tactic[NameError]) => MacOs is Navigable by Name[MacOs] under Rules as navigable =
    new Navigable:
      type Self = MacOs
      type Operand = Name[MacOs]
      type Constraint = Rules

      val separator: Text = t"/"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[MacOs] = Name(element)
      def elementText(element: Name[MacOs]): Text = element.text
      def caseSensitivity: Case = Case.Preserving

erased trait MacOs extends Posix
