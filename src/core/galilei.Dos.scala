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
import hieroglyph.*
import hypotenuse.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

erased trait Dos extends Filesystem

object Dos:
  type Rules = MustMatch["[^.]{1,8}(\\.[^.]{1,3})?"] & MustNotContain[" "] & MustMatch["[!-~]*"]

  given (using Tactic[PathError]) => Dos is Radical from DosDrive as radical = new Radical:
    type Self = Dos
    type Source = DosDrive

    def rootLength(path: Text): Int = 3
    def rootText(drive: Source): Text = t"${drive.letter}:\\"

    def root(path: Text): DosDrive =
      if path.length < 3
      then raise(PathError(PathError.Reason.InvalidRoot, path)) yet DosDrive('Z')
      else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
        if path.segment(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then DosDrive(letter)
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet DosDrive('Z')


  given (using Tactic[NameError]) => Dos is Navigable by Name[Dos] under Rules as navigable =
    new Navigable:
      type Operand = Name[Dos]
      type Self = Dos
      type Constraint = Rules

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."

      def element(element: Text): Name[Dos] = Name(element)
      def elementText(element: Name[Dos]): Text = element.text
      def caseSensitivity: Case = Case.Upper
