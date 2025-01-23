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
import hieroglyph.*
import hypotenuse.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

erased trait Windows extends Filesystem

object Windows:
  type Rules = MustNotContain["\\"] & MustNotContain["/"] & MustNotContain[":"] &
      MustNotContain["*"] & MustNotContain["?"] & MustNotContain["\""] & MustNotContain["<"] &
      MustNotContain[">"] & MustNotContain["|"] & MustNotEnd["."] & MustNotEnd[" "] &
      MustNotMatch["(?i)CON(\\.[^.]+)?"] & MustNotEqual["(?i)PRN(\\.[^.]+)?"] &
      MustNotEqual["(?i)AUX(\\.[^.]+)?"] & MustNotEqual["(?i)NUL(\\.[^.]+)?"] &
      MustNotEqual["(?i)COM[0-9](\\.[^.]+)?"] & MustNotEqual["(?i)LPT[0-9](\\.[^.]+)?"]

  given navigable: Tactic[NameError]
  => Windows is Navigable by Name[Windows] under Rules =
    new Navigable:
      type Operand = Name[Windows]
      type Self = Windows
      type Constraint = Rules

      val separator: Text = t"\\"
      val parentElement: Text = t".."
      val selfText: Text = t"."
      def element(element: Text): Name[Windows] = Name(element)
      def elementText(element: Name[Windows]): Text = element.text
      def caseSensitivity: Case = Case.Preserving

  given radical: Tactic[PathError] => Windows is Radical from WindowsDrive = new Radical:
    type Self = Windows
    type Source = WindowsDrive

    def root(path: Text): WindowsDrive =
      if path.length < 3
      then raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')
      else unsafely(path.at(Prim).vouch).upper.pipe: letter =>
        if path.segment(Sec ~ Ter) == t":\\" && 'A' <= letter <= 'Z' then WindowsDrive(letter)
        else raise(PathError(PathError.Reason.InvalidRoot, path)) yet WindowsDrive('Z')

    def rootLength(path: Text): Int = 3
    def rootText(drive: Source): Text = t"${drive.letter}:\\"
