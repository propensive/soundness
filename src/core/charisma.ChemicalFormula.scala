/*
    Charisma, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package charisma

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

object ChemicalFormula:
  def apply(molecule: Molecule): ChemicalFormula = ChemicalFormula(ListMap(molecule -> 1))

  given ChemicalFormula is Showable as show = formula =>
    formula.molecules.to(List).map: (molecule, count) =>
      (if count == 1 then t"" else count.show)+molecule.show

    . join(t" + ")

case class ChemicalFormula(molecules: ListMap[Molecule, Int]) extends Formulable:
  def formula: ChemicalFormula = this

  def atoms: Map[ChemicalElement, Int] =
    molecules.foldLeft(Molecule.empty):
      case (acc, (molecule, count)) => acc*(molecule**count)

    . elements
