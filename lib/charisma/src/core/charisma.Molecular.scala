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

import proscenium.*
import vacuous.*

trait Molecular extends Formulable:
  def molecule: Molecule
  def formula: ChemicalFormula = ChemicalFormula(molecule)

  @targetName("with")
  infix def * (moleculable: Molecular): Molecule =
    val elements2 = moleculable.molecule.elements.foldLeft(molecule.elements): (acc, next) =>
      acc.updated(next(0), molecule.elements.getOrElse(next(0), 0) + next(1))

    Molecule(elements2, molecule.charge + moleculable.molecule.charge)

  @targetName("times")
  infix def * (multiplier: Int): ChemicalFormula = ChemicalFormula(ListMap(molecule -> multiplier))

  @targetName("times2")
  infix def ** (multiplier: Int): Molecule =
    Molecule
     (molecule.elements.view.mapValues(_*multiplier).to(Map), molecule.charge*multiplier, Unset)

  @targetName("cation")
  def `unary_-`: Molecule = molecule.copy(charge = molecule.charge - 1)

  @targetName("anion")
  def `unary_+`: Molecule = molecule.copy(charge = molecule.charge + 1)

  def ion(charge: Int): Molecule = molecule.copy(charge = charge)
  def as(state: Optional[PhysicalState]): Molecule = molecule.copy(state = state)
