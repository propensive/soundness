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
import spectacular.*

object ChemicalEquation:
  given ChemicalEquation is Showable = equation =>
    t"${equation.lhs} ${equation.reaction} ${equation.rhs}"

case class ChemicalEquation(lhs: ChemicalFormula, reaction: Reaction, rhs: ChemicalFormula):
  def balanced: Boolean = lhs.atoms == rhs.atoms
