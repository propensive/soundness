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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package charisma

import proscenium.compat.*

import anticipation.*
import gossamer.*
import hieroglyph.*
import hypotenuse.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Molecule:
  def apply(): Molecule = Molecule(Map(), 0)

  given showable: Molecule is Showable = molecule =>
    val orderedElements =
      if !molecule.elements.defines(PeriodicTable.C)
      then molecule.elements.toList.sort(_(0).symbol)
      else
        val carbon = PeriodicTable.C -> molecule.elements.stdlib(PeriodicTable.C)

        val hydrogen =
          if !molecule.elements.defines(PeriodicTable.H) then Nil else
            List(PeriodicTable.H -> molecule.elements.stdlib(PeriodicTable.H))

        val rest =
          (molecule.elements.stdlib - PeriodicTable.C - PeriodicTable.H)
          . to(List).sort(_(0).symbol)

        carbon :: hydrogen ::: rest

    val suffix =
      val polarity =
        if molecule.charge == 0 then t"" else if molecule.charge < 0 then t"⁻" else t"⁺"

      val magnitude = if molecule.charge.abs < 2 then t"" else
        t"${molecule.charge.abs.show.chars.map(_.superscript).sift[Char].map(_.show).join}"

      t"$magnitude$polarity${molecule.state.let(_.show).or(t"")}"

    orderedElements.map: (element, count) =>
      val number =
        if count == 1 then t"" else count.show.chars.map(_.subscript).sift[Char].map(_.show).join

      t"${element.symbol}$number"

    . join +
      suffix

  def apply(element: ChemicalElement): Molecule = element.molecule

case class Molecule
  ( elements: Map[ChemicalElement, Int], charge: Int, state: Optional[PhysicalState] = Unset )
extends Molecular:
  def molecule: Molecule = this
