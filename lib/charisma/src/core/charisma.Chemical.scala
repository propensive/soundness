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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import anticipation.*
import denominative.nil
import gossamer.*
import rudiments.*
import spectacular.*
import symbolism.*

// A namespace for the chemistry vocabulary. There is no `Chemical` type: the members
// name the kinds of chemical thing the library models.
object Chemical:
  // ChemicalElement → Chemical.Element
  object Element:
    given showable: Element is Showable = _.symbol

    // The `Showable` is the bare symbol, which is indistinguishable from any other short `Text`;
    // the atom sign marks it as an element and the atomic number (which, with the symbol, fixes
    // the periodic-table entry the value came from) follows it.
    given inspectable: [element <: Element] => element is Inspectable = element =>
      t"⚛${element.symbol}(${element.number})"

  case class Element(number: Int, symbol: Text, name: Text) extends Molecular:
    def apply[count <: Nat: ValueOf]: Molecule = Molecule(Map(this -> valueOf[count]), 0)
    def molecule: Molecule = apply[1]

  // ChemicalEquation → Chemical.Equation
  object Equation:
    given showable: Equation is Showable = equation =>
      t"${equation.lhs} ${equation.reaction} ${equation.rhs}"

    // The reaction is named rather than drawn as its arrow: an arrow glyph is what `Reaction`'s
    // `Showable` produces, and a borrowed rendering is exactly what inspection must not look like.
    given inspectable: [equation <: Equation] => equation is Inspectable = equation =>
      t"Equation(${equation.lhs.inspect} ╱ ${equation.reaction.inspect} ╱ ${equation.rhs.inspect})"

  case class Equation(lhs: Formula, reaction: Reaction, rhs: Formula):
    def balanced: Boolean = lhs.atoms == rhs.atoms

  // ChemicalFormula → Chemical.Formula
  object Formula:
    def apply(molecule: Molecule): Formula = Formula(Ledger(molecule -> 1))

    given showable: Formula is Showable = formula =>
      formula.molecules.to[List].map: (molecule, count) =>
        (if count == 1 then t"" else count.show)+molecule.show

      . join(t" + ")

    // Every coefficient is written out, including a `1` which the `Showable` leaves implicit, so
    // that a one-molecule formula is never rendered identically to the `Molecule` it holds.
    given inspectable: [formula <: Formula] => formula is Inspectable = formula =>
      val parts = formula.molecules.to[List].map: (molecule, count) =>
        val number: Text = count.show
        t"$number${molecule.inspect}"

      if parts.nil then t"∅" else parts.join(t" + ")

  case class Formula(molecules: Ledger[Molecule, Int]) extends Formulable:
    def formula: Formula = this

    def atoms: Map[Element, Int] =
      molecules.fuse(Molecule()):
        val (molecule, count) = next
        state*(molecule**count)

      . elements
