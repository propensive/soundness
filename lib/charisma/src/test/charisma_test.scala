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

import soundness.*

import PeriodicTable.*

object Tests extends Suite(m"Charisma Tests"):
  def run(): Unit =
    suite(m"Periodic table tests"):
      test(m"Look up an element by atomic number"):
        PeriodicTable(26)
      . assert(_ == Fe)

      test(m"Look up the first element by atomic number"):
        PeriodicTable(1)
      . assert(_ == H)

      test(m"Look up the last element by atomic number"):
        PeriodicTable(118)
      . assert(_ == Og)

      test(m"An atomic number of zero has no element"):
        PeriodicTable(0)
      . assert(_ == Unset)

      test(m"An atomic number beyond the table has no element"):
        PeriodicTable(119)
      . assert(_ == Unset)

      test(m"Look up an element by symbol"):
        PeriodicTable(t"Cu")
      . assert(_ == Cu)

      test(m"An unknown symbol has no element"):
        PeriodicTable(t"Zz")
      . assert(_ == Unset)

      test(m"Symbol lookup is case-sensitive"):
        PeriodicTable(t"cu")
      . assert(_ == Unset)

      test(m"Every element's number matches its position"):
        (0 until PeriodicTable.elements.length).all: index =>
          PeriodicTable.elements.readUnchecked(index).number == index + 1
      . assert(_ == true)

      test(m"Every element has a distinct symbol"):
        PeriodicTable.symbols.stdlib.size
      . assert(_ == 118)

      test(m"An element is shown as its symbol"):
        Fe.show
      . assert(_ == t"Fe")

      test(m"Element 119 has a systematic name"):
        PeriodicTable.element(119).name
      . assert(_ == t"Ununennium")

      test(m"Element 119 has a systematic symbol"):
        PeriodicTable.element(119).symbol
      . assert(_ == t"Uue")

      test(m"Element 120 has a systematic name"):
        PeriodicTable.element(120).name
      . assert(_ == t"Unbinilium")

      test(m"Doubled `i`s are elided from systematic names"):
        PeriodicTable.element(122).name
      . assert(_ == t"Unbibium")

      test(m"A systematic element keeps its atomic number"):
        PeriodicTable.element(126).number
      . assert(_ == 126)

    suite(m"Molecule construction tests"):
      test(m"An element on its own is a single-atom molecule"):
        O.molecule.elements
      . assert(_ == Map(O -> 1))

      test(m"A subscripted element repeats the atom"):
        O[2].elements
      . assert(_ == Map(O -> 2))

      test(m"Combining elements sums their atoms"):
        (H[2]*O).elements
      . assert(_ == Map(H -> 2, O -> 1))

      test(m"Combining three elements accumulates atoms"):
        (C[6]*H[12]*O[6]).elements
      . assert(_ == Map(C -> 6, H -> 12, O -> 6))

      test(m"Multiplying a molecule scales every atom"):
        (H[2]*O ** 3).elements
      . assert(_ == Map(H -> 6, O -> 3))

      test(m"A new molecule is uncharged"):
        (H[2]*O).charge
      . assert(_ == 0)

      test(m"A new molecule has no physical state"):
        (H[2]*O).state
      . assert(_ == Unset)

      test(m"Unary minus decrements the charge"):
        (-Cl.molecule).charge
      . assert(_ == -1)

      test(m"Unary plus increments the charge"):
        (+(N*H[4])).charge
      . assert(_ == 1)

      test(m"An explicit charge replaces the existing one"):
        (S*O[4]).ion(-2).charge
      . assert(_ == -2)

      test(m"Combining molecules sums their charges"):
        (Na.molecule.ion(1)*Cl.molecule.ion(-1)).charge
      . assert(_ == 0)

      test(m"Multiplying a molecule scales its charge"):
        ((S*O[4]).ion(-2) ** 3).charge
      . assert(_ == -6)

      test(m"A physical state can be attached to a molecule"):
        (Na*Cl).inState(PhysicalState.Aqueous).state
      . assert(_ == PhysicalState.Aqueous)

    suite(m"Molecule rendering tests"):
      test(m"Water is rendered with a subscript"):
        (H[2]*O).show
      . assert(_ == t"H₂O")

      test(m"Elements are ordered alphabetically without carbon"):
        (S*O[4]).show
      . assert(_ == t"O₄S")

      test(m"Carbon and hydrogen lead in an organic molecule"):
        (C[6]*H[12]*O[6]).show
      . assert(_ == t"C₆H₁₂O₆")

      test(m"Carbon leads even without hydrogen"):
        (C*O[2]).show
      . assert(_ == t"CO₂")

      test(m"A single anion charge is a bare minus sign"):
        (-Cl.molecule).show
      . assert(_ == t"Cl⁻")

      test(m"A single cation charge is a bare plus sign"):
        (+(N*H[4])).show
      . assert(_ == t"H₄N⁺")

      test(m"A charge above one is rendered as a superscript"):
        (S*O[4]).ion(-2).show
      . assert(_ == t"O₄S²⁻")

      test(m"A multi-digit charge is rendered as superscripts"):
        C.molecule.ion(12).show
      . assert(_ == t"C¹²⁺")

      test(m"A physical state is appended to the molecule"):
        (Na*Cl).inState(PhysicalState.Aqueous).show
      . assert(_ == t"ClNa(aq)")

      test(m"A charge precedes the physical state"):
        (-Cl.molecule).inState(PhysicalState.Aqueous).show
      . assert(_ == t"Cl⁻(aq)")

      test(m"Physical states are rendered in parentheses"):
        Array.unsafeFrozen(PhysicalState.values).readable.to(List).map(_.show)
      . assert(_ == List(t"(s)", t"(l)", t"(g)", t"(aq)"))

    suite(m"Chemical formula tests"):
      test(m"A molecule's formula has a single term"):
        (H[2]*O).formula.molecules
      . assert(_ == Ledger((H[2]*O) -> 1))

      test(m"Scaling a molecule gives a formula with a coefficient"):
        (H[2]*2).molecules
      . assert(_ == Ledger(H[2] -> 2))

      test(m"Summing formulae collects both terms"):
        (H[2]*2 + O[2]).molecules
      . assert(_ == Ledger(H[2] -> 2, O[2] -> 1))

      test(m"Summing the same molecule twice adds its coefficients"):
        (H[2]*2 + H[2]*3).molecules
      . assert(_ == Ledger(H[2] -> 5))

      test(m"A formula's atoms tally every molecule"):
        (H[2]*2 + O[2]).atoms
      . assert(_ == Map(H -> 4, O -> 2))

      test(m"An empty coefficient is omitted when rendering"):
        (H[2]*O).formula.show
      . assert(_ == t"H₂O")

      test(m"A coefficient is rendered before the molecule"):
        (H[2]*2).show
      . assert(_ == t"2H₂")

      test(m"Formula terms are joined with a plus sign"):
        (H[2]*2 + O[2]).show
      . assert(_ == t"2H₂ + O₂")

    suite(m"Chemical equation tests"):
      val hydrogen = H[2]
      val oxygen = O[2]
      val water = H[2]*O

      test(m"A balanced equation has matching atom counts"):
        (hydrogen*2 + oxygen --> water*2).balanced
      . assert(_ == true)

      test(m"An unbalanced equation has differing atom counts"):
        (hydrogen + oxygen --> water).balanced
      . assert(_ == false)

      test(m"An equation records its left-hand side"):
        (hydrogen*2 + oxygen --> water*2).lhs.atoms
      . assert(_ == Map(H -> 4, O -> 2))

      test(m"An equation records its right-hand side"):
        (hydrogen*2 + oxygen --> water*2).rhs.atoms
      . assert(_ == Map(H -> 4, O -> 2))

      test(m"A net-forward equation uses a single arrow"):
        (hydrogen*2 + oxygen --> water*2).show
      . assert(_ == t"2H₂ + O₂ → 2H₂O")

      test(m"Reactions are rendered with distinct arrows"):
        Array.unsafeFrozen(Reaction.values).readable.to(List).map(_.show)
      . assert(_ == List(t"→", t"⇄", t"⇋", t"↔", t"="))

      test(m"A resonance equation has a resonance reaction"):
        (water <-> water).reaction
      . assert(_ == Reaction.Resonance)

      test(m"A both-directions equation is reversible"):
        (water <=> water).reaction
      . assert(_ == Reaction.BothDirections)

      test(m"An equilibrium equation is at equilibrium"):
        (water <~> water).reaction
      . assert(_ == Reaction.Equilibrium)

      test(m"A stoichiometric equation is stoichiometric"):
        (water === water).reaction
      . assert(_ == Reaction.Stoichiometric)
