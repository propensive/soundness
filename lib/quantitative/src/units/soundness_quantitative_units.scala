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
package soundness

// `apply` (the `Kelvin(…)` absolute-temperature constructor) is deliberately NOT re-exported:
// its forwarder is a toplevel `apply` in package `soundness`, which collides with the deindexing
// `apply` declared by `rudiments` — the compiler keeps whichever the classpath orders first and
// silently drops the other. The extension remains available via `import quantitative.*`.
export
  quantitative
  . { Acre, Ampere, Are, Barye, Becquerel, Biot, Calorie, Candela, Coulomb, Darcy, Days,
      Debye,
      Drams, Dyne, Em, Ems, Emu, Erg, Farad, Feet, FluidOunce, Foot, Franklin, Furlong, Furlongs,
      Galileo, Gallon, Gauss, Gilbert, Grain, Grains, Gram, Gray, Henry, Hertz, Hours,
      Hundredweight,
      Hundredweights, Inch, Inches, Joule, Katal, Kayser, Kelvin, Lambert, Langley, Lightyear,
      Lightyears, Litre, Lux, Maxwell, Metre, Mile, Miles, Minutes, Mole, NauticalMile,
      NauticalMiles, Newton, Oersted, Ohm, Ounce, Ounces, Pascal, Phot, Picas, Pint, Points, Poise,
      Pound, Pounds, Quart, Quarters, Rankines, Second, SiderealDays, Siemens, Sievert, SolarDay,
      Stokes, Stone, Stones, Tesla, Ton, Tons, Typometry, Volt, Watt, Weber, Yard, Yards }

package constants:
  export
    quantitative.constants
    . { AvogadroConstant, BoltzmannConstant, CharacteristicImpedanceOfVacuum, ElectricConstant,
        ElementaryCharge, GravitationalConstant, MagneticConstant, PlanckConstant,
        SpeedOfLightInVacuum }
