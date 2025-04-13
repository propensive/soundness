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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package quantitative

import anticipation.*
import hypotenuse.*
import symbolism.*

val Metre: MetricUnit[Metres[1]] = MetricUnit(1)
val Gram: MetricUnit[Kilograms[1]] = MetricUnit(0.001)
val Candela: MetricUnit[Candelas[1]] = MetricUnit(1)
val Mole: MetricUnit[Moles[1]] = MetricUnit(1)
val Ampere: MetricUnit[Amperes[1]] = MetricUnit(1)
val Kelvin: MetricUnit[Kelvins[1]] = MetricUnit(1)
val Second: MetricUnit[Seconds[1]] = MetricUnit(1)

val Galileo = MetricUnit(0.01*Metre/(Second*Second))
val Biot = MetricUnit(10.0*Ampere)
val Stokes = MetricUnit(10e-4*Metre*Metre/Second)
val Lambert = MetricUnit((10e4/π)*Candela/(Metre*Metre))
val Emu = MetricUnit(10e-3*Ampere/(Metre*Metre))
val Oersted = MetricUnit(79.577*Ampere/Metre)
val Gilbert = MetricUnit(0.796*Ampere)
val Darcy = MetricUnit(0.987e-12*Metre*Metre)
val Kayser = MetricUnit(Quantity[Measure](100)/Second)

val Hertz = MetricUnit(Quantity[Measure](1.0)/Second)
val Newton = MetricUnit(Metre*Kilo(Gram)/(Second*Second))
val Dyne = MetricUnit(10e-5*Newton)
val Pascal = MetricUnit(Newton/(Metre*Metre))
val Barye = MetricUnit(0.1*Pascal)
val Poise = MetricUnit(0.1*Pascal)
val Joule = MetricUnit(Newton*Metre)
val Calorie = MetricUnit(4.184*Joule)
val Langley = MetricUnit(41840.0*Joule/(Metre*Metre))
val Erg = MetricUnit(10e-7*Joule)
val Watt = MetricUnit(Joule/Second)
val Coulomb = MetricUnit(Second*Ampere)
val Debye = MetricUnit(3.335e-30*Coulomb*Metre)
val Franklin = MetricUnit(3.34e-10*Coulomb)
val Volt = MetricUnit(Watt/Ampere)
val Farad = MetricUnit(Coulomb/Volt)
val Ohm = MetricUnit(Volt/Ampere)
val Siemens = MetricUnit(Ampere/Volt)
val Weber = MetricUnit(Volt*Second)
val Maxwell = MetricUnit(10e-8*Weber)
val Tesla = MetricUnit(Weber/(Metre*Metre))
val Gauss = MetricUnit(10e-4*Tesla)
val Henry = MetricUnit(Weber/Ampere)
val Lux = MetricUnit(Candela/(Metre*Metre))
val Phot = MetricUnit(10e4*Lux)
val Becquerel = MetricUnit(Quantity[Measure](1.0)/Second)
val Gray = MetricUnit(Joule/Kilo(Gram))
val Sievert = MetricUnit(Joule/Kilo(Gram))
val Katal = MetricUnit(Mole/Second)

val Inch = Quantity[Inches[1]](1.0)
val Foot = Quantity[Feet[1]](1.0)
val Yard = Quantity[Yards[1]](1.0)
val Mile = Quantity[Miles[1]](1.0)
val Lightyear = Quantity[Lightyears[1]](1.0)
val NauticalMile = Quantity[NauticalMiles[1]](1.0)
val Furlong = Quantity[Furlongs[1]](1.0)

val Grain = Quantity[Grains[1]](1.0)
val Ounce = Quantity[Ounces[1]](1.0)
val Pound = Quantity[Pounds[1]](1.0)
val Stone = Quantity[Stones[1]](1.0)
val Hundredweight = Quantity[Hundredweights[1]](1.0)
val Ton = Quantity[Tons[1]](1.0)

val Day = Quantity[Days[1]](1.0)
val Hour = Quantity[Hours[1]](1.0)
val Minute = Quantity[Minutes[1]](1.0)

val Are = MetricUnit[Metres[2]](100.0)
val Acre = Furlong*Furlong/10.0

val Litre = MetricUnit[Metres[3]](0.001)
val FluidOunce = Milli(Litre)*28.4130625
val Pint = Milli(Litre)*568.26125
val Quart = Milli(Litre)*1136.5225
val Gallon = Milli(Litre)*4546.09

package constants:
  val SpeedOfLightInVacuum = 299792458.0*Metre/Second
  val MagneticConstant = 4*π*(10e-7)*Newton/(Ampere*Ampere)
  val ElectricConstant = 8.854187817e-12*Farad/Metre
  val CharacteristicImpedanceOfVacuum = 376.730313461*Ohm
  val PlanckConstant = 6.62607015e-34*Metre*Metre*Kilo(Gram)/Second
  val GravitationalConstant = 6.67430e-11*Newton/Metre*Metre/Kilo(Gram)/Kilo(Gram)
  val ElementaryCharge = 1.602176634e-19*Coulomb
  val AvogadroConstant = Quantity[Measure](6.02214076e23)/Mole
  val BoltzmannConstant = 1.380649e-13*Joule/Kelvin
