/*
    Quantify, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantify

import rudiments.*

case class Ratio[UnitsType <: Measure](value: Quantity[UnitsType])

// Units of length

trait Inches[Power <: Nat] extends Units[Power, Length]
object Inches:
  given inchesPerMetre: Ratio[Inches[1] & Metres[-1]] = Ratio(Quantity(39.3701))

trait Feet[Power <: Nat] extends Units[Power, Length]
object Feet:
  given feetPerMetre: Ratio[Feet[1] & Metres[-1]] = Ratio(Quantity(3.28084))

trait Yards[Power <: Nat] extends Units[Power, Length]
object Yards:
  given yardsPerMetre: Ratio[Yards[1] & Metres[-1]] = Ratio(Quantity(1.09361))

trait Miles[Power <: Nat] extends Units[Power, Length]
object Miles:
  given milesPerMetre: Ratio[Miles[1] & Metres[-1]] = Ratio(Quantity(0.000621371))

trait Lightyears[Power <: Nat] extends Units[Power, Length]
object Lightyears:
  given lightYearsPerMetre: Ratio[Lightyears[1] & Metres[-1]] = Ratio(Quantity(1.057e-16))

trait NauticalMiles[Power <: Nat] extends Units[Power, Length]
object NauticalMiles:
  given nauticalMilesPerMetre: Ratio[NauticalMiles[1] & Metres[-1]] = Ratio(Quantity(1.0/1852.0))

trait Furlongs[Power <: Nat] extends Units[Power, Length]
object Furlongs:
  given metresPerFurlong: Ratio[Metres[1] & Furlongs[-1]] = Ratio(Quantity(201.168))

trait Chains[Power <: Nat] extends Units[Power, Length]
object Chains:
  given metresPerChain: Ratio[Metres[1] & Chains[-1]] = Ratio(Quantity(20.1168))

val Inch = Quantity[Inches[1]](1.0)
val Foot = Quantity[Feet[1]](1.0)
val Yard = Quantity[Yards[1]](1.0)
val Mile = Quantity[Miles[1]](1.0)
val Lightyear = Quantity[Lightyears[1]](1.0)
val NauticalMile = Quantity[NauticalMiles[1]](1.0)
val Furlong = Quantity[Furlongs[1]](1.0)
val Chain = Quantity[Chains[1]](1.0)

// Units of Area

val Acre = Furlong*Furlong/10.0

// Units of Volume

val Litre = Quantity[Metres[3]](0.001)
val Millilitre = Litre/1000.0
val FluidOunce = Millilitre*28.4130625
val Pint = Millilitre*568.26125
val Quart = Millilitre*1136.5225
val Gallon = Millilitre*4546.09

// Units of Mass

trait Grains[Power <: Nat] extends Units[Power, Mass]
object Grains:
  given grainsPerGram: Ratio[Grains[1] & Grams[-1]] = Ratio(Quantity(15.4324))

trait Ounces[Power <: Nat] extends Units[Power, Mass]
object Ounces:
  given ouncesPerGram: Ratio[Ounces[1] & Grams[-1]] = Ratio(Quantity(0.035274))

trait Pounds[Power <: Nat] extends Units[Power, Mass]
object Pounds:
  given gramsPerPound: Ratio[Grams[1] & Pounds[-1]] = Ratio(Quantity(453.59237))

trait Stones[Power <: Nat] extends Units[Power, Mass]
object Stones:
  given gramsPerStone: Ratio[Grams[1] & Stones[-1]] = Ratio(Quantity(6350.29318))

trait Hundredweights[Power <: Nat] extends Units[Power, Mass]
object Hundredweights:
  given gramsPerHundredweight: Ratio[Grams[1] & Hundredweights[-1]] = Ratio(Quantity(50802.34544))

trait Tons[Power <: Nat] extends Units[Power, Mass]
object Tons:
  given gramsPerTon: Ratio[Grams[1] & Tons[1]] = Ratio(Quantity(1016046.9088))

val Grain = Quantity[Grains[1]](1.0)
val Ounce = Quantity[Ounces[1]](1.0)
val Pound = Quantity[Pounds[1]](1.0)
val Stone = Quantity[Stones[1]](1.0)
val Hundredweight = Quantity[Hundredweights[1]](1.0)
val Ton = Quantity[Tons[1]](1.0)