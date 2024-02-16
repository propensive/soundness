/*
    Quantitative, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package quantitative

import rudiments.*
import anticipation.*

import language.experimental.captureChecking

erased trait Ratio[UnitsType <: Measure, RatioType <: Double & Singleton]

// Units of length

trait Points[Power <: Nat] extends Units[Power, Length]
object Points:
  given UnitName[Points[1]] = () => "pt".tt
  erased given pointsPerMetre: Ratio[Points[-1] & Metres[1], 3.5277777777777776E-4] = ###

trait Picas[Power <: Nat] extends Units[Power, Length]
object Picas:
  given UnitName[Picas[1]] = () => "pc".tt
  erased given picasPerMetre: Ratio[Picas[-1] & Metres[1], 0.004233333333333333] = ###

trait Inches[Power <: Nat] extends Units[Power, Length]
object Inches:
  given UnitName[Inches[1]] = () => "in".tt
  erased given inchesPerMetre: Ratio[Inches[-1] & Metres[1], 0.0254] = ###

trait Feet[Power <: Nat] extends Units[Power, Length]
object Feet:
  given UnitName[Inches[1]] = () => "ft".tt
  erased given feetPerMetre: Ratio[Feet[-1] & Metres[1], 0.3048] = ###

trait Yards[Power <: Nat] extends Units[Power, Length]
object Yards:
  given UnitName[Inches[1]] = () => "yd".tt
  erased given yardsPerMetre: Ratio[Yards[-1] & Metres[1], 0.9144] = ###

trait Miles[Power <: Nat] extends Units[Power, Length]
object Miles:
  given UnitName[Inches[1]] = () => "mi".tt
  erased given milesPerMetre: Ratio[Miles[-1] & Metres[1], 1609.344] = ###

trait Lightyears[Power <: Nat] extends Units[Power, Length]
object Lightyears:
  given UnitName[Inches[1]] = () => "ly".tt
  erased given lightYearsPerMetre: Ratio[Lightyears[1] & Metres[-1], 1.057E-16] = ###

trait NauticalMiles[Power <: Nat] extends Units[Power, Length]
object NauticalMiles:
  given UnitName[Inches[1]] = () => "NM".tt
  erased given nauticalMilesPerMetre: Ratio[NauticalMiles[1] & Metres[-1], 5.399568034557236E-4] =
    ###

trait Furlongs[Power <: Nat] extends Units[Power, Length]
object Furlongs:
  given UnitName[Inches[1]] = () => "fur".tt
  erased given metresPerFurlong: Ratio[Metres[1] & Furlongs[-1], 201.168] = ###

trait Chains[Power <: Nat] extends Units[Power, Length]
object Chains:
  given UnitName[Inches[1]] = () => "ch".tt
  erased given metresPerChain: Ratio[Metres[1] & Chains[-1], 20.1168] = ###

val Inch = Quantity[Inches[1]](1.0)
val Foot = Quantity[Feet[1]](1.0)
val Yard = Quantity[Yards[1]](1.0)
val Mile = Quantity[Miles[1]](1.0)
val Lightyear = Quantity[Lightyears[1]](1.0)
val NauticalMile = Quantity[NauticalMiles[1]](1.0)
val Furlong = Quantity[Furlongs[1]](1.0)
val Chain = Quantity[Chains[1]](1.0)

// Units of Area

val Are = MetricUnit[Metres[2]](100.0)
val Acre = Furlong*Furlong/10.0

// Units of Volume

val Litre = MetricUnit[Metres[3]](0.001)
val FluidOunce = Milli(Litre)*28.4130625
val Pint = Milli(Litre)*568.26125
val Quart = Milli(Litre)*1136.5225
val Gallon = Milli(Litre)*4546.09

// Units of Mass

trait Grains[Power <: Nat] extends Units[Power, Mass]
object Grains:
  given UnitName[Grains[1]] = () => Text("gr")
  erased given kilogramsPerGrain: Ratio[Kilograms[1] & Grains[-1], 0.0000647989] = ###

trait Drams[Power <: Nat] extends Units[Power, Mass]
object Drams:
  given UnitName[Drams[1]] = () => Text("dr")
  erased given kilogramsPerDram: Ratio[Kilograms[1] & Drams[-1], 0.00177184375] = ###

trait Ounces[Power <: Nat] extends Units[Power, Mass]
object Ounces:
  given UnitName[Ounces[1]] = () => Text("oz")
  erased given kilogramsPerOunce: Ratio[Kilograms[1] & Ounces[-1], 0.0283495] = ###

trait Pounds[Power <: Nat] extends Units[Power, Mass]
object Pounds:
  given UnitName[Pounds[1]] = () => Text("lb")
  erased given kilogramsPerPound: Ratio[Kilograms[1] & Pounds[-1], 0.453592] = ###

trait Stones[Power <: Nat] extends Units[Power, Mass]
object Stones:
  given UnitName[Stones[1]] = () => Text("st")
  erased given kilogramsPerStone: Ratio[Kilograms[1] & Stones[-1], 6.35029318] = ###

trait Quarters[Power <: Nat] extends Units[Power, Mass]
object Quarters:
  given UnitName[Quarters[1]] = () => Text("qr")
  erased given kilogramsPerQuarter: Ratio[Kilograms[1] & Quarters[-1], 12.700586360000001] = ###

trait Hundredweights[Power <: Nat] extends Units[Power, Mass]
object Hundredweights:
  given UnitName[Hundredweights[1]] = () => Text("cwt")
  erased given gramsPerHundredweight: Ratio[Kilograms[1] & Hundredweights[-1], 50.80234544] = ###

trait Tons[Power <: Nat] extends Units[Power, Mass]
object Tons:
  erased given gramsPerTon: Ratio[Kilograms[1] & Tons[-1], 1016.0469088] = ###

val Grain = Quantity[Grains[1]](1.0)
val Ounce = Quantity[Ounces[1]](1.0)
val Pound = Quantity[Pounds[1]](1.0)
val Stone = Quantity[Stones[1]](1.0)
val Hundredweight = Quantity[Hundredweights[1]](1.0)
val Ton = Quantity[Tons[1]](1.0)

// Units of Time

val Day = Quantity[Days[1]](1.0)
val Hour = Quantity[Hours[1]](1.0)
val Minute = Quantity[Minutes[1]](1.0)

trait SiderealDays[Power <: Nat] extends Units[Power, Time]
object SiderealDays:
  erased given secondsPerSiderealDay: Ratio[Seconds[1] & SiderealDays[-1], 86164.0905] = ###

trait Days[Power <: Nat] extends Units[Power, Time]
object Days:
  given UnitName[Hours[1]] = () => "d".tt
  erased given secondsPerDay: Ratio[Seconds[1] & Days[-1], 86400.0] = ###

trait Hours[Power <: Nat] extends Units[Power, Time]
object Hours:
  given UnitName[Hours[1]] = () => "h".tt
  erased given secondsPerHour: Ratio[Seconds[1] & Hours[-1], 3600.0] = ###

trait Minutes[Power <: Nat] extends Units[Power, Time]
object Minutes:
  given UnitName[Minutes[1]] = () => "min".tt
  erased given secondsPerMinute: Ratio[Seconds[1] & Minutes[-1], 60.0] = ###

// Units of Angle

trait Degrees[Power <: Nat] extends Units[Power, Angle]
object Degrees:
  given UnitName[Degrees[1]] = () => Text("°")
  erased given degreesPerRadian: Ratio[Degrees[1] & Radians[-1], 57.2957795131] = ###

trait ArcMinutes[Power <: Nat] extends Units[Power, Angle]
object ArcMinutes:
  given UnitName[ArcMinutes[1]] = () => Text("'")
  erased given degreesPerRadian: Ratio[ArcMinutes[1] & Radians[-1], 3437.74677078] = ###

trait ArcSeconds[Power <: Nat] extends Units[Power, Angle]
object ArcSeconds:
  given UnitName[ArcSeconds[1]] = () => Text("\"")
  erased given degreesPerRadian: Ratio[ArcSeconds[1] & Radians[-1], 206264.806247] = ###

trait Celsius[Power <: Nat] extends Units[Power, Temperature]
object Celsius:
  private final val offset = 273.15
  def apply(value: Double): Quantity[Celsius[1]] = Quantity[Celsius[1]](value + offset)
  given UnitsOffset[Celsius[1]] = () => offset
  given UnitName[Celsius[1]] = () => Text("°C")
  erased given celsiusPerKelvin: Ratio[Celsius[1] & Kelvins[-1], 1.0] = ###

trait Fahrenheit[Power <: Nat] extends Units[Power, Temperature]
object Fahrenheit:
  private final val offset = 459.67
  def apply(value: Double): Quantity[Fahrenheit[1]] = Quantity[Fahrenheit[1]](value + offset)
  given UnitsOffset[Fahrenheit[1]] = () => offset
  given UnitName[Fahrenheit[1]] = () => Text("°F")
  erased given fahrenheitPerKelvin: Ratio[Fahrenheit[1] & Kelvins[-1], 1.8] = ###

trait Rankines[Power <: Nat] extends Units[Power, Temperature]
object Rankines:
  given UnitName[Rankines[1]] = () => Text("°R")
  erased given rankinesPerKelvin: Ratio[Rankines[1] & Kelvins[-1], 1.8] = ###
