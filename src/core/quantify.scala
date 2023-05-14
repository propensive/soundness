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

import gossamer.*
import rudiments.*
import spectacular.*

import scala.quoted.*
import scala.compiletime.*

import language.implicitConversions

trait Dimension

erased trait Length extends Dimension
erased trait Mass extends Dimension
erased trait Time extends Dimension
erased trait Current extends Dimension
erased trait Luminosity extends Dimension
erased trait Temperature extends Dimension
erased trait AmountOfSubstance extends Dimension

erased trait DimensionName[DimensionType <: Units[?, ?], LabelType <: Label]()

object DimensionName:
  // base units
  erased given length: DimensionName[Units[1, Length], "length"] = erasedValue
  erased given mass: DimensionName[Units[1, Mass], "mass"] = erasedValue
  erased given time: DimensionName[Units[1, Time], "time"] = erasedValue
  erased given current: DimensionName[Units[1, Current], "current"] = erasedValue
  erased given temperature: DimensionName[Units[1, Temperature], "temperature"] = erasedValue
  erased given luminosity: DimensionName[Units[1, Luminosity], "luminosity"] = erasedValue
  
  // derived units from https://en.wikipedia.org/wiki/List_of_physical_quantities
  type Absement = Units[1, Mass] & Units[1, Time]
  erased given absement: DimensionName[Absement, "absement"] = erasedValue

  type AbsorbedDoseRate = Units[2, Length] & Units[-3, Time]
  erased given absorbedDoseRate: DimensionName[AbsorbedDoseRate, "absorbed dose rate"] = erasedValue

  type Acceleration = Units[1, Length] & Units[-2, Time]
  erased given acceleration: DimensionName[Acceleration, "acceleration"] = erasedValue

  type Action = Units[2, Length] & Units[-1, Time] & Units[1, Mass]
  erased given action: DimensionName[Action, "action"] = erasedValue

  type AngularMomentum = Units[1, Mass] & Units[2, Length] & Units[-1, Time]
  erased given angularMomentum: DimensionName[AngularMomentum, "angular momentum"] = erasedValue

  type Area = Units[2, Length]
  erased given area: DimensionName[Area, "area"] = erasedValue

  type AreaDensity = Units[-2, Length] & Units[1, Mass]
  erased given areaDensity: DimensionName[AreaDensity, "area density"] = erasedValue

  type Capacitance = Units[-2, Length] & Units[-1, Mass] & Units[4, Time] & Units[1, Current]
  erased given capacitance: DimensionName[Capacitance, "capacitance"] = erasedValue

  type CatalyticActivityConcentration = Units[-3, Length] & Units[-1, Time] & Units[1,
      AmountOfSubstance]
  
  erased given catalyticActivityConcentration: DimensionName[CatalyticActivityConcentration,
     "catalytic activity concentration"] = erasedValue

  type CentrifugalForce = Units[1, Mass] & Units[1, Length] & Units[-2, Time]
  erased given centrifugalForce: DimensionName[CentrifugalForce, "centrifugal force"] = erasedValue
  
  type ChemicalPotential = Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1,
      AmountOfSubstance]
  
  erased given chemicalPotential: DimensionName[ChemicalPotential, "chemical potential"] =
    erasedValue
  
  type Crackle = Units[1, Length] & Units[-5, Time]
  erased given crackle: DimensionName[Crackle, "crackle"] = erasedValue
  
  type CurrentDensity = Units[-2, Length] & Units[1, Current]
  erased given currentDensity: DimensionName[CurrentDensity, "current density"] = erasedValue
  
  type DoseEquivalent = Units[2, Length] & Units[-2, Time]
  erased given doseEquivalent: DimensionName[DoseEquivalent, "dose equivalent"] = erasedValue
  
  type DynamicViscosity = Units[-1, Length] & Units[1, Mass] & Units[-1, Time]
  erased given dynamicViscosity: DimensionName[DynamicViscosity, "dynamic viscosity"] = erasedValue
  
  type ElectricCharge = Units[1, Time] & Units[1, Current]
  erased given electricCharge: DimensionName[ElectricCharge, "electric charge"] = erasedValue
  
  type ElectricChargeDensity = Units[-3, Length] & Units[1, Time] & Units[1, Current]
  
  erased given electricChargeDensity: DimensionName[ElectricChargeDensity,
      "electric charge density"] = erasedValue
  
  type ElectricDipoleMoment = Units[1, Length] & Units[1, Time] & Units[1, Current]
  
  erased given electricDipoleMoment: DimensionName[ElectricDipoleMoment, "electric dipole moment"] =
    erasedValue
  
  type ElectricDisplacementField = Units[-2, Length] & Units[1, Time] & Units[1, Current]
  
  erased given electricDisplacementField: DimensionName[ElectricDisplacementField,
      "electric displacement field"] = erasedValue
  
  type ElectricFieldStrength = Units[1, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1,
      Current]
  
  erased given electricFieldStrength: DimensionName[ElectricFieldStrength,
      "electric field strength"] = erasedValue
  
  type ElectricalConductance = Units[-2, Length] & Units[-1, Mass] & Units[3, Time] & Units[2,
      Current]
  
  erased given electricalConductance: DimensionName[ElectricalConductance, "electric conductance"] =
    erasedValue
  
  type ElectricalConductivity = Units[-3, Length] & Units[-1, Mass] & Units[3, Time] & Units[2,
      Current]
  
  erased given electricalConductivity: DimensionName[ElectricalConductivity,
      "electric conductivity"] = erasedValue
  
  type ElectricalPotential = Units[2, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1,
      Current]
  
  erased given electricalPotential: DimensionName[ElectricalPotential, "electric potential"] =
    erasedValue
  
  type ElectricalResistance = Units[2, Length] & Units[1, Mass] & Units[-3, Time] & Units[-2,
      Current]
  
  erased given electricalResistance: DimensionName[ElectricalResistance, "electric resistance"] =
    erasedValue
  
  type ElectricalResistivity = Units[3, Length] & Units[1, Mass] & Units[-3, Time] & Units[-2,
      Current]
  
  erased given electricalResistivity: DimensionName[ElectricalResistivity, "electric resistivity"] =
    erasedValue
  
  type Energy = Units[2, Length] & Units[1, Mass] & Units[-2, Time]
  erased given energy: DimensionName[Energy, "energy"] = erasedValue
  
  type EnergyDensity = Units[-1, Length] & Units[1, Mass] & Units[-2, Time]
  erased given energyDensity: DimensionName[EnergyDensity, "energy density"] = erasedValue
  
  type Entropy = Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1, Temperature]
  erased given entropy: DimensionName[Entropy, "entropy"] = erasedValue
  
  type Force = Units[1, Mass] & Units[1, Length] & Units[-2, Time]
  erased given force: DimensionName[Force, "force"] = erasedValue
  
  type Frequency = Units[-1, Time]
  erased given frequency: DimensionName[Frequency, "frequency"] = erasedValue
  
  // need to continue alphabetically
  erased given substance: DimensionName[Units[1, AmountOfSubstance], "amount of substance"] =
    erasedValue
  
  type Velocity = Units[1, Length] & Units[-1, Time]
  erased given velocity: DimensionName[Velocity, "velocity"] = erasedValue
  
  

sealed trait Measure
trait Units[PowerType <: Nat, DimensionType <: Dimension] extends Measure

erased trait Metres[Power <: Nat] extends Units[Power, Length]
erased trait Kilograms[Power <: Nat] extends Units[Power, Mass]
erased trait Candelas[Power <: Nat] extends Units[Power, Luminosity]
erased trait Moles[Power <: Nat] extends Units[Power, AmountOfSubstance]
erased trait Amperes[Power <: Nat] extends Units[Power, Current]
erased trait Kelvins[Power <: Nat] extends Units[Power, Temperature]
erased trait Seconds[Power <: Nat] extends Units[Power, Time]

trait UnitName[-ValueType]:
  def siPrefix: SiPrefix = NoPrefix
  def name(): Text
  def text: Text = t"${siPrefix.symbol}${name()}"

object UnitName:
  given UnitName[Metres[1]] = () => t"m"
  given UnitName[Candelas[1]] = () => t"cd"
  given UnitName[Moles[1]] = () => t"mol"
  given UnitName[Amperes[1]] = () => t"A"
  given UnitName[Kelvins[1]] = () => t"K"
  given UnitName[Seconds[1]] = () => t"s"

  given UnitName[Kilograms[1]] with
    override def siPrefix: SiPrefix = Kilo
    def name(): Text = t"g"

trait PrincipalUnit[DimensionType <: Dimension, UnitType[_ <: Nat] <: Measure]()

object PrincipalUnit:
  given length: PrincipalUnit[Length, Metres]()
  given mass: PrincipalUnit[Mass, Kilograms]()
  given time: PrincipalUnit[Time, Seconds]()
  given current: PrincipalUnit[Current, Amperes]()
  given luminosity: PrincipalUnit[Luminosity, Candelas]()
  given temperature: PrincipalUnit[Temperature, Kelvins]()
  given amountOfSubstance: PrincipalUnit[AmountOfSubstance, Moles]()

trait SubstituteUnits[UnitsType <: Measure](val name: Text)

object SubstituteUnits:
  given joules: SubstituteUnits[Kilograms[1] & Metres[2] & Seconds[-2]](t"J")
  given newtons: SubstituteUnits[Kilograms[1] & Metres[1] & Seconds[-2]](t"N")

object QuantifyOpaques:
  opaque type Quantity[UnitsType <: Measure] = Double
  opaque type MetricUnit[UnitsType <: Measure] <: Quantity[UnitsType] = Double

  extension [UnitsType <: Measure](quantity: Quantity[UnitsType])
    def value: Double = quantity

  object MetricUnit:
    def apply[UnitsType <: Measure](value: Double): MetricUnit[UnitsType] = value

    @targetName("makeDerivedUnit")
    def apply[UnitsType <: Measure](value: Quantity[UnitsType]): MetricUnit[UnitsType] = value

  object Quantity:
    erased given [UnitsType <: Measure]: CanEqual[Quantity[UnitsType], Quantity[UnitsType]] =
      compiletime.erasedValue

    def apply[UnitsType <: Measure](value: Double): Quantity[UnitsType] = value
    
    given convertDouble[UnitsType <: Measure]: Conversion[Double, Quantity[UnitsType]] =
      Quantity(_)
    
    given convertInt[UnitsType <: Measure]: Conversion[Int, Quantity[UnitsType]] = int =>
      Quantity(int.toDouble)

    inline given [UnitsType <: Measure](using Decimalizer): Show[Quantity[UnitsType]] =
      new Show[Quantity[UnitsType]]:
        def apply(value: Quantity[UnitsType]): Text = value.render
    
    inline given [UnitsType <: Measure](using Decimalizer): Debug[Quantity[UnitsType]] =
      new Debug[Quantity[UnitsType]]:
        def apply(value: Quantity[UnitsType]): Text = value.render
  
    def renderUnits(units: Map[Text, Int]): Text =
      units.to(List).map: (unit, power) =>
        if power == 1 then unit
        else 
          val exponent: Text =
            power.show.mapChars:
              case '0' => '⁰'
              case '1' => '¹'
              case '2' => '²'
              case '3' => '³'
              case '4' => '⁴'
              case '5' => '⁵'
              case '6' => '⁶'
              case '7' => '⁷'
              case '8' => '⁸'
              case '9' => '⁹'
              case '-' => '¯'
              case _   => ' '
          
          t"$unit$exponent"
      .join(t"·")

export QuantifyOpaques.{Quantity, MetricUnit}

val Metre: MetricUnit[Metres[1]] = MetricUnit(1)
val Gram: MetricUnit[Kilograms[1]] = MetricUnit(0.001)
val Candela: MetricUnit[Candelas[1]] = MetricUnit(1)
val Mole: MetricUnit[Moles[1]] = MetricUnit(1)
val Ampere: MetricUnit[Amperes[1]] = MetricUnit(1)
val Kelvin: MetricUnit[Kelvins[1]] = MetricUnit(1)
val Second: MetricUnit[Seconds[1]] = MetricUnit(1)

extension [UnitsType <: Measure](inline quantity: Quantity[UnitsType])
  @targetName("plus")
  transparent inline def +[UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.add[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("minus")
  transparent inline def -[UnitsType2 <: Measure](quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.add[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  transparent inline def invert: Any = Quantity[Measure](1.0)/quantity

  transparent inline def in[UnitsType2[power <: Nat] <: Units[power, ?]]: Any =
    ${QuantifyMacros.norm[UnitsType, UnitsType2]('quantity)}
  
  @targetName("times2")
  transparent inline def *
      [UnitsType2 <: Measure](@convertible inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, false)}
  
  @targetName("divide2")
  transparent inline def /
      [UnitsType2 <: Measure](@convertible inline quantity2: Quantity[UnitsType2]): Any =
    ${QuantifyMacros.multiply[UnitsType, UnitsType2]('quantity, 'quantity2, true)}

  inline def units: Map[Text, Int] = ${QuantifyMacros.collectUnits[UnitsType]}
  inline def render(using Decimalizer): Text = t"${quantity.value} ${Quantity.renderUnits(units)}"

  inline def dimension: Text = ${QuantifyMacros.describe[UnitsType]}

  @targetName("greaterThan")
  inline def >[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType, UnitsType2]('quantity, 'that, false)}
  
  @targetName("greaterThanOrEqualTo")
  inline def >=[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType, UnitsType2]('quantity, 'that, true)}
  
  @targetName("lessThanOrEqualTo")
  inline def <=[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType2, UnitsType]('that, 'quantity, true)}
  
  @targetName("lessThan")
  inline def <[UnitsType2 <: Measure](that: Quantity[UnitsType2]): Boolean =
    ${QuantifyMacros.greaterThan[UnitsType2, UnitsType]('that, 'quantity, false)}

extension (value: Double)
  @targetName("times")
  def *[UnitsType <: Measure](quantity: Quantity[UnitsType]): Quantity[UnitsType] = quantity*value
  
  @targetName("divide")
  transparent inline def /[UnitsType <: Measure](quantity: Quantity[UnitsType]): Any =
    ((1.0/value)*quantity).invert
  
