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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import language.implicitConversions

import anticipation.*
import proscenium.*
import rudiments.*

sealed trait Amount[dimension <: Measure, label <: Label]()

object Amount:
  // base units
  inline given distance: Amount[Units[1, Distance], "distance"] = !!
  inline given mass: Amount[Units[1, Mass], "mass"] = !!
  inline given time: Amount[Units[1, Time], "time"] = !!
  inline given current: Amount[Units[1, Current], "current"] = !!
  inline given heat: Amount[Units[1, Heat], "temperature"] = !!
  inline given luminosity: Amount[Units[1, Luminosity], "luminosity"] = !!

  inline def apply[units <: Measure]: Text = ${Quantitative.amount[units]}

  // derived units from https://en.wikipedia.org/wiki/List_of_physical_quantities

  type ElectricalConductivity =
    Units[-3, Distance] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]

  type Permittivity = Units[-3, Distance] & Units[-1, Mass] & Units[4, Time] & Units[2, Current]
  type ReactionRate = Units[-3, Distance] & Units[-1, Time] & Units[1, AmountOfSubstance]
  type MolarConcentration = Units[-3, Distance] & Units[1, AmountOfSubstance]
  type ElectricChargeDensity = Units[-3, Distance] & Units[1, Time] & Units[1, Current]
  type MassDensity = Units[-3, Distance] & Units[1, Mass]
  type Reluctance = Units[-2, Distance] & Units[-1, Mass] & Units[2, Time] & Units[2, Current]

  type ElectricalConductance =
    Units[-2, Distance] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]

  type ThermalResistance =
    Units[-2, Distance] & Units[-1, Mass] & Units[3, Time] & Units[1, Heat]

  type Capacitance = Units[-2, Distance] & Units[-1, Mass] & Units[4, Time] & Units[1, Current]
  type CurrentDensity = Units[-2, Distance] & Units[1, Current]
  type ElectricDisplacementField = Units[-2, Distance] & Units[1, Time] & Units[1, Current]
  type Illuminance = Units[-2, Distance] & Units[1, Luminosity]
  type AreaDensity = Units[-2, Distance] & Units[1, Mass]

  type ThermalResistivity =
    Units[-1, Distance] & Units[-1, Mass] & Units[3, Time] & Units[1, Heat]

  type Magnetization = Units[-1, Distance] & Units[1, Current]
  type OpticalPower = Units[-1, Distance]
  type TempratureGradient = Units[-1, Distance] & Units[1, Heat]
  type Pressure = Units[-1, Distance] & Units[1, Mass] & Units[-2, Time]
  type DynamicViscosity = Units[-1, Distance] & Units[1, Mass] & Units[-1, Time]
  type LinearDensity = Units[-1, Distance] & Units[1, Mass]
  type Frequency = Units[-1, Time]
  type ElectricCharge = Units[1, Time] & Units[1, Current]
  type Radiance = Units[1, Mass] & Units[-3, Time]
  type MagneticFluxDensity = Units[1, Mass] & Units[-2, Time] & Units[-1, Current]
  type SurfaceTension = Units[1, Mass] & Units[-2, Time]
  type Absement = Units[1, Mass] & Units[1, Time]
  type Pop = Units[1, Distance] & Units[-6, Time]
  type Crackle = Units[1, Distance] & Units[-5, Time]
  type Jounce = Units[1, Distance] & Units[-4, Time]
  type Jerk = Units[1, Distance] & Units[-3, Time]
  type Acceleration = Units[1, Distance] & Units[-2, Time]
  type Velocity = Units[1, Distance] & Units[-1, Time]
  type ElectricDipoleMoment = Units[1, Distance] & Units[1, Time] & Units[1, Current]

  type ElectricFieldStrength =
    Units[1, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Current]

  type ThermalConductivity =
    Units[1, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Heat]

  type Permeability = Units[1, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type Force = Units[1, Distance] & Units[1, Mass] & Units[-2, Time]
  type Momentum = Units[1, Distance] & Units[1, Mass] & Units[-1, Time]
  type AbsorbedDoseRate = Units[2, Distance] & Units[-3, Time]
  type SpecificHeatCapacity = Units[2, Distance] & Units[-2, Time] & Units[-1, Heat]
  type SpecificEnergy = Units[2, Distance] & Units[-2, Time]
  type Area = Units[2, Distance]
  type MagneticMoment = Units[2, Distance] & Units[1, Current]
  type Impedance = Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-2, Current]

  type ElectricalPotential =
    Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Current]

  type ThermalConductance =
    Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Heat]

  type Power = Units[2, Distance] & Units[1, Mass] & Units[-3, Time]
  type Inductance = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type MagneticFlux = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Current]
  type Entropy = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Heat]

  type MolarEntropy =
    Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Heat] & Units[-1,
        AmountOfSubstance]

  type ChemicalPotential =
    Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, AmountOfSubstance]

  type Energy = Units[2, Distance] & Units[1, Mass] & Units[-2, Time]
  type Spin = Units[2, Distance] & Units[1, Mass] & Units[-1, Time]
  type MomentOfInertia = Units[2, Distance] & Units[1, Mass]
  type SpecificVolume = Units[3, Distance] & Units[-1, Mass]
  type VolumetricFlowRate = Units[3, Distance] & Units[-1, Time]
  type Volume = Units[3, Distance]

  type ElectricalResistivity =
    Units[3, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-2, Current]

  inline given absement: Amount[Absement, "absement"] = !!
  inline given absorbedDoseRate: Amount[AbsorbedDoseRate, "absorbed dose rate"] = !!
  inline given acceleration: Amount[Acceleration, "acceleration"] = !!
  inline given area: Amount[Area, "area"] = !!
  inline given areaDensity: Amount[AreaDensity, "area density"] = !!
  inline given capacitance: Amount[Capacitance, "capacitance"] = !!
  inline given crackle: Amount[Crackle, "crackle"] = !!
  inline given currentDensity: Amount[CurrentDensity, "current density"] = !!
  inline given dynamicViscosity: Amount[DynamicViscosity, "dynamic viscosity"] = !!
  inline given electricCharge: Amount[ElectricCharge, "electric charge"] = !!
  inline given energy: Amount[Energy, "energy"] = !!
  inline given entropy: Amount[Entropy, "entropy"] = !!
  inline given force: Amount[Force, "force"] = !!
  inline given frequency: Amount[Frequency, "frequency"] = !!
  inline given substance: Amount[Units[1, AmountOfSubstance], "amount of substance"] = !!
  inline given illuminance: Amount[Illuminance, "illuminance"] = !!
  inline given impedance: Amount[Impedance, "impedance"] = !!
  inline given inductance: Amount[Inductance, "inductance"] = !!
  inline given jerk: Amount[Jerk, "jerk"] = !!
  inline given jounce: Amount[Jounce, "jounce"] = !!
  inline given linearDensity: Amount[LinearDensity, "linear density"] = !!
  inline given magneticFlux: Amount[MagneticFlux, "magnetic flux"] = !!
  inline given magneticMoment: Amount[MagneticMoment, "magnetic moment"] = !!
  inline given magnetization: Amount[Magnetization, "magnetization"] = !!
  inline given massDensity: Amount[MassDensity, "mass density"] = !!
  inline given molarConcentration: Amount[MolarConcentration, "molar concentration"] = !!
  inline given chemicalPotential: Amount[ChemicalPotential, "chemical potential"] = !!
  inline given molarEntropy: Amount[MolarEntropy, "molar entropy"] = !!
  inline given momentOfInertia: Amount[MomentOfInertia, "moment of inertia"] = !!
  inline given momentum: Amount[Momentum, "momentum"] = !!
  inline given opticalPower: Amount[OpticalPower, "optical power"] = !!
  inline given permeability: Amount[Permeability, "permeability"] = !!
  inline given permittivity: Amount[Permittivity, "permittivity"] = !!
  inline given power: Amount[Power, "power"] = !!
  inline given pressure: Amount[Pressure, "pressure"] = !!
  inline given pop: Amount[Pop, "pop"] = !!
  inline given radiance: Amount[Radiance, "radiance"] = !!
  inline given reactionRate: Amount[ReactionRate, "reaction rate"] = !!
  inline given reluctance: Amount[Reluctance, "reluctance"] = !!
  inline given specificEnergy: Amount[SpecificEnergy, "specific energy"] = !!
  inline given specificVolume: Amount[SpecificVolume, "specific volume"] = !!
  inline given spin: Amount[Spin, "spin"] = !!
  inline given surfaceTension: Amount[SurfaceTension, "surface tension"] = !!
  inline given thermalConductance: Amount[ThermalConductance, "thermal conductance"] = !!
  inline given thermalResistance: Amount[ThermalResistance, "thermal resistance"] = !!
  inline given thermalResistivity: Amount[ThermalResistivity, "thermal resistivity"] = !!
  inline given velocity: Amount[Velocity, "velocity"] = !!
  inline given volume: Amount[Volume, "volume"] = !!
  inline given electricChargeDensity: Amount[ElectricChargeDensity, "electric charge density"] = !!
  inline given electricDipoleMoment: Amount[ElectricDipoleMoment, "electric dipole moment"] = !!
  inline given electricFieldStrength: Amount[ElectricFieldStrength, "electric field strength"] = !!
  inline given electricalConductance: Amount[ElectricalConductance, "electric conductance"] = !!
  inline given electricalConductivity: Amount[ElectricalConductivity, "electric conductivity"] = !!
  inline given electricalPotential: Amount[ElectricalPotential, "electric potential"] = !!
  inline given electricalResistivity: Amount[ElectricalResistivity, "electric resistivity"] = !!
  inline given magneticFluxDensity: Amount[MagneticFluxDensity, "magnetic flux density"] = !!
  inline given specificHeatCapacity: Amount[SpecificHeatCapacity, "specific heat capacity"] = !!
  inline given thermalConductivity: Amount[ThermalConductivity, "thermal conductivity"] = !!
  inline given volumetricFlowRate: Amount[VolumetricFlowRate, "volumetric flow rate"] = !!


  inline given electricDisplacementField: Amount[ElectricDisplacementField,
      "electric displacement field"] = !!
