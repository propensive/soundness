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
┃    Soundness, version 0.49.0.                                                                    ┃
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

erased trait Amount[dimension <: Measure, label <: Label]()

object Amount:
  // base units
  erased given distance: Amount[Units[1, Distance], "distance"] = !!
  erased given mass: Amount[Units[1, Mass], "mass"] = !!
  erased given time: Amount[Units[1, Time], "time"] = !!
  erased given current: Amount[Units[1, Current], "current"] = !!
  erased given heat: Amount[Units[1, Heat], "temperature"] = !!
  erased given luminosity: Amount[Units[1, Luminosity], "luminosity"] = !!

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

  erased given absement: Amount[Absement, "absement"] = !!
  erased given absorbedDoseRate: Amount[AbsorbedDoseRate, "absorbed dose rate"] = !!
  erased given acceleration: Amount[Acceleration, "acceleration"] = !!
  erased given area: Amount[Area, "area"] = !!
  erased given areaDensity: Amount[AreaDensity, "area density"] = !!
  erased given capacitance: Amount[Capacitance, "capacitance"] = !!
  erased given crackle: Amount[Crackle, "crackle"] = !!
  erased given currentDensity: Amount[CurrentDensity, "current density"] = !!
  erased given dynamicViscosity: Amount[DynamicViscosity, "dynamic viscosity"] = !!
  erased given electricCharge: Amount[ElectricCharge, "electric charge"] = !!
  erased given energy: Amount[Energy, "energy"] = !!
  erased given entropy: Amount[Entropy, "entropy"] = !!
  erased given force: Amount[Force, "force"] = !!
  erased given frequency: Amount[Frequency, "frequency"] = !!
  erased given substance: Amount[Units[1, AmountOfSubstance], "amount of substance"] = !!
  erased given illuminance: Amount[Illuminance, "illuminance"] = !!
  erased given impedance: Amount[Impedance, "impedance"] = !!
  erased given inductance: Amount[Inductance, "inductance"] = !!
  erased given jerk: Amount[Jerk, "jerk"] = !!
  erased given jounce: Amount[Jounce, "jounce"] = !!
  erased given linearDensity: Amount[LinearDensity, "linear density"] = !!
  erased given magneticFlux: Amount[MagneticFlux, "magnetic flux"] = !!
  erased given magneticMoment: Amount[MagneticMoment, "magnetic moment"] = !!
  erased given magnetization: Amount[Magnetization, "magnetization"] = !!
  erased given massDensity: Amount[MassDensity, "mass density"] = !!
  erased given molarConcentration: Amount[MolarConcentration, "molar concentration"] = !!
  erased given chemicalPotential: Amount[ChemicalPotential, "chemical potential"] = !!
  erased given molarEntropy: Amount[MolarEntropy, "molar entropy"] = !!
  erased given momentOfInertia: Amount[MomentOfInertia, "moment of inertia"] = !!
  erased given momentum: Amount[Momentum, "momentum"] = !!
  erased given opticalPower: Amount[OpticalPower, "optical power"] = !!
  erased given permeability: Amount[Permeability, "permeability"] = !!
  erased given permittivity: Amount[Permittivity, "permittivity"] = !!
  erased given power: Amount[Power, "power"] = !!
  erased given pressure: Amount[Pressure, "pressure"] = !!
  erased given pop: Amount[Pop, "pop"] = !!
  erased given radiance: Amount[Radiance, "radiance"] = !!
  erased given reactionRate: Amount[ReactionRate, "reaction rate"] = !!
  erased given reluctance: Amount[Reluctance, "reluctance"] = !!
  erased given specificEnergy: Amount[SpecificEnergy, "specific energy"] = !!
  erased given specificVolume: Amount[SpecificVolume, "specific volume"] = !!
  erased given spin: Amount[Spin, "spin"] = !!
  erased given surfaceTension: Amount[SurfaceTension, "surface tension"] = !!
  erased given thermalConductance: Amount[ThermalConductance, "thermal conductance"] = !!
  erased given thermalResistance: Amount[ThermalResistance, "thermal resistance"] = !!
  erased given thermalResistivity: Amount[ThermalResistivity, "thermal resistivity"] = !!
  erased given velocity: Amount[Velocity, "velocity"] = !!
  erased given volume: Amount[Volume, "volume"] = !!
  erased given electricChargeDensity: Amount[ElectricChargeDensity, "electric charge density"] = !!
  erased given electricDipoleMoment: Amount[ElectricDipoleMoment, "electric dipole moment"] = !!
  erased given electricFieldStrength: Amount[ElectricFieldStrength, "electric field strength"] = !!
  erased given electricalConductance: Amount[ElectricalConductance, "electric conductance"] = !!
  erased given electricalConductivity: Amount[ElectricalConductivity, "electric conductivity"] = !!
  erased given electricalPotential: Amount[ElectricalPotential, "electric potential"] = !!
  erased given electricalResistivity: Amount[ElectricalResistivity, "electric resistivity"] = !!
  erased given magneticFluxDensity: Amount[MagneticFluxDensity, "magnetic flux density"] = !!
  erased given specificHeatCapacity: Amount[SpecificHeatCapacity, "specific heat capacity"] = !!
  erased given thermalConductivity: Amount[ThermalConductivity, "thermal conductivity"] = !!
  erased given volumetricFlowRate: Amount[VolumetricFlowRate, "volumetric flow rate"] = !!

  erased given electricDisplacementField: Amount[ElectricDisplacementField,
      "electric displacement field"] = !!
