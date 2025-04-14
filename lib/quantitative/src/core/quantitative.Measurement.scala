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

import language.implicitConversions

import proscenium.*
import rudiments.*

erased trait Measurement[dimension <: Units[?, ?], label <: Label]()

object Measurement:
  // base units
  erased given distance: Measurement[Units[1, Distance], "distance"] = !!
  erased given mass: Measurement[Units[1, Mass], "mass"] = !!
  erased given time: Measurement[Units[1, Time], "time"] = !!
  erased given current: Measurement[Units[1, Current], "current"] = !!
  erased given temperature: Measurement[Units[1, Temperature], "temperature"] = !!
  erased given luminosity: Measurement[Units[1, Luminosity], "luminosity"] = !!

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
    Units[-2, Distance] & Units[-1, Mass] & Units[3, Time] & Units[1, Temperature]

  type Capacitance = Units[-2, Distance] & Units[-1, Mass] & Units[4, Time] & Units[1, Current]
  type CurrentDensity = Units[-2, Distance] & Units[1, Current]
  type ElectricDisplacementField = Units[-2, Distance] & Units[1, Time] & Units[1, Current]
  type Illuminance = Units[-2, Distance] & Units[1, Luminosity]
  type AreaDensity = Units[-2, Distance] & Units[1, Mass]

  type ThermalResistivity =
    Units[-1, Distance] & Units[-1, Mass] & Units[3, Time] & Units[1, Temperature]

  type Magnetization = Units[-1, Distance] & Units[1, Current]
  type OpticalPower = Units[-1, Distance]
  type TempratureGradient = Units[-1, Distance] & Units[1, Temperature]
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
    Units[1, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Temperature]

  type Permeability = Units[1, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type Force = Units[1, Distance] & Units[1, Mass] & Units[-2, Time]
  type Momentum = Units[1, Distance] & Units[1, Mass] & Units[-1, Time]
  type AbsorbedDoseRate = Units[2, Distance] & Units[-3, Time]
  type SpecificHeatCapacity = Units[2, Distance] & Units[-2, Time] & Units[-1, Temperature]
  type SpecificEnergy = Units[2, Distance] & Units[-2, Time]
  type Area = Units[2, Distance]
  type MagneticMoment = Units[2, Distance] & Units[1, Current]
  type Impedance = Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-2, Current]

  type ElectricalPotential =
    Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Current]

  type ThermalConductance =
    Units[2, Distance] & Units[1, Mass] & Units[-3, Time] & Units[-1, Temperature]

  type Power = Units[2, Distance] & Units[1, Mass] & Units[-3, Time]
  type Inductance = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type MagneticFlux = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Current]
  type Entropy = Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Temperature]

  type MolarEntropy =
    Units[2, Distance] & Units[1, Mass] & Units[-2, Time] & Units[-1, Temperature] & Units[-1,
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

  erased given absement: Measurement[Absement, "absement"] = !!
  erased given absorbedDoseRate: Measurement[AbsorbedDoseRate, "absorbed dose rate"] = !!
  erased given acceleration: Measurement[Acceleration, "acceleration"] = !!
  erased given area: Measurement[Area, "area"] = !!
  erased given areaDensity: Measurement[AreaDensity, "area density"] = !!
  erased given capacitance: Measurement[Capacitance, "capacitance"] = !!
  erased given crackle: Measurement[Crackle, "crackle"] = !!
  erased given currentDensity: Measurement[CurrentDensity, "current density"] = !!
  erased given dynamicViscosity: Measurement[DynamicViscosity, "dynamic viscosity"] = !!
  erased given electricCharge: Measurement[ElectricCharge, "electric charge"] = !!
  erased given energy: Measurement[Energy, "energy"] = !!
  erased given entropy: Measurement[Entropy, "entropy"] = !!
  erased given force: Measurement[Force, "force"] = !!
  erased given frequency: Measurement[Frequency, "frequency"] = !!
  erased given substance: Measurement[Units[1, AmountOfSubstance], "amount of substance"] = !!
  erased given illuminance: Measurement[Illuminance, "illuminance"] = !!
  erased given impedance: Measurement[Impedance, "impedance"] = !!
  erased given inductance: Measurement[Inductance, "inductance"] = !!
  erased given jerk: Measurement[Jerk, "jerk"] = !!
  erased given jounce: Measurement[Jounce, "jounce"] = !!
  erased given linearDensity: Measurement[LinearDensity, "linear density"] = !!
  erased given magneticFlux: Measurement[MagneticFlux, "magnetic flux"] = !!
  erased given magneticMoment: Measurement[MagneticMoment, "magnetic moment"] = !!
  erased given magnetization: Measurement[Magnetization, "magnetization"] = !!
  erased given massDensity: Measurement[MassDensity, "mass density"] = !!
  erased given molarConcentration: Measurement[MolarConcentration, "molar concentration"] = !!
  erased given chemicalPotential: Measurement[ChemicalPotential, "chemical potential"] = !!
  erased given molarEntropy: Measurement[MolarEntropy, "molar entropy"] = !!
  erased given momentOfInertia: Measurement[MomentOfInertia, "moment of inertia"] = !!
  erased given momentum: Measurement[Momentum, "momentum"] = !!
  erased given opticalPower: Measurement[OpticalPower, "optical power"] = !!
  erased given permeability: Measurement[Permeability, "permeability"] = !!
  erased given permittivity: Measurement[Permittivity, "permittivity"] = !!
  erased given power: Measurement[Power, "power"] = !!
  erased given pressure: Measurement[Pressure, "pressure"] = !!
  erased given pop: Measurement[Pop, "pop"] = !!
  erased given radiance: Measurement[Radiance, "radiance"] = !!
  erased given reactionRate: Measurement[ReactionRate, "reaction rate"] = !!
  erased given reluctance: Measurement[Reluctance, "reluctance"] = !!
  erased given specificEnergy: Measurement[SpecificEnergy, "specific energy"] = !!
  erased given specificVolume: Measurement[SpecificVolume, "specific volume"] = !!
  erased given spin: Measurement[Spin, "spin"] = !!
  erased given surfaceTension: Measurement[SurfaceTension, "surface tension"] = !!
  erased given thermalConductance: Measurement[ThermalConductance, "thermal conductance"] = !!
  erased given thermalResistance: Measurement[ThermalResistance, "thermal resistance"] = !!
  erased given thermalResistivity: Measurement[ThermalResistivity, "thermal resistivity"] = !!
  erased given velocity: Measurement[Velocity, "velocity"] = !!
  erased given volume: Measurement[Volume, "volume"] = !!

  erased given electricChargeDensity: Measurement[ElectricChargeDensity,
      "electric charge density"] = !!

  erased given electricDipoleMoment: Measurement[ElectricDipoleMoment,
      "electric dipole moment"] = !!

  erased given electricFieldStrength: Measurement[ElectricFieldStrength,
      "electric field strength"] = !!

  erased given electricalConductance: Measurement[ElectricalConductance,
      "electric conductance"] = !!

  erased given electricalConductivity: Measurement[ElectricalConductivity,
      "electric conductivity"] = !!

  erased given electricalPotential: Measurement[ElectricalPotential, "electric potential"] =
    !!

  erased given electricalResistivity: Measurement[ElectricalResistivity,
      "electric resistivity"] = !!

  erased given magneticFluxDensity: Measurement[MagneticFluxDensity, "magnetic flux density"] =
    !!

  erased given specificHeatCapacity: Measurement[SpecificHeatCapacity,
      "specific heat capacity"] = !!

  erased given thermalConductivity: Measurement[ThermalConductivity, "thermal conductivity"] =
    !!

  erased given volumetricFlowRate: Measurement[VolumetricFlowRate, "volumetric flow rate"] =
    !!

  erased given electricDisplacementField: Measurement[ElectricDisplacementField,
      "electric displacement field"] = !!
