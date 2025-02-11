/*
    Quantitative, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import language.experimental.captureChecking
import language.implicitConversions

import proscenium.*
import rudiments.*

erased trait PhysicalQuantity[DimensionType <: Units[?, ?], LabelType <: Label]()

object PhysicalQuantity:
  // base units
  erased given length: PhysicalQuantity[Units[1, Length], "length"] = ###
  erased given mass: PhysicalQuantity[Units[1, Mass], "mass"] = ###
  erased given time: PhysicalQuantity[Units[1, Time], "time"] = ###
  erased given current: PhysicalQuantity[Units[1, Current], "current"] = ###
  erased given temperature: PhysicalQuantity[Units[1, Temperature], "temperature"] = ###
  erased given luminosity: PhysicalQuantity[Units[1, Luminosity], "luminosity"] = ###

  erased given angle: PhysicalQuantity[Units[1, Angle], "angle"] = ###

  // derived units from https://en.wikipedia.org/wiki/List_of_physical_quantities

  type ElectricalConductivity =
    Units[-3, Length] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]

  type Permittivity = Units[-3, Length] & Units[-1, Mass] & Units[4, Time] & Units[2, Current]
  type ReactionRate = Units[-3, Length] & Units[-1, Time] & Units[1, AmountOfSubstance]
  type MolarConcentration = Units[-3, Length] & Units[1, AmountOfSubstance]
  type ElectricChargeDensity = Units[-3, Length] & Units[1, Time] & Units[1, Current]
  type MassDensity = Units[-3, Length] & Units[1, Mass]
  type Reluctance = Units[-2, Length] & Units[-1, Mass] & Units[2, Time] & Units[2, Current]

  type ElectricalConductance =
    Units[-2, Length] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]

  type ThermalResistance =
    Units[-2, Length] & Units[-1, Mass] & Units[3, Time] & Units[1, Temperature]

  type Capacitance = Units[-2, Length] & Units[-1, Mass] & Units[4, Time] & Units[1, Current]
  type CurrentDensity = Units[-2, Length] & Units[1, Current]
  type ElectricDisplacementField = Units[-2, Length] & Units[1, Time] & Units[1, Current]
  type Illuminance = Units[-2, Length] & Units[1, Luminosity]
  type AreaDensity = Units[-2, Length] & Units[1, Mass]

  type ThermalResistivity =
    Units[-1, Length] & Units[-1, Mass] & Units[3, Time] & Units[1, Temperature]

  type Magnetization = Units[-1, Length] & Units[1, Current]
  type OpticalPower = Units[-1, Length]
  type TempratureGradient = Units[-1, Length] & Units[1, Temperature]
  type Pressure = Units[-1, Length] & Units[1, Mass] & Units[-2, Time]
  type DynamicViscosity = Units[-1, Length] & Units[1, Mass] & Units[-1, Time]
  type LinearDensity = Units[-1, Length] & Units[1, Mass]
  type Frequency = Units[-1, Time]
  type ElectricCharge = Units[1, Time] & Units[1, Current]
  type Radiance = Units[1, Mass] & Units[-3, Time]
  type MagneticFluxDensity = Units[1, Mass] & Units[-2, Time] & Units[-1, Current]
  type SurfaceTension = Units[1, Mass] & Units[-2, Time]
  type Absement = Units[1, Mass] & Units[1, Time]
  type Pop = Units[1, Length] & Units[-6, Time]
  type Crackle = Units[1, Length] & Units[-5, Time]
  type Jounce = Units[1, Length] & Units[-4, Time]
  type Jerk = Units[1, Length] & Units[-3, Time]
  type Acceleration = Units[1, Length] & Units[-2, Time]
  type Velocity = Units[1, Length] & Units[-1, Time]
  type ElectricDipoleMoment = Units[1, Length] & Units[1, Time] & Units[1, Current]

  type ElectricFieldStrength =
    Units[1, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1, Current]

  type ThermalConductivity =
    Units[1, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1, Temperature]

  type Permeability = Units[1, Length] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type Force = Units[1, Length] & Units[1, Mass] & Units[-2, Time]
  type Momentum = Units[1, Length] & Units[1, Mass] & Units[-1, Time]
  type AbsorbedDoseRate = Units[2, Length] & Units[-3, Time]
  type SpecificHeatCapacity = Units[2, Length] & Units[-2, Time] & Units[-1, Temperature]
  type SpecificEnergy = Units[2, Length] & Units[-2, Time]
  type Area = Units[2, Length]
  type MagneticMoment = Units[2, Length] & Units[1, Current]
  type Impedance = Units[2, Length] & Units[1, Mass] & Units[-3, Time] & Units[-2, Current]

  type ElectricalPotential =
    Units[2, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1, Current]

  type ThermalConductance =
    Units[2, Length] & Units[1, Mass] & Units[-3, Time] & Units[-1, Temperature]

  type Power = Units[2, Length] & Units[1, Mass] & Units[-3, Time]
  type Inductance = Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-2, Current]
  type MagneticFlux = Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1, Current]
  type Entropy = Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1, Temperature]

  type MolarEntropy =
    Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1, Temperature] & Units[-1,
        AmountOfSubstance]

  type ChemicalPotential =
    Units[2, Length] & Units[1, Mass] & Units[-2, Time] & Units[-1, AmountOfSubstance]

  type Energy = Units[2, Length] & Units[1, Mass] & Units[-2, Time]
  type Spin = Units[2, Length] & Units[1, Mass] & Units[-1, Time]
  type MomentOfInertia = Units[2, Length] & Units[1, Mass]
  type SpecificVolume = Units[3, Length] & Units[-1, Mass]
  type VolumetricFlowRate = Units[3, Length] & Units[-1, Time]
  type Volume = Units[3, Length]

  type ElectricalResistivity =
    Units[3, Length] & Units[1, Mass] & Units[-3, Time] & Units[-2, Current]

  erased given absement: PhysicalQuantity[Absement, "absement"] = ###
  erased given absorbedDoseRate: PhysicalQuantity[AbsorbedDoseRate, "absorbed dose rate"] = ###
  erased given acceleration: PhysicalQuantity[Acceleration, "acceleration"] = ###
  erased given area: PhysicalQuantity[Area, "area"] = ###
  erased given areaDensity: PhysicalQuantity[AreaDensity, "area density"] = ###
  erased given capacitance: PhysicalQuantity[Capacitance, "capacitance"] = ###
  erased given crackle: PhysicalQuantity[Crackle, "crackle"] = ###
  erased given currentDensity: PhysicalQuantity[CurrentDensity, "current density"] = ###
  erased given dynamicViscosity: PhysicalQuantity[DynamicViscosity, "dynamic viscosity"] = ###
  erased given electricCharge: PhysicalQuantity[ElectricCharge, "electric charge"] = ###
  erased given energy: PhysicalQuantity[Energy, "energy"] = ###
  erased given entropy: PhysicalQuantity[Entropy, "entropy"] = ###
  erased given force: PhysicalQuantity[Force, "force"] = ###
  erased given frequency: PhysicalQuantity[Frequency, "frequency"] = ###
  erased given substance: PhysicalQuantity[Units[1, AmountOfSubstance], "amount of substance"] = ###
  erased given illuminance: PhysicalQuantity[Illuminance, "illuminance"] = ###
  erased given impedance: PhysicalQuantity[Impedance, "impedance"] = ###
  erased given inductance: PhysicalQuantity[Inductance, "inductance"] = ###
  erased given jerk: PhysicalQuantity[Jerk, "jerk"] = ###
  erased given jounce: PhysicalQuantity[Jounce, "jounce"] = ###
  erased given linearDensity: PhysicalQuantity[LinearDensity, "linear density"] = ###
  erased given magneticFlux: PhysicalQuantity[MagneticFlux, "magnetic flux"] = ###
  erased given magneticMoment: PhysicalQuantity[MagneticMoment, "magnetic moment"] = ###
  erased given magnetization: PhysicalQuantity[Magnetization, "magnetization"] = ###
  erased given massDensity: PhysicalQuantity[MassDensity, "mass density"] = ###
  erased given molarConcentration: PhysicalQuantity[MolarConcentration, "molar concentration"] = ###
  erased given chemicalPotential: PhysicalQuantity[ChemicalPotential, "chemical potential"] = ###
  erased given molarEntropy: PhysicalQuantity[MolarEntropy, "molar entropy"] = ###
  erased given momentOfInertia: PhysicalQuantity[MomentOfInertia, "moment of inertia"] = ###
  erased given momentum: PhysicalQuantity[Momentum, "momentum"] = ###
  erased given opticalPower: PhysicalQuantity[OpticalPower, "optical power"] = ###
  erased given permeability: PhysicalQuantity[Permeability, "permeability"] = ###
  erased given permittivity: PhysicalQuantity[Permittivity, "permittivity"] = ###
  erased given power: PhysicalQuantity[Power, "power"] = ###
  erased given pressure: PhysicalQuantity[Pressure, "pressure"] = ###
  erased given pop: PhysicalQuantity[Pop, "pop"] = ###
  erased given radiance: PhysicalQuantity[Radiance, "radiance"] = ###
  erased given reactionRate: PhysicalQuantity[ReactionRate, "reaction rate"] = ###
  erased given reluctance: PhysicalQuantity[Reluctance, "reluctance"] = ###
  erased given specificEnergy: PhysicalQuantity[SpecificEnergy, "specific energy"] = ###
  erased given specificVolume: PhysicalQuantity[SpecificVolume, "specific volume"] = ###
  erased given spin: PhysicalQuantity[Spin, "spin"] = ###
  erased given surfaceTension: PhysicalQuantity[SurfaceTension, "surface tension"] = ###
  erased given thermalConductance: PhysicalQuantity[ThermalConductance, "thermal conductance"] = ###
  erased given thermalResistance: PhysicalQuantity[ThermalResistance, "thermal resistance"] = ###
  erased given thermalResistivity: PhysicalQuantity[ThermalResistivity, "thermal resistivity"] = ###
  erased given velocity: PhysicalQuantity[Velocity, "velocity"] = ###
  erased given volume: PhysicalQuantity[Volume, "volume"] = ###

  erased given electricChargeDensity: PhysicalQuantity[ElectricChargeDensity,
      "electric charge density"] = ###

  erased given electricDipoleMoment: PhysicalQuantity[ElectricDipoleMoment,
      "electric dipole moment"] = ###

  erased given electricFieldStrength: PhysicalQuantity[ElectricFieldStrength,
      "electric field strength"] = ###

  erased given electricalConductance: PhysicalQuantity[ElectricalConductance,
      "electric conductance"] = ###

  erased given electricalConductivity: PhysicalQuantity[ElectricalConductivity,
      "electric conductivity"] = ###

  erased given electricalPotential: PhysicalQuantity[ElectricalPotential, "electric potential"] =
    ###

  erased given electricalResistivity: PhysicalQuantity[ElectricalResistivity,
      "electric resistivity"] = ###

  erased given magneticFluxDensity: PhysicalQuantity[MagneticFluxDensity, "magnetic flux density"] =
    ###

  erased given specificHeatCapacity: PhysicalQuantity[SpecificHeatCapacity,
      "specific heat capacity"] = ###

  erased given thermalConductivity: PhysicalQuantity[ThermalConductivity, "thermal conductivity"] =
    ###

  erased given volumetricFlowRate: PhysicalQuantity[VolumetricFlowRate, "volumetric flow rate"] =
    ###

  erased given electricDisplacementField: PhysicalQuantity[ElectricDisplacementField,
      "electric displacement field"] = ###
