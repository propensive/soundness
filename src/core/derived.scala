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

val Hertz = SiUnit(1.0/Second)
val Newton = SiUnit(Metre*Kilo(Gram)/(Second*Second))
val Pascal = SiUnit(Newton/(Metre*Metre))
val Joule = SiUnit(Newton*Metre)
val Watt = SiUnit(Joule/Second)
val Coulomb = SiUnit(Second*Ampere)
val Volt = SiUnit(Watt/Ampere)
val Farad = SiUnit(Coulomb/Volt)
val Ohm = SiUnit(Volt/Ampere)
val Siemens = SiUnit(Ampere/Volt)
val Weber = SiUnit(Volt*Second)
val Tesla = SiUnit(Weber/(Metre*Metre))
val Henry = SiUnit(Weber/Ampere)
val Lux = SiUnit(Candela/(Metre*Metre))
val Becquerel = SiUnit(1.0/Second)
val Gray = SiUnit(Joule/Kilo(Gram))
val Sievert = SiUnit(Joule/Kilo(Gram))
val Katal = SiUnit(Mole/Second)