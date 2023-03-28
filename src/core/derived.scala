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

val Hertz = Quantity[Second[-1]](1.0)
val Newton = Metre*Kilo(Gram)/(Second*Second)
val Pascal = Newton/(Metre*Metre)
val Joule = Newton*Metre
val Watt = Joule/Second
val Coulomb = Second*Ampere
val Volt = Watt/Ampere
val Farad = Coulomb/Volt
val Ohm = Volt/Ampere
val Siemens = Ampere/Volt
val Weber = Volt*Second
val Tesla = Weber/(Metre*Metre)
val Henry = Weber/Ampere
val Lux = Candela/(Metre*Metre)
val Becquerel = Quantity[Second[-1]](1.0)
val Gray = Joule/Kilo(Gram)
val Sievert = Joule/Kilo(Gram)
val Katal = Mole/Second