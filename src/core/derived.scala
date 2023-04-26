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

val Hertz = (1.0/Second).asSiUnit
val Newton = (Metre*Kilo(Gram)/(Second*Second)).asSiUnit
val Pascal = (Newton/(Metre*Metre)).asSiUnit
val Joule = (Newton*Metre).asSiUnit
val Watt = (Joule/Second).asSiUnit
val Coulomb = (Second*Ampere).asSiUnit
val Volt = (Watt/Ampere).asSiUnit
val Farad = (Coulomb/Volt).asSiUnit
val Ohm = (Volt/Ampere).asSiUnit
val Siemens = (Ampere/Volt).asSiUnit
val Weber = (Volt*Second).asSiUnit
val Tesla = (Weber/(Metre*Metre)).asSiUnit
val Henry = (Weber/Ampere).asSiUnit
val Lux = (Candela/(Metre*Metre)).asSiUnit
val Becquerel = (1.0/Second).asSiUnit
val Gray = (Joule/Kilo(Gram)).asSiUnit
val Sievert = (Joule/Kilo(Gram)).asSiUnit
val Katal = (Mole/Second).asSiUnit