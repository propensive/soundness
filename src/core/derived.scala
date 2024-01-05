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

import language.experimental.captureChecking

val Hertz = MetricUnit(1.0/Second)
val Newton = MetricUnit(Metre*Kilo(Gram)/(Second*Second))
val Pascal = MetricUnit(Newton/(Metre*Metre))
val Joule = MetricUnit(Newton*Metre)
val Watt = MetricUnit(Joule/Second)
val Coulomb = MetricUnit(Second*Ampere)
val Volt = MetricUnit(Watt/Ampere)
val Farad = MetricUnit(Coulomb/Volt)
val Ohm = MetricUnit(Volt/Ampere)
val Siemens = MetricUnit(Ampere/Volt)
val Weber = MetricUnit(Volt*Second)
val Tesla = MetricUnit(Weber/(Metre*Metre))
val Henry = MetricUnit(Weber/Ampere)
val Lux = MetricUnit(Candela/(Metre*Metre))
val Becquerel = MetricUnit(1.0/Second)
val Gray = MetricUnit(Joule/Kilo(Gram))
val Sievert = MetricUnit(Joule/Kilo(Gram))
val Katal = MetricUnit(Mole/Second)
