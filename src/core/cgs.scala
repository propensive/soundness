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

val Galileo = MetricUnit(0.01*Metre/(Second*Second))
val Poise = MetricUnit(0.1*Pascal)
val Franklin = MetricUnit(3.34e-10*Coulomb)
val Biot = MetricUnit(10*Ampere)
val Debye = MetricUnit(3.335e-30*Coulomb*Metre)
val Erg = MetricUnit(10e-7*Joule)
val Dyne = MetricUnit(10e-5*Newton)
val Calorie = MetricUnit(4.184*Joule)
val Langley = MetricUnit(41840*Joule/(Metre*Metre))
val Phot = MetricUnit(10e4*Lux)
val Stokes = MetricUnit(10e-4*Metre*Metre/Second)
val Lambert = MetricUnit((10e4/math.Pi)*Candela/(Metre*Metre))
val Emu = MetricUnit(10e-3*Ampere/(Metre*Metre))
val Oersted = MetricUnit(79.577*Ampere/Metre)
val Maxwell = MetricUnit(10e-8*Weber)
val Gauss = MetricUnit(10e-4*Tesla)
val Gilbert = MetricUnit(0.796*Ampere)
val Darcy = MetricUnit(0.987e-12*Metre*Metre)
val Barye = MetricUnit(0.1*Pascal)
val Kayser = MetricUnit(100/Second)
