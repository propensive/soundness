/*
    Quantitative, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import rudiments.*

trait Units[PowerType <: Nat, DimensionType <: Dimension] extends Measure

erased trait Metres[Power <: Nat] extends Units[Power, Length]
erased trait Kilograms[Power <: Nat] extends Units[Power, Mass]
erased trait Candelas[Power <: Nat] extends Units[Power, Luminosity]
erased trait Moles[Power <: Nat] extends Units[Power, AmountOfSubstance]
erased trait Amperes[Power <: Nat] extends Units[Power, Current]
erased trait Kelvins[Power <: Nat] extends Units[Power, Temperature]
erased trait Seconds[Power <: Nat] extends Units[Power, Time]

erased trait Radians[Power <: Nat] extends Units[Power, Angle]
