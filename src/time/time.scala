/*
    Anticipation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import language.experimental.captureChecking

trait SpecificInstant[InstantType]:
  def instant(millisecondsSinceEpoch: Long): InstantType

trait GenericInstant[InstantType]:
  def millisecondsSinceEpoch(instant: InstantType): Long

trait SpecificDuration[DurationType]:
  def duration(milliseconds: Long): DurationType

trait GenericDuration[DurationType]:
  def milliseconds(duration: DurationType): Long

package timeInterfaces {}

extension [InstantType](instant: InstantType)(using generic: GenericInstant[InstantType])
  def millisecondsSinceEpoch: Long = generic.millisecondsSinceEpoch(instant)

object SpecificInstant:
  def apply[InstantType](using specific: SpecificInstant[InstantType])(millisecondsSinceEpoch: Long): InstantType =
    specific.instant(millisecondsSinceEpoch)
  
  given long: SpecificInstant[Long] = identity(_)

extension [DurationType](duration: DurationType)(using generic: GenericDuration[DurationType])
  def milliseconds: Long = generic.milliseconds(duration)

object SpecificDuration:
  def apply[DurationType](using specific: SpecificDuration[DurationType])(milliseconds: Long): DurationType =
    specific.duration(milliseconds)

  given long: SpecificDuration[Long] = identity(_)

object GenericInstant:
  given long: GenericInstant[Long] = identity(_)

object GenericDuration:
  given long: GenericDuration[Long] = identity(_)
