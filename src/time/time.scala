/*
    Anticipation, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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

import annotation.implicitNotFound

@implicitNotFound("a contextual GenericInstant instance is required to work with instants in time, for example,\n"+
                  "    import timeApi.long     // Use Longs to represent instants\n"+
                  "    import timeApi.aviation // Use Aviation types for instants")
trait GenericInstant[InstantType]:
  def makeInstant(long: Long): InstantType
  def readInstant(value: InstantType): Long

@implicitNotFound("a contextual GenericDuration instance is required to work with durations of time, for example,\n"+
                  "    import timeApi.long     // Use Longs for time durations\n"+
                  "    import timeApi.aviation // Use Aviation types for time durations")
trait GenericDuration[DurationType]:
  def makeDuration(long: Long): DurationType
  def readDuration(value: DurationType): Long

package timeApi {}

def makeInstant[InstantType](instant: Long)(using generic: GenericInstant[InstantType]): InstantType =
  generic.makeInstant(instant)

def readInstant[InstantType](using generic: GenericInstant[InstantType])(instant: InstantType): Long =
  generic.readInstant(instant)

def makeDuration[DurationType](duration: Long)(using generic: GenericDuration[DurationType]): DurationType =
  generic.makeDuration(duration)

def readDuration[DurationType](using generic: GenericDuration[DurationType])(duration: DurationType): Long =
  generic.readDuration(duration)
