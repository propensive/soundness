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

@implicitNotFound("a contextual GenericInstant instance is required to work with instants in time")
trait GenericInstant:
  type Instant
  def makeInstant(long: Long): Instant
  def readInstant(value: Instant): Long

@implicitNotFound("a contextual GenericDuration instance is required to work with time durations")
trait GenericDuration:
  type Duration
  def makeDuration(long: Long): Duration
  def readDuration(value: Duration): Long

package timekeeping {}

def makeInstant(value: Long)(using time: GenericInstant): time.Instant = time.makeInstant(value)
def readInstant(using time: GenericInstant)(value: time.Instant): Long = time.readInstant(value)
def makeDuration(value: Long)(using time: GenericDuration): time.Duration = time.makeDuration(value)
def readDuration(using time: GenericDuration)(value: time.Duration): Long = time.readDuration(value)
