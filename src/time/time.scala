/*
    Anticipation, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

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

@implicitNotFound("an implicit Timekeeper instance is required to determine what type should be used to represent instants in time")
trait Timekeeper:
  type Type
  def from(long: Long): Type
  def to(value: Type): Long

object timekeeping:
  given long: Timekeeper with
    type Type = Long
    def from(long: Long): Long = long
    def to(value: Long): Long = value

  given date: Timekeeper with
    type Type = java.util.Date
    def from(long: Long): java.util.Date = java.util.Date(long)
    def to(value: java.util.Date): Long = value.getTime

def now()(using time: Timekeeper): time.Type = time.from(System.currentTimeMillis)
