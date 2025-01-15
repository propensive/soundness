/*
    Guillotine, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import anticipation.*
import rudiments.*
import vacuous.*

trait ProcessRef:
  def pid: Pid
  def kill(): Unit logs ExecEvent
  def abort(): Unit logs ExecEvent
  def alive: Boolean
  def attend(): Unit
  def startTime[InstantType: SpecificInstant]: Optional[InstantType]
  def cpuUsage[DurationType: SpecificDuration]: Optional[DurationType]
