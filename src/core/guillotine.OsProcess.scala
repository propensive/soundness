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

import language.experimental.pureFunctions

import anticipation.*
import contingency.*
import prepositional.*
import proscenium.*
import rudiments.*
import vacuous.*

object OsProcess:
  private def allHandles = ProcessHandle.allProcesses.nn.iterator.nn.asScala.to(List)

  def apply(pid: Pid)(using pidError: Tactic[PidError]): OsProcess =
    val handle = ProcessHandle.of(pid.value).nn
    if handle.isPresent then new OsProcess(handle.get.nn) else abort(PidError(pid))

  def all: List[OsProcess] = allHandles.map(new OsProcess(_))
  def roots: List[OsProcess] = allHandles.filter(!_.parent.nn.isPresent).map(new OsProcess(_))
  def apply(): OsProcess = new OsProcess(ProcessHandle.current.nn)

class OsProcess private (java: ProcessHandle) extends ProcessRef:
  def pid: Pid = Pid(java.pid)
  def kill(): Unit logs ExecEvent = java.destroy()
  def abort(): Unit logs ExecEvent = java.destroyForcibly()
  def alive: Boolean = java.isAlive
  def attend(): Unit = java.onExit.nn.get()

  def parent: Optional[OsProcess] =
    val parent = java.parent.nn
    if parent.isPresent then new OsProcess(parent.get.nn) else Unset

  def children: List[OsProcess] =
    java.children.nn.iterator.nn.asScala.map(new OsProcess(_)).to(List)

  def startTime[InstantType: Specializable across Instants from Long]: Optional[InstantType] =
    val instant = java.info.nn.startInstant.nn
    if instant.isPresent then instant.get.nn.toEpochMilli.specialize else Unset

  def cpuUsage[DurationType: SpecificDuration]: Optional[DurationType] =
    val duration = java.info.nn.totalCpuDuration.nn
    if duration.isPresent then SpecificDuration(duration.get.nn.toMillis) else Unset
