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

import scala.compiletime.*

import anticipation.*
import contextual.*
import fulminate.*
import gossamer.*
import spectacular.*

object ExecEvent:
  given ExecEvent is Communicable =
    case AbortProcess(pid)       => m"The process with PID $pid was aborted"
    case PipelineStart(commands) => m"Started pipeline ${commands.map(_.show).join(t" ")}"
    case KillProcess(pid)        => m"Killed process with PID $pid"
    case ProcessStart(command)   => m"Starting process $command"

enum ExecEvent:
  case ProcessStart(command: Command)
  case AbortProcess(pid: Pid)
  case PipelineStart(commands: Seq[Command])
  case KillProcess(pid: Pid)
