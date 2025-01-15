/*
    Ethereal, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package ethereal

import anticipation.*
import fulminate.*
import profanity.*
import rudiments.*

enum DaemonLogEvent:
  case WriteExecutable(location: Text)
  case Shutdown
  case Termination
  case Failure
  case NewCli
  case UnrecognizedMessage
  case ReceivedSignal(signal: Signal)
  case ExitStatusRequest(pid: Pid)
  case CloseConnection(pid: Pid)
  case StderrRequest(pid: Pid)
  case Init(pid: Pid)

object DaemonLogEvent:
  given DaemonLogEvent is Communicable =
    case WriteExecutable(location) => m"Writing executable to $location"
    case Shutdown                  => m"Shutting down"
    case Termination               => m"Terminating client connection"
    case Failure                   => m"A failure occurred"
    case NewCli                    => m"Instantiating a new CLI"
    case UnrecognizedMessage       => m"Unrecognized message"
    case ReceivedSignal(signal)    => m"Received signal $signal"
    case ExitStatusRequest(pid)    => m"Exit status requested from $pid"
    case CloseConnection(pid)      => m"Connection closed from $pid"
    case StderrRequest(pid)        => m"STDERR requested from $pid"
    case Init(pid)                 => m"Initializing $pid"
