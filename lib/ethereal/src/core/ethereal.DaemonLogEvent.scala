                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package ethereal

import anticipation.*
import fulminate.*
import guillotine.*
import profanity.*

object DaemonLogEvent:
  given communicable: DaemonLogEvent is Communicable =
    case WriteExecutable(location) => m"writing the executable to $location"
    case Shutdown                  => m"shutting down"
    case Termination               => m"terminating the client connection"
    case IdleTimeout               => m"shutting down after an idle timeout"
    case Failure                   => m"the connection handler failed"
    case NewCli                    => m"instantiating a new CLI"
    case UnrecognizedMessage       => m"received an unrecognized message"

    case ReceivedSignal(signal) => signal match
      case unix: UnixSignal       => m"received signal $unix"
      case windows: WindowsSignal => m"received signal $windows"

    case ExitStatusRequest(pid)    => m"exit status requested from $pid"
    case CloseConnection(pid)      => m"closing the connection from $pid"
    case StderrRequest(pid)        => m"stderr requested from $pid"
    case ControlRequest(pid)       => m"terminal control requested from $pid"
    case Init(pid)                 => m"initialising $pid"

enum DaemonLogEvent:
  case WriteExecutable(location: Text) extends DaemonLogEvent, Log.Filesystem
  case Shutdown extends DaemonLogEvent, Log.Process
  case Termination extends DaemonLogEvent, Log.Network
  case IdleTimeout extends DaemonLogEvent, Log.Scheduler
  case Failure extends DaemonLogEvent, Log.Runtime
  case NewCli extends DaemonLogEvent, Log.Process
  case UnrecognizedMessage extends DaemonLogEvent, Log.Protocol
  case ReceivedSignal(signal: UnixSignal | WindowsSignal) extends DaemonLogEvent, Log.Process
  case ExitStatusRequest(pid: Pid) extends DaemonLogEvent, Log.Process
  case CloseConnection(pid: Pid) extends DaemonLogEvent, Log.Network
  case StderrRequest(pid: Pid) extends DaemonLogEvent, Log.Process
  case ControlRequest(pid: Pid) extends DaemonLogEvent, Log.Process
  case Init(pid: Pid) extends DaemonLogEvent, Log.Process
