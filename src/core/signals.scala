/*
    Profanity, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package profanity

import anticipation.*
import gossamer.*
import guillotine.*
import spectacular.*
import perforate.*
import rudiments.*
import eucalyptus.*, logging.silent

object Signal:
  given decoder: Decoder[Signal] = text => Signal.valueOf(text.lower.capitalize.s)
  given encoder: Encoder[Signal] = _.shortName

enum Signal:
  case Hup, Int, Quit, Ill, Trap, Abrt, Bus, Fpe, Kill, Usr1, Segv, Usr2, Pipe, Alrm, Term, Chld, Cont, Stop,
      Tstp, Ttin, Ttou, Urg, Xcpu, Xfsz, Vtalrm, Prof, Winch, Io, Pwr, Sys
  
  def shortName: Text = this.toString.show.upper
  def name: Text = t"SIG${this.toString.show.upper}"
  def id: Int = if ordinal < 15 then ordinal - 1 else ordinal

  def sendTo(process: OsProcess)(using WorkingDirectory, Raises[ExecError]): Unit =
    sh"kill -${shortName} ${process.pid.value}"()
