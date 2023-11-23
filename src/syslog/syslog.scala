/*
    Eucalyptus, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package eucalyptus

import guillotine.*
import rudiments.*
import parasite.*
import anticipation.*
import gossamer.*
import turbulence.*
import perforate.*
import hieroglyph.*, textWidthCalculation.uniform

//import scala.language.experimental.captureChecking

case class Syslog(tag: Text)

object Syslog:
  given logFormat: LogFormat[Syslog, Text] = entry =>
    val realm: Text = entry.realm.name.fit(8)
    val stack: Text = entry.envelopes.reverse.join(t"", t" ⟩ ", t" ⟩")

    t"[${entry.level}] $realm: $stack ${entry.message}\n"
  
  given (using Monitor): Appendable[Syslog, Text] = (syslog, stream) =>
    safely:
      import workingDirectories.default
      import logging.silent
      stream.appendTo(sh"logger --tag ${syslog.tag}".fork[Unit]())

package logging:
  given syslog(using realm: Realm, monitor: Monitor): Log[Text] = Log.route:
    case _ => Syslog(realm.name)
