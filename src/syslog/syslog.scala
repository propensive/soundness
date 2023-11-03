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
import turbulence.*
import perforate.*
import hieroglyph.*, charEncoders.utf8

case class Syslog(tag: Text)

object Syslog:

  given (using Monitor): Appendable[Syslog, Text] = (syslog, stream) =>
    safely:
      Async:
        import workingDirectories.default
        given Log = SilentLog
        val process: Process["logger", Unit] = sh"logger --tag ${syslog.tag}".fork[Unit]()
        process.stdin(stream)

package logging:
  given syslog(using realm: Realm, monitor: Monitor): Log = Log:
    case _ => Syslog(realm.name)