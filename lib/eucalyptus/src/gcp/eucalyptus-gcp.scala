/*
    Eucalyptus, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import fulminate.*
import gossamer.*
import jacinta.*
import prepositional.*
import spectacular.*

import jsonPrinters.minimal

package logFormats:
  given googleCloudPlatform: Message is Inscribable in Text = (event, level, realm, timestamp) =>
    case class GcpLog(severity: Text, message: Text)

    val severity = level match
      case Level.Fine => t"DEBUG"
      case Level.Info => t"INFO"
      case Level.Warn => t"WARNING"
      case Level.Fail => t"ERROR"

    GcpLog(severity, event.text).json.show
