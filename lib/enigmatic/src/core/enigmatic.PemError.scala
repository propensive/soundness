/*
    Enigmatic, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package enigmatic

import anticipation.*
import fulminate.*

object PemError:
  given Reason is Communicable =
    case Reason.BadBase64    => m"could not parse the BASE-64 PEM message"
    case Reason.BeginMissing => m"the BEGIN line could not be found"
    case Reason.EndMissing   => m"the END line could not be found"
    case Reason.EmptyFile    => m"the file was empty"

  enum Reason:
    case BeginMissing, EndMissing, BadBase64, EmptyFile

case class PemError(reason: PemError.Reason)(using Diagnostics)
extends Error(m"could not parse PEM content because $reason")
