/*
    Exoskeleton, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import anticipation.*
import fulminate.*

object InstallError:
  object Reason:
    given Reason is Communicable as communicable =
      case Environment =>
        m"it was not possible to get enough information about the install environment"

      case Io =>
        m"an I/O error occurred when trying to write an installation file"

  enum Reason:
    case Environment, Io

case class InstallError(reason: InstallError.Reason)(using Diagnostics)
extends Error(m"the installation failed because $reason")
