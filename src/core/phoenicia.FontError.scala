/*
    Phoenicia, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package phoenicia

import anticipation.*
import fulminate.*

object FontError:
  enum Reason:
    case MissingTable(tag: TableTag)
    case UnknownFormat
    case MagicNumber

  given Reason is Communicable =
    case Reason.MissingTable(tag) => m"the table ${tag.text} was not found"
    case Reason.UnknownFormat     => m"the table contains data in an unknown format"
    case Reason.MagicNumber       => m"the font did not contain expected check data"

case class FontError(reason: FontError.Reason)(using Diagnostics)
extends Error(m"the font could not be read because $reason")
