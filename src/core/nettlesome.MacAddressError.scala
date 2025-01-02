/*
    Nettlesome, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package nettlesome

import anticipation.*
import fulminate.*

object MacAddressError:
  enum Reason:
    case WrongGroupCount(count: Int)
    case WrongGroupLength(group: Int, length: Int)
    case NotHex(group: Int, content: Text)

  object Reason:
    given Reason is Communicable =
      case WrongGroupCount(count) =>
        m"there should be six colon-separated groups, but there were $count"

      case WrongGroupLength(group, length) =>
        m"group $group should be two hex digits, but its length is $length"

      case NotHex(group, content) =>
        m"group $group should be a two-digit hex number, but it is $content"


case class MacAddressError(reason: MacAddressError.Reason)(using Diagnostics)
extends Error(m"the MAC address is not valid because $reason")
