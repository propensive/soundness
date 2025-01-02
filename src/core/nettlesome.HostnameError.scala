/*
    Nettlesome, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

object HostnameError:
  enum Reason:
    case LongDnsLabel(label: Text)
    case LongHostname
    case InvalidChar(char: Char)
    case EmptyDnsLabel(n: Int)
    case InitialDash(label: Text)

  object Reason:
    given Reason is Communicable =
      case LongDnsLabel(label) => m"the DNS label $label is longer than 63 characters"
      case LongHostname        => m"the hostname is longer than 253 characters"
      case InvalidChar(char)   => m"the character $char is not allowed in a hostname"
      case EmptyDnsLabel(n)    => m"a DNS label cannot be empty"
      case InitialDash(label)  => m"the DNS label $label begins with a dash which is not allowed"

case class HostnameError(text: Text, reason: HostnameError.Reason)(using Diagnostics)
extends Error(m"the hostname is not valid because $reason")
