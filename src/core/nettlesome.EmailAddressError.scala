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

import scala.compiletime.*

case class EmailAddressError(reason: EmailAddressError.Reason)(using Diagnostics)
extends Error(m"the email address is not valid because $reason")

object EmailAddressError:
  enum Reason:
    case Empty
    case InvalidDomain(error: IpAddressError | HostnameError)
    case LongLocalPart
    case TerminalPeriod
    case SuccessivePeriods
    case InitialPeriod
    case UnescapedQuote
    case UnclosedQuote
    case MissingDomain
    case MissingAtSymbol
    case UnclosedIpAddress
    case InvalidChar(char: Char)

  object Reason:
    given Reason is Communicable =
      case Empty             => m"it is empty"
      case LongLocalPart     => m"the local part is more than 64 characters long"
      case TerminalPeriod    => m"the local part ends in a period, which is not allowed"
      case SuccessivePeriods => m"the local part contains two adjacent periods"
      case UnclosedQuote     => m"the quoted local part has no closing quote"
      case MissingDomain     => m"the domain is missing"
      case MissingAtSymbol   => m"the at-symbol is missing"
      case InitialPeriod     => m"the local part starts with a period, which is not allowed"
      case UnclosedIpAddress => m"the domain begins with ${'['} but does not end with ${']'}"
      case UnescapedQuote    => m"the local part contains a quote character which is not escaped"

      case InvalidChar(char) =>
        m"the local part contains the character $char which is not allowed"

      case InvalidDomain(error) => error match
        case error: IpAddressError => m"the domain is not a valid IP address: ${error.message}"
        case error: HostnameError  => m"the domain is not a valid hostname: ${error.message}"
