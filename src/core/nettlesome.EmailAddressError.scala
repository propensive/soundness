/*
    Nettlesome, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import fulminate.*
import anticipation.*

import scala.compiletime.*

case class EmailAddressError(reason: EmailAddressError.Reason)
extends Error(msg"the email address is not valid because $reason")

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
      case Empty             => msg"it is empty"
      case LongLocalPart     => msg"the local part is more than 64 characters long"
      case TerminalPeriod    => msg"the local part ends in a period, which is not allowed"
      case SuccessivePeriods => msg"the local part contains two adjacent periods"
      case UnclosedQuote     => msg"the quoted local part has no closing quote"
      case MissingDomain     => msg"the domain is missing"
      case MissingAtSymbol   => msg"the at-symbol is missing"
      case InitialPeriod     => msg"the local part starts with a period, which is not allowed"
      case UnclosedIpAddress => msg"the domain begins with ${'['} but does not end with ${']'}"
      case UnescapedQuote    => msg"the local part contains a quote character which is not escaped"

      case InvalidChar(char) =>
        msg"the local part contains the character $char which is not allowed"

      case InvalidDomain(error) => error match
        case error: IpAddressError => msg"the domain is not a valid IP address: ${error.message}"
        case error: HostnameError  => msg"the domain is not a valid hostname: ${error.message}"
