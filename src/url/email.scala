/*
    Nettlesome, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import gossamer.*
import rudiments.*
import fulminate.*
import perforate.*
import anticipation.*
import contextual.*

case class EmailAddressError(reason: EmailAddressError.Reason)
extends Error(msg"the email address was not valid")

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

import EmailAddressError.Reason.*

object EmailAddress:
  def parse(text: Text): EmailAddress raises EmailAddressError =
    val buffer: StringBuilder = StringBuilder()
    if text.empty then abort(EmailAddressError(Empty))
    
    def quoted(index: Int, escape: Boolean): (LocalPart, Int) =
      safely(text(index)) match
        case '\"' =>
          if escape then
            buffer.append('\"')
            quoted(index + 1, false)
          else
            if safely(text(index + 1)) == '@'
            then (LocalPart.Quoted(buffer.toString.tt), index + 2)
            else abort(EmailAddressError(UnescapedQuote))
        
        case '\\' =>
          if escape then buffer.append('\\')
          quoted(index + 1, !escape)
        
        case char: Char =>
          buffer.append(char)
          quoted(index + 1, false)

        case Unset =>
          raise(EmailAddressError(UnclosedQuote))((LocalPart.Quoted(buffer.toString.tt), index))
    
    def unquoted(index: Int, dot: Boolean): (LocalPart, Int) =
      safely(text(index)) match
        case '@' =>
          if dot then raise(EmailAddressError(TerminalPeriod))(())
          if buffer.length > 64 then raise(EmailAddressError(LongLocalPart))(())
          (LocalPart.Unquoted(buffer.toString.tt), index + 1)

        case '.'  =>
          if dot then raise(EmailAddressError(SuccessivePeriods))(())
          if index == 0 then raise(EmailAddressError(InitialPeriod))(())
          buffer.append('.')
          unquoted(index + 1, true)

        case char: Char =>
          if 'A' <= char <= 'Z' || 'a' <= char <= 'z' || char.isDigit || t"!#$$%&'*+-/=?^_`{|}~".contains(char)
          then buffer.append(char)
          else raise(EmailAddressError(InvalidChar(char)))(())
          unquoted(index + 1, false)

        case Unset =>
          raise(EmailAddressError(MissingAtSymbol))((LocalPart.Unquoted(buffer.toString.tt), index))
    
    val (localPart, index) =
      if text.starts(t"\"") then quoted(1, false) else unquoted(0, false)

    val domain =
      if text.length < index + 1 then abort(EmailAddressError(MissingDomain))
      else if safely(text(index)) == '[' then
        try
          import errorHandlers.throwUnsafely
          if text.last != ']' then abort(EmailAddressError(UnclosedIpAddress))
          val ipAddress = text.slice(index + 1, text.length - 1)
          if ipAddress.starts(t"IPv6:") then Ipv6.parse(ipAddress.drop(5)) else Ipv4.parse(ipAddress)
        catch case error: IpAddressError =>
          abort(EmailAddressError(InvalidDomain(error)))
      else
        try
          import errorHandlers.throwUnsafely
          Hostname.parse(text.drop(index))
        catch case error: HostnameError =>
          abort(EmailAddressError(InvalidDomain(error)))

    EmailAddress(Unset, localPart, domain)

case class EmailAddress(displayName: Maybe[Text], localPart: LocalPart, domain: Hostname | Ipv4 | Ipv6)

enum LocalPart:
  case Quoted(text: Text)
  case Unquoted(text: Text)

