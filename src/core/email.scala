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

import gossamer.*
import rudiments.*
import vacuous.*
import fulminate.*
import perforate.*
import anticipation.*
import contextual.*

import scala.quoted.*
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
    given Communicable[Reason] =
      case Empty                                => msg"it is empty"
      case InvalidDomain(error: IpAddressError) => msg"the domain is not a valid IP address: ${error.message}"
      case InvalidDomain(error: HostnameError)  => msg"the domain is not a valid hostname: ${error.message}"
      case LongLocalPart                        => msg"the local part is more than 64 characters long"
      case TerminalPeriod                       => msg"the local part ends in a period, which is not allowed"
      case SuccessivePeriods                    => msg"the local part contains two adjacent periods"
      case InitialPeriod                        => msg"the local part starts with a period, which is not allowed"
      case UnescapedQuote                       => msg"the local part contains a quote character which is not escaped"
      case UnclosedQuote                        => msg"the quoted local part has no closing quote"
      case MissingDomain                        => msg"the domain is missing"
      case MissingAtSymbol                      => msg"the at-symbol is missing"
      case UnclosedIpAddress                    => msg"the domain begins with ${'['} but does not end with ${']'}"
      case InvalidChar(char)                    => msg"the local part contains the character $char which is not allowed"

import EmailAddressError.Reason.*

object EmailAddress:
  
  def expand(context: Expr[StringContext])(using Quotes): Expr[EmailAddress] = failCompilation:
    val text: Text = context.valueOrAbort.parts.head.tt
    val address = EmailAddress.parse(text)
    
    val localPart: Expr[LocalPart] = address.localPart match
      case LocalPart.Quoted(text)   => '{LocalPart.Quoted(${Expr(text)})}
      case LocalPart.Unquoted(text) => '{LocalPart.Unquoted(${Expr(text)})}
    
    (address.domain.asMatchable: @unchecked) match
      case ipv6: Ipv6         => '{EmailAddress(Unset, $localPart, ${Expr(ipv6)})}
      case hostname: Hostname => '{EmailAddress(Unset, $localPart, ${Expr(hostname)})}
      case ipv4: Int          => '{EmailAddress(Unset, $localPart, Ipv4(${Expr(ipv4)}))}

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
            then (LocalPart.Quoted(buffer.text), index + 2)
            else abort(EmailAddressError(UnescapedQuote))
        
        case '\\' =>
          if escape then buffer.append('\\')
          quoted(index + 1, !escape)
        
        case char: Char =>
          buffer.append(char)
          quoted(index + 1, false)

        case Unset =>
          raise(EmailAddressError(UnclosedQuote))((LocalPart.Quoted(buffer.text), index))
    
    def unquoted(index: Int, dot: Boolean): (LocalPart, Int) =
      safely(text(index)) match
        case '@' =>
          if dot then raise(EmailAddressError(TerminalPeriod))(())
          if buffer.length > 64 then raise(EmailAddressError(LongLocalPart))(())
          (LocalPart.Unquoted(buffer.text), index + 1)

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
          raise(EmailAddressError(MissingAtSymbol))((LocalPart.Unquoted(buffer.text), index))
    
    val (localPart, index) =
      if text.starts(t"\"") then quoted(1, false) else unquoted(0, false)

    val domain =
      if text.length < index + 1 then abort(EmailAddressError(MissingDomain))
      else if safely(text(index)) == '[' then
        try
          if text.last != ']' then abort(EmailAddressError(UnclosedIpAddress))
          import errorHandlers.throwUnsafely
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

case class EmailAddress(displayName: Optional[Text], localPart: LocalPart, domain: Hostname | Ipv4 | Ipv6)

enum LocalPart:
  case Quoted(text: Text)
  case Unquoted(text: Text)

