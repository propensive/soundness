                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package urticose

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

import EmailAddress.Error.Reason.*

object EmailAddress:
  given decodable: (tactic: Tactic[EmailAddress.Error])
  =>  ((EmailAddress is Decodable in Text)^{tactic}) =
    EmailAddress.parse(_)

  given encodable: EmailAddress is Encodable in Text = _.text
  given showable: EmailAddress is Showable = _.text

  def parse(text: Text): EmailAddress raises EmailAddress.Error =
    val buffer: StringBuilder = StringBuilder()
    if text.nil then abort(EmailAddress.Error(Empty))

    def quoted(index: Ordinal, escape: Boolean): (LocalPart, Ordinal) = text(index) match
      case '\"' =>
        if escape then
          buffer.append('\"')
          quoted(index + 1, false)
        else
          if text(index + 1) == '@'
          then (LocalPart.Quoted(buffer.text), index + 2)
          else abort(EmailAddress.Error(UnescapedQuote))

      case '\\' =>
        if escape then buffer.append('\\')
        quoted(index + 1, !escape)

      case char: Char =>
        buffer.append(char)
        quoted(index + 1, false)

      case _ =>
        abort(EmailAddress.Error(UnclosedQuote))

    def unquoted(index: Ordinal, dot: Boolean): (LocalPart, Ordinal) =
      text(index) match
        case '@' =>
          if dot then raise(EmailAddress.Error(TerminalPeriod))
          if buffer.length > 64 then raise(EmailAddress.Error(LongLocalPart))

          (LocalPart.Unquoted(buffer.text), index + 1)

        case '.' =>
          if dot then raise(EmailAddress.Error(SuccessivePeriods))
          if index == Prim then raise(EmailAddress.Error(InitialPeriod))
          buffer.append('.')
          unquoted(index + 1, true)

        case char: Char =>
          def symbolic: Boolean = t"!#$$%&'*+-/=?^_`{|}~".contains(char)

          if 'A' <= char <= 'Z' || 'a' <= char <= 'z' || char.isDigit || symbolic
          then buffer.append(char)
          else raise(EmailAddress.Error(InvalidChar(char)))

          unquoted(index + 1, false)

        case _ =>
          abort(EmailAddress.Error(MissingAtSymbol))

    val (localPart, index) =
      if text.starts(t"\"") then quoted(Sec, false) else unquoted(Prim, false)

    val domain =
      if index >= text.length.limit then abort(EmailAddress.Error(MissingDomain))
      else if text(index) == '[' then
        try
          if text.ult.let(text(_)) != ']' then abort(EmailAddress.Error(UnclosedIpAddress))
          import strategies.throwUnsafely

          val ipAddress =
            text.pen.lay(abort(EmailAddress.Error(UnclosedIpAddress))): (pen: Ordinal) =>
              text.segment(index.next thru pen)

          if ipAddress.starts(t"IPv6:") then ipAddress.skip(5).as[Ipv6] else ipAddress.as[Ipv4]
        catch case error: IpAddress.Error => abort(EmailAddress.Error(InvalidDomain(error)))

      else
        try
          import strategies.throwUnsafely
          text.skip(index.n0).as[Hostname]
        catch case error: Hostname.Error =>
          abort(EmailAddress.Error(InvalidDomain(error)))

    EmailAddress(Unset, localPart, domain)

  // EmailAddressError → EmailAddress.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
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
        case InvalidChar(char) => m"the local part contains the character $char which is not allowed"

        case InvalidDomain(error) =>
          error match
            case error: IpAddress.Error => m"the domain is not a valid IP address: ${error.message}"
            case error: Hostname.Error  => m"the domain is not a valid hostname: ${error.message}"

    enum Reason(val number: Int) extends Clarification:
      case Empty                                              extends Reason(1)
      case InvalidDomain(error: IpAddress.Error | Hostname.Error) extends Reason(2)
      case LongLocalPart                                      extends Reason(3)
      case TerminalPeriod                                     extends Reason(4)
      case SuccessivePeriods                                  extends Reason(5)
      case InitialPeriod                                      extends Reason(6)
      case UnescapedQuote                                     extends Reason(7)
      case UnclosedQuote                                      extends Reason(8)
      case MissingDomain                                      extends Reason(9)
      case MissingAtSymbol                                    extends Reason(10)
      case UnclosedIpAddress                                  extends Reason(11)
      case InvalidChar(char: Char)                            extends Reason(12)

  case class Error(reason: EmailAddress.Error.Reason)(using Diagnostics)
  extends fulminate.Error(159, reason.number)(m"the email address is not valid because $reason")

case class EmailAddress
  ( displayName: Optional[Text], localPart: LocalPart, domain: Hostname | Ipv4 | Ipv6 ):

  def text: Text =
    val local = localPart match
      case LocalPart.Quoted(quoted)     => t"\"$quoted\""
      case LocalPart.Unquoted(unquoted) => unquoted

    val remote = domain.absolve match
      case host: Hostname          => host.show
      case ipv4: (Ipv4 @unchecked) => ipv4.show
      case ipv6: Ipv6              => ipv6.show

    val address = t"$local@$remote"

    displayName.lay(address): name => t"$name <$address>"
