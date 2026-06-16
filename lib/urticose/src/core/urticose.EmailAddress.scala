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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import EmailAddressError.Reason.*

object EmailAddress:
  given decodable: Tactic[EmailAddressError] => EmailAddress is Decodable in Text =
    EmailAddress.parse(_)

  given encodable: EmailAddress is Encodable in Text = _.text
  given showable: EmailAddress is Showable = _.text

  def parse(text: Text): EmailAddress raises EmailAddressError =
    val buffer: StringBuilder = StringBuilder()
    if text.nil then abort(EmailAddressError(Empty))

    def quoted(index: Ordinal, escape: Boolean): (LocalPart, Ordinal) = text.at(index) match
      case '\"' =>
        if escape then
          buffer.append('\"')
          quoted(index + 1, false)
        else
          if text.at(index + 1) == '@'
          then (LocalPart.Quoted(buffer.text), index + 2)
          else abort(EmailAddressError(UnescapedQuote))

      case '\\' =>
        if escape then buffer.append('\\')
        quoted(index + 1, !escape)

      case char: Char =>
        buffer.append(char)
        quoted(index + 1, false)

      case Unset =>
        abort(EmailAddressError(UnclosedQuote))

    def unquoted(index: Ordinal, dot: Boolean): (LocalPart, Ordinal) =
      text.at(index) match
        case '@' =>
          if dot then raise(EmailAddressError(TerminalPeriod))
          if buffer.length > 64 then raise(EmailAddressError(LongLocalPart))

          (LocalPart.Unquoted(buffer.text), index + 1)

        case '.' =>
          if dot then raise(EmailAddressError(SuccessivePeriods))
          if index == Prim then raise(EmailAddressError(InitialPeriod))
          buffer.append('.')
          unquoted(index + 1, true)

        case char: Char =>
          def symbolic: Boolean = t"!#$$%&'*+-/=?^_`{|}~".contains(char)

          if 'A' <= char <= 'Z' || 'a' <= char <= 'z' || char.isDigit || symbolic
          then buffer.append(char)
          else raise(EmailAddressError(InvalidChar(char)))

          unquoted(index + 1, false)

        case Unset =>
          abort(EmailAddressError(MissingAtSymbol))

    val (localPart, index) =
      if text.starts(t"\"") then quoted(Sec, false) else unquoted(Prim, false)

    val domain =
      if index >= text.length.limit then abort(EmailAddressError(MissingDomain))
      else if text.at(index) == '[' then
        try
          if text.ult.let(text.at(_)) != ']' then abort(EmailAddressError(UnclosedIpAddress))
          import strategies.throwUnsafely
          val ipAddress = text.segment(index.next thru text.pen.vouch)

          if ipAddress.starts(t"IPv6:") then ipAddress.skip(5).decode[Ipv6]
          else ipAddress.decode[Ipv4]
        catch case error: IpAddressError => abort(EmailAddressError(InvalidDomain(error)))

      else
        try
          import strategies.throwUnsafely
          text.skip(index.n0).decode[Hostname]
        catch case error: HostnameError =>
          abort(EmailAddressError(InvalidDomain(error)))

    EmailAddress(Unset, localPart, domain)

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

    displayName.lay(address): name =>
      t"$name <$address>"
