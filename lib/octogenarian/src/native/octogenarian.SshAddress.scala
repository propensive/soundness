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
package octogenarian

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// The address of a repository reached over SSH, in either form git accepts: the URL
// `ssh://[user@]host[:port]/path`, or the scp-like `[user@]host:path`, provided the colon comes
// before any slash (else it is a local path).
object SshAddress:
  def parse(url: Text): Optional[SshAddress] =
    def userHost(text: Text): (Optional[Text], Text) =
      val at = text.s.lastIndexOf('@')
      if at < 0 then (Unset, text) else (text.keep(at), text.skip(at + 1))

    if url.starts(t"ssh://") then
      val rest = url.skip(6)
      val slash = rest.s.indexOf('/')

      if slash < 0 then Unset else
        val authority = rest.keep(slash)
        val path = rest.skip(slash)
        val (user, hostPort) = userHost(authority)
        val colon = hostPort.s.lastIndexOf(':')

        if colon < 0 then SshAddress(user, hostPort, Unset, path)
        else
          safely(hostPort.skip(colon + 1).s.toInt) match
            case port: Int => SshAddress(user, hostPort.keep(colon), port, path)
            case _         => Unset
    else if url.s.contains("://") then
      Unset
    else
      val colon = url.s.indexOf(':')
      val slash = url.s.indexOf('/')

      if colon <= 0 || (slash >= 0 && slash < colon) then Unset
      else
        val (user, host) = userHost(url.keep(colon))
        SshAddress(user, host, Unset, url.skip(colon + 1))

case class SshAddress(user: Optional[Text], host: Text, port: Optional[Int], path: Text)
