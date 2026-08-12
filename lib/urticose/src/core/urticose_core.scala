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
import prepositional.*

export urticose.internal.Ipv6
export urticose.internal.Ipv4Subnet
export urticose.internal.Ipv6Subnet
export urticose.internal.Opaques.Ipv4
export urticose.internal.Opaques.MacAddress
export urticose.internal.Opaques.DnsLabel
export urticose.internal.Opaques.Port

extension (inline context: StringContext)
  transparent inline def ip(): Ipv4 | Ipv6 = ${urticose.internal.ip('context)}

  transparent inline def subnet(): Ipv4Subnet | Ipv6Subnet =
    ${urticose.internal.subnet('context)}

  inline def mac(): MacAddress = ${urticose.internal.mac('context)}
  transparent inline def tcp(): Port = ${urticose.internal.portService('context, true)}
  transparent inline def udp(): Port = ${urticose.internal.portService('context, false)}

extension [remote: Remotable](value: remote)
  infix def on [port](port: port): Endpoint[port] =
    Endpoint(remote.remote(value), port)

extension [port](port: port)
  transparent inline def serve[protocol: Protocolic over port]
    ( handler: (request: protocol.Request) ?=> protocol.Response^{request} )
  :   protocol.Server^ =

    protocol.server(port)(handler)


def internet[result](online: Boolean)(block: Internet ?=> result): result =
  block(using Internet(online))

def online(using internet: Internet): Boolean = internet.online

val Localhost: Hostname = Hostname(DnsLabel("localhost".tt))

type Host = Hostname | Ipv4 | Ipv6

package internetAccess:
  // `inline`: an unparameterized given would be a static field, and a field of capability type
  // would force the enclosing package object to become a capability. Inlining mints a fresh
  // `Online` at each summon site instead.
  inline given online: Online = Online()
  given offline: Tactic[OfflineError] => Online = abort(OfflineError())
