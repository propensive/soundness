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
package syndesis

import anticipation.*
import coaxial.*
import contingency.*
import gigantism.*
import parasite.*
import prepositional.*

// The loans: an advertisement or a browse lives for its block, and ends with it — or with the
// `Monitor`, whose cancellation unwinds the block. `transparent inline`, as `listen` is, so the
// block is not an argument that could hide the capabilities it shares with the monitor; the
// backend is an explicit using-parameter (a system responder's may be a capability), in a
// clause of its own so that one other than the given can be named: `(using other)`. What the
// backend does for the loan is reported as `Discovery.Activity` to the `Loggable` in scope.
extension (service: Discovery.Service)
  transparent inline def advertise[result](description: Discovery.Description)
    ( using backend: Discovery.Backend^ )
    ( block: Discovery.Advertisement ?=> result )
    ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
  :   result =

    val advertising = backend.advertise(service, description)
    val advertisement = Discovery.Advertisement(advertising)
    try block(using advertisement) finally backend.withdraw(advertising)

  transparent inline def browse[result](using backend: Discovery.Backend^)
    ( block: Discovery.Browser ?=> result )
    ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
  :   result =

    val browsing = backend.browse(service)
    val browser = Discovery.Browser(browsing)
    try block(using browser) finally backend.dismiss(browsing)

extension (instance: Discovery.Instance)
  def resolve[duration: Abstractable across Durations to Long](timeout: duration)
    ( using backend: Discovery.Backend^ )
    ( using Monitor^, SharedProbate, Tactic[Discovery.Error], (Discovery.Activity is Loggable)^ )
  :   Discovery.Resolution =

    backend.resolve(instance, timeout)

// The backend selection: the socket-based mDNS responder, over the `Socket.Backend` in scope.
// A single responder per program is the intent, so bind it once (`given backend:
// Discovery.Backend = discoveryBackends.mdnsSockets`) rather than summoning it afresh at each
// use, which would open a socket per summons.
package discoveryBackends:
  given mdnsSockets: (sockets: Socket.Backend, options: Every[Socket.Option.Udp])
  =>  Discovery.Backend =

    val options2 =
      Socket.Option.MulticastLoop ::
        Socket.Option.MulticastHops(255) ::
        Socket.Option.DatagramSize(9000) ::
        options.values.to(List)

    Mdns.Responder: () => Mdns.Transport.sockets(sockets, options2)
