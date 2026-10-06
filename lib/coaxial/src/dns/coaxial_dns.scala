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
package coaxial

import java.util.concurrent as juc

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import rudiments.*
import urticose.*
import vacuous.*

import Dns.Error.Reason

extension (endpoint: Endpoint[Udp.Port])
  // One query to a nameserver, one reply, over UDP: the reply must carry the query's ID and
  // its first question, or it is some other exchange's. The wait is bounded by a
  // `Socket.Option.Timeout` in scope, or five seconds. No retransmission in this first cut:
  // a lost datagram is a `Dns.Error(Timeout)`, and the caller decides whether to ask again.
  def query(message: Dns.Message)
    ( using backend: Socket.Backend, options: Every[Socket.Option.Udp] )
  :   Dns.Message raises Dns.Error raises Socket.Error =

    val supplied = options.values.to(List)

    val timed =
      if supplied.exists(_.isInstanceOf[Socket.Option.Timeout]) then supplied
      else Socket.Option.Timeout(5000) :: supplied

    // A timeout is reported in DNS terms; any other socket failure passes through.
    val packet =
      recover:
        case error: Socket.Error =>
          given diagnostics: Diagnostics = error.diagnostics

          if error.reason == Socket.Error.Reason.Timeout then abort(Dns.Error(Reason.Timeout))
          else abort(error)

      . protect:
        backend.exchangeUdp(endpoint, Unset, timed, message.in[Data])

    val reply = packet.data.as[Dns.Message]

    if reply.id != message.id || reply.questions.prim != message.questions.prim
    then abort(Dns.Error(Reason.Mismatch(reply.id)))

    reply

  // The records answering a question for `name`'s `rtype`, through the nameserver, with
  // recursion requested, under a fresh ID.
  def lookup(name: Dns.Name, rtype: Dns.Type = Dns.Type.A)
    ( using backend: Socket.Backend, options: Every[Socket.Option.Udp] )
  :   List[Dns.Record] raises Dns.Error raises Socket.Error =

    val id = juc.ThreadLocalRandom.current.nn.nextInt(0x10000)
    query(Dns.Message.query(id, List(Dns.Question(name, rtype)))).answers
