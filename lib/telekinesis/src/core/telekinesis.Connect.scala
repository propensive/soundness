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
package telekinesis
import fulminate.*

// Reaching a server in the first place: name resolution, the TCP connection itself, and
// the TLS handshake over it. Distinct from `Http.Error`, which reports a response the
// server did send, and from coaxial's `Socket.Error`, which reports a connection that
// was established and then failed. Note the neighbours: `Http.Connect` is the CONNECT
// method and `Http.Connection` is an established connection; neither is this.
object Connect:
  // ConnectError → Connect.Error
  object Error:
    object Reason:
      object Ssl:
        object Reason:
          given communicable: Reason is Communicable =
            case Handshake =>
              m"the local and remote peer could not negotiate the desired level of security"

            case Key =>
              m"the SSL key was bad"

            case Peer =>
              m"the remote peer's identity could not be verified"

            case Protocol =>
              m"the local or remote implementation of the SSL protocol did not behave as expected"

        enum Reason:
          case Handshake, Key, Peer, Protocol

      given communicable: Reason is Communicable =
        case Dns         => m"the server address could not be resolved by DNS"
        case Refused     => m"the connection was refused"
        case Ssl(reason) => m"the SSL/TLS layer could not be established because $reason"
        case Timeout     => m"the server did not respond within the time limit"
        case Unknown     => m"an unrecognized error occurred"

    enum Reason(val number: Int) extends Clarification:
      case Dns                   extends Reason(1)
      case Refused               extends Reason(2)
      case Ssl(reason: Ssl.Reason) extends Reason(3)
      case Timeout               extends Reason(4)
      case Unknown               extends Reason(5)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(999, reason.number)(m"the TCP connection failed because $reason")
