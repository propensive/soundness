/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import fulminate.*

object TcpError:
  enum Reason:
    case Dns
    case Refused
    case Ssl(reason: Ssl.Reason)
    case Timeout
    case Unknown

  object Reason:
    object Ssl:
      enum Reason:
        case Handshake, Key, Peer, Protocol

      object Reason:
        given Reason is Communicable =
          case Handshake => m"""the local and remote peer could not negotiate the desired level of
                                security"""
          case Key       => m"the SSL key was bad"
          case Peer      => m"the remote peer's identity could not be verified"
          case Protocol  => m"""the local or remote implementation of the SSL protocol did not
                                behave as expected"""

    given Reason is Communicable =
      case Dns         => m"the server address could not be resolved by DNS"
      case Refused     => m"the connection was refused"
      case Ssl(reason) => m"the SSL/TLS layer could not be established because $reason"
      case Timeout     => m"the server did not respond within the time limit"
      case Unknown     => m"an unrecognized error occurred"

case class TcpError(reason: TcpError.Reason)(using Diagnostics)
extends Error(m"the TCP connection failed because $reason")
