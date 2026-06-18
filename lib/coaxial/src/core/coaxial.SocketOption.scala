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
package coaxial

import vacuous.*

// A socket option, applied to a freshly-constructed socket before it is bound or connected. Each
// option is typed by the connection types it is valid for: the nested marker traits `Tcp`, `Udp`
// and `Domain` (Unix-domain) each extend `SocketOption`, and every concrete option extends every
// connection trait it applies to. A bind/connect site for a particular connection then collects
// `Every[SocketOption.Tcp]` (or `.Udp`/`.Domain`), so only options valid for that connection are
// ever gathered. Flag options that simply enable a feature are parameterless markers — their
// presence is a deviation from the default-off baseline; only options carrying a real value (a
// buffer size, a duration, a linger interval, a traffic class) take a parameter.
object SocketOption:
  sealed trait Tcp    extends SocketOption
  sealed trait Udp    extends SocketOption
  sealed trait Domain extends SocketOption

  case object ReuseAddress                  extends Tcp, Udp, Domain  // SO_REUSEADDR
  case object ReusePort                      extends Tcp, Udp         // SO_REUSEPORT (OS-dependent)
  case class  ReceiveBuffer(bytes: Int)      extends Tcp, Udp, Domain // SO_RCVBUF
  case class  SendBuffer(bytes: Int)         extends Tcp, Udp, Domain // SO_SNDBUF
  case class  Timeout(milliseconds: Int)     extends Tcp, Udp, Domain // SO_TIMEOUT (blocking)

  case object NoDelay                        extends Tcp              // TCP_NODELAY
  case object KeepAlive                      extends Tcp              // SO_KEEPALIVE
  case class  Linger(seconds: Optional[Int]) extends Tcp, Domain      // SO_LINGER
  case class  TrafficClass(value: Int)       extends Tcp, Udp         // IP_TOS

  case object Broadcast                      extends Udp              // SO_BROADCAST

sealed trait SocketOption

// Importable socket-option contributions. Each flag is a named `given` declared at its concrete
// option type (not `SocketOption`), so the per-connection `Every[SocketOption.Tcp]` / `.Udp` /
// `.Domain` searches collect it. Bring one into scope with, e.g.,
// `import socketOptions.noDelaySocketOption`. Options that carry a value are factory methods whose
// result type is the concrete option, so a
// `given SocketOption.ReceiveBuffer = socketOptions.receiveBuffer(65536)` is likewise collected.
package socketOptions:
  given reuseAddressSocketOption: SocketOption.ReuseAddress.type = SocketOption.ReuseAddress
  given reusePortSocketOption:    SocketOption.ReusePort.type    = SocketOption.ReusePort
  given noDelaySocketOption:      SocketOption.NoDelay.type      = SocketOption.NoDelay
  given keepAliveSocketOption:    SocketOption.KeepAlive.type    = SocketOption.KeepAlive
  given broadcastSocketOption:    SocketOption.Broadcast.type    = SocketOption.Broadcast

  def receiveBuffer(bytes: Int): SocketOption.ReceiveBuffer = SocketOption.ReceiveBuffer(bytes)
  def sendBuffer(bytes: Int): SocketOption.SendBuffer = SocketOption.SendBuffer(bytes)
  def linger(seconds: Optional[Int] = Unset): SocketOption.Linger = SocketOption.Linger(seconds)
  def trafficClass(value: Int): SocketOption.TrafficClass = SocketOption.TrafficClass(value)
  def timeout(milliseconds: Int): SocketOption.Timeout = SocketOption.Timeout(milliseconds)
