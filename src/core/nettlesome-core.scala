/*
    Nettlesome, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import contingency.*
import prepositional.*

export Nettlesome.Ipv6
export Nettlesome.Opaques.Ipv4
export Nettlesome.Opaques.MacAddress
export Nettlesome.Opaques.DnsLabel
export Nettlesome.Opaques.TcpPort
export Nettlesome.Opaques.UdpPort

extension (inline context: StringContext)
  transparent inline def ip(): Ipv4 | Ipv6 = ${Nettlesome.ip('context)}
  inline def mac(): MacAddress = ${Nettlesome.mac('context)}
  inline def tcp(): TcpPort = ${Nettlesome.tcpPort('context)}
  inline def udp(): UdpPort = ${Nettlesome.udpPort('context)}

extension [RemoteType: Connectable](value: RemoteType)
  infix def on [PortType](port: PortType): Endpoint[PortType] =
    Endpoint(RemoteType.remote(value), port)

extension [PortType](port: PortType)
  def serve[ProtocolType: Protocolic over PortType]
     (handler: ProtocolType.Request ?=> ProtocolType.Response)
          : ProtocolType.Server =
    ProtocolType.server(port)(handler)

def internet[ResultType](online: Boolean)(block: Internet ?=> ResultType): ResultType =
  block(using Internet(online))

def online(using internet: Internet): Boolean = internet.online

package internetAccess:
  given Online as enabled = Online()
  given (using Tactic[OfflineError]) => Online as disabled = abort(OfflineError())
