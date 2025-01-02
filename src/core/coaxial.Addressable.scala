/*
    Coaxial, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package coaxial

import anticipation.*
import nettlesome.*
import rudiments.*
import vacuous.*

import java.net as jn

import Control.*

object Addressable:
  given udpEndpoint: Addressable[Endpoint[UdpPort]] with
    case class Connection(address: jn.InetAddress, port: Int, socket: jn.DatagramSocket)

    def connect(endpoint: Endpoint[UdpPort]): Connection =
      val address = jn.InetAddress.getByName(endpoint.remote.s).nn
      Connection(address, endpoint.port.number, jn.DatagramSocket())

    def transmit(connection: Connection, input: Bytes): Unit =
      val packet = jn.DatagramPacket(input.mutable(using Unsafe), input.length, connection.address,
          connection.port)

      connection.socket.send(packet)

  given udpPort: Addressable[UdpPort] with
    case class Connection(port: Int, socket: jn.DatagramSocket)

    def connect(port: UdpPort): Connection =
      Connection(port.number, jn.DatagramSocket())

    def transmit(connection: Connection, input: Bytes): Unit =
      val packet = jn.DatagramPacket(input.mutable(using Unsafe), input.length, jn.InetAddress.getLocalHost.nn,
          connection.port)

      connection.socket.send(packet)

trait Addressable[EndpointType]:
  type Connection

  def connect(endpoint: EndpointType): Connection
  def transmit(connection: Connection, input: Bytes): Unit
