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

// The `transmit`, `listen`, `exchange` and `duplex` extensions are defined in this
// package, so they are already in scope; re-importing them through `soundness`
// would make the two same-shaped `transmit` overloads (from `Serviceable` and
// `Routable`) ambiguous, so they are excluded from the wildcard.
import soundness.{transmit as _, listen as _, exchange as _, duplex as _, *}

import charEncoders.utf8
import charDecoders.utf8
import textSanitizers.skip
import errorDiagnostics.stackTraces
import threading.platform
import codicils.await

import Control.*

object Tests extends Suite(m"Coaxial tests"):
  def run(): Unit = unsafely:

    // A `Data` (`IArray[Byte]`) compares by reference, so byte-level assertions
    // go through `List[Byte]`.
    def ascii(text: Text): Data = IArray.from(text.s.getBytes("US-ASCII").nn.to(List))
    def bytes(data: Data): List[Byte] = data.to(List)
    def joined(stream: Stream[Data]): List[Byte] = stream.to(List).flatMap(_.to(List))

    suite(m"Transmissible serialization"):
      test(m"Data is transmitted as a single chunk"):
        joined(summon[Data is Transmissible].serialize(ascii(t"abc")))
      . assert(_ == bytes(ascii(t"abc")))

      test(m"A Stream[Data] is transmitted unchanged"):
        val stream = Stream(ascii(t"ab"), ascii(t"cd"))
        joined(summon[Stream[Data] is Transmissible].serialize(stream))
      . assert(_ == bytes(ascii(t"abcd")))

      test(m"Text is transmitted via its character encoding"):
        joined(summon[Text is Transmissible].serialize(t"hello"))
      . assert(_ == bytes(ascii(t"hello")))

      test(m"An Encodable value is transmitted via its Text encoding"):
        joined(summon[Port is Transmissible].serialize(Port.unsafe[Tcp](8080)))
      . assert(_ == bytes(ascii(t"8080")))

      test(m"contramap adapts a Transmissible to a new source type"):
        val ints: Int is Transmissible = summon[Text is Transmissible].contramap[Int](_.show)
        joined(ints.serialize(42))
      . assert(_ == bytes(ascii(t"42")))

    suite(m"Ingressive deserialization"):
      test(m"Data is received as identity"):
        bytes(Ingressive.bytes.deserialize(ascii(t"xyz")))
      . assert(_ == bytes(ascii(t"xyz")))

      test(m"Text is received via its character encoding"):
        Ingressive.text.deserialize(ascii(t"hello"))
      . assert(_ == t"hello")

      test(m"A Decodable value is received via its Text decoding"):
        Ingressive.decoder[Port].deserialize(ascii(t"443")).number
      . assert(_ == 443)

      test(m"map adapts an Ingressive to a new result type"):
        val lengths: Int is Ingressive = Ingressive.text.map[Int](_.length)
        lengths.deserialize(ascii(t"hello"))
      . assert(_ == 5)

    suite(m"Control state machine"):
      test(m"Terminate is a Control value"):
        (Terminate: Control[Int])
      . assert(_ == Terminate)

      test(m"Continue carries an optional state"):
        Continue[Int](7).pipe { case Continue(state) => state.or(0) }
      . assert(_ == 7)

      test(m"Continue without an argument leaves the state Unset"):
        Continue[Int]().pipe { case Continue(state) => state.absent }
      . assert(_ == true)

      test(m"Reply from Data stores the message bytes verbatim"):
        bytes(Reply(ascii(t"hi"), 3).message)
      . assert(_ == bytes(ascii(t"hi")))

      test(m"Conclude from Data stores the message bytes verbatim"):
        bytes(Conclude(ascii(t"bye"), 3).message)
      . assert(_ == bytes(ascii(t"bye")))

      test(m"Continue and Reply are Interactive"):
        (Continue(1).isInstanceOf[Interactive], Reply(ascii(t"x"), 1).isInstanceOf[Interactive])
      . assert(_ == (true, true))

      test(m"Conclude is not Interactive"):
        Conclude(ascii(t"x"), 1).isInstanceOf[Interactive]
      . assert(_ == false)

      test(m"Reply serializes a Transmissible (Text) message"):
        bytes(Reply(t"hi", 1).message)
      . assert(_ == bytes(ascii(t"hi")))

      test(m"Conclude serializes a Transmissible (Text) message"):
        bytes(Conclude(t"bye", 1).message)
      . assert(_ == bytes(ascii(t"bye")))

    suite(m"UdpResponse"):
      test(m"Ignore is a distinct response"):
        (UdpResponse.Ignore: UdpResponse)
      . assert(_ == UdpResponse.Ignore)

      test(m"Reply carries its payload"):
        UdpResponse.Reply(ascii(t"pong")) match
          case UdpResponse.Reply(data) => bytes(data)
          case UdpResponse.Ignore      => Nil
      . assert(_ == bytes(ascii(t"pong")))

    suite(m"Packet"):
      test(m"A Packet exposes its data, sender and port"):
        val packet = Packet(ascii(t"payload"), ip"192.168.0.1", Port.unsafe[Udp](9999))
        (bytes(packet.data), packet.sender, packet.port.number)
      . assert(_ == (bytes(ascii(t"payload")), ip"192.168.0.1", 9999))

    suite(m"DomainSocket"):
      test(m"A DomainSocket endpoint pairs a socket with a path"):
        val socket = DomainSocket(t"/var/run/docker.sock")
        socket.at(t"/info") == DomainSocketEndpoint(socket, t"/info")
      . assert(_ == true)

    suite(m"BindError"):
      test(m"PortInUse has a descriptive message"):
        BindError.Reason.PortInUse.communicate.text
      . assert(_ == t"another process is already bound to the port")

      test(m"PermissionDenied has a descriptive message"):
        BindError.Reason.PermissionDenied.communicate.text
      . assert(_ == t"the user does not have permission to bind the port")

      test(m"AddressUnavailable has a descriptive message"):
        BindError.Reason.AddressUnavailable.communicate.text
      . assert(_ == t"the requested address is not available on this host")

      test(m"A BindError incorporates its reason"):
        BindError(BindError.Reason.PortInUse).message.text
      . assert(_ == t"the socket could not be bound because another process is already "+
          t"bound to the port")

    supervise:
      suite(m"UDP server and client"):
        test(m"A bound UDP server receives a transmitted datagram"):
          val port = Port[Udp]()
          val received: Promise[Text] = Promise()

          val server = port.listen[Data]: packet =>
            received.fulfill(packet.data.utf8)
            UdpResponse.Reply(ascii(t"ack"))

          // The `transmit` extension is overloaded on `Serviceable` (returns
          // `Stream[Data]`) and `Routable` (returns `Unit`); since value-discarding
          // makes any type conform to `Unit`, the `Routable` overload is not
          // reachable by ascription, so its given is exercised directly here.
          val routable = summon[UdpPort is Routable]
          routable.transmit(routable.connect(port), Stream(ascii(t"ping")))
          received.await().also(server.stop())
        . assert(_ == t"ping")

      suite(m"TCP server and client"):
        test(m"A client exchange receives the server's pushed message"):
          val port = Port[Tcp]()
          val server = port.listen[Data](socket => ascii(t"greeting"))

          val received: Data = port.exchange(Data())[Data](Data()): message =>
            Conclude(ascii(t""), message)

          bytes(received).also(server.stop())
        . assert(_ == bytes(ascii(t"greeting")))

      suite(m"Unix domain socket server and client"):
        test(m"A server receives the bytes a client transmits"):
          val path = t"/tmp/coaxial-request-response.sock"
          java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(path.s))
          val socket = DomainSocket(path)
          val received: Promise[Text] = Promise()

          val server = socket.listen[Data]: connection =>
            val request = IArray.from(joined(connection.stream()))
            received.fulfill(request.utf8)
            request

          // The ascription selects the `Serviceable` overload of `transmit`; the
          // client half-closes after sending, so the server reads to EOF.
          val _: Stream[Data] = socket.transmit(t"request")
          received.await().also(server.stop())
        . assert(_ == t"request")

      suite(m"Duplex connections"):
        test(m"A Duplex sends and receives over a domain socket"):
          val path = t"/tmp/coaxial-duplex.sock"
          java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(path.s))
          val socket = DomainSocket(path)

          val server = socket.listen[Data]: connection =>
            val buffer = new Array[Byte](64)
            val count = connection.in.read(buffer)
            IArray.from(buffer.take(count.max(0)).to(List))

          val duplex = socket.duplex()
          duplex.send(Stream(ascii(t"ping")))
          val reply = bytes(duplex.stream.head)
          duplex.close()
          server.stop()
          reply
        . assert(_ == bytes(ascii(t"ping")))
