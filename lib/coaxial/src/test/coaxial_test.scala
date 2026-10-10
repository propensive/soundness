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

// The `transmit`, `listen`, `react` and `duplex` extensions are defined in this
// package, so they are already in scope; re-importing them through `soundness`
// would make the two same-shaped `transmit` overloads (from `Serviceable` and
// `Routable`) ambiguous, so they are excluded from the wildcard.

import java.net as jn
import java.nio.channels as jnc

import soundness.{transmit as _, listen as _, react as _, duplex as _, *}
import denominative.capped

import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import errorDiagnostics.stackTracesDiagnostics
import threads.platformThreads
import probates.awaitProbate

import Control.*

import socketBackends.javaBaseSockets

object Tests extends Suite(m"Coaxial tests"):
  def run(): Unit = unsafely:

    // A `Data` (`Array[Byte]^{}`) compares by reference, so byte-level assertions
    // go through `List[Byte]`.
    def ascii(text: Text): Data = Array.unsafeFrozen(text.s.getBytes("US-ASCII").nn)
    def bytes(data: Data): List[Byte] = data.to[List]
    def joined(stream: Chain[Data]): List[Byte] =
      (stream.stdlib.flatMap { d => d.readable.toSeq }.toList).to(List)
    def drained(stream: zephyrine.Stream[Data] over Credit): List[Byte] = stream.memoize.to[List]

    suite(m"Duplex streaming endpoints"):
      supervise:
        val payload = Data.fill(60000) { index => (index%97).toByte }

        test(m"data flows through native socket stream endpoints"):
          import supervisors.globalSupervisor

          val server = jnc.ServerSocketChannel.open().nn
          server.bind(jn.InetSocketAddress("127.0.0.1", 0))
          val port = server.socket.nn.getLocalPort

          val received = async:
            val serverDuplex = channelDuplex(server.accept().nn)
            summon[Data is Aggregable by Data].accept(serverDuplex.source)

          val client = jnc.SocketChannel.open(jn.InetSocketAddress("127.0.0.1", port)).nn
          val clientDuplex = channelDuplex(client)

          summon[Data is Streamable by Data over Credit].stream(payload).pump(clientDuplex.intake)
          client.shutdownOutput()

          val result = received.await()
          server.close()
          client.close()
          result.to[List]
        . assert(_ == payload.to[List])

    suite(m"Transmissible serialization"):
      test(m"Data is transmitted as a single chunk"):
        drained(summon[Data is Transmissible].serialize(ascii(t"abc")))
      . assert(_ == bytes(ascii(t"abc")))

      test(m"A Chain[Data] is transmitted unchanged"):
        val stream = Chain(ascii(t"ab"), ascii(t"cd"))
        drained(summon[Chain[Data] is Transmissible].serialize(stream))
      . assert(_ == bytes(ascii(t"abcd")))

      test(m"Text is transmitted via its character encoding"):
        drained(summon[Text is Transmissible].serialize(t"hello"))
      . assert(_ == bytes(ascii(t"hello")))

      test(m"An Encodable value is transmitted via its Text encoding"):
        drained(summon[Port is Transmissible].serialize(Port.unsafe[Tcp](8080)))
      . assert(_ == bytes(ascii(t"8080")))

      test(m"contramap adapts a Transmissible to a new source type"):
        val ints: Int is Transmissible = summon[Text is Transmissible].contramap[Int](_.show)
        drained(ints.serialize(42))
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
        socket.at(t"/info") == DomainSocket.Endpoint(socket, t"/info")
      . assert(_ == true)

    suite(m"Bind.Error"):
      test(m"PortInUse has a descriptive message"):
        Bind.Error.Reason.PortInUse.communicate.text
      . assert(_ == t"another process is already bound to the port")

      test(m"PermissionDenied has a descriptive message"):
        Bind.Error.Reason.PermissionDenied.communicate.text
      . assert(_ == t"the user does not have permission to bind the port")

      test(m"AddressUnavailable has a descriptive message"):
        Bind.Error.Reason.AddressUnavailable.communicate.text
      . assert(_ == t"the requested address is not available on this host")

      test(m"A Bind.Error incorporates its reason"):
        Bind.Error(Bind.Error.Reason.PortInUse).message.text
      . assert(_ == t"the socket could not be bound because another process is already "+
          t"bound to the port")

    supervise:
      suite(m"UDP server and client"):
        test(m"A bound UDP server receives a transmitted datagram"):
          val port = Port[Udp]()
          val received: Promise[Text] = Promise()

          val handler = (packet: Packet) =>
            received.fulfill(packet.data.utf8)
            UdpResponse.Reply(ascii(t"ack"))

          port.listen[Data](handler):

            // The `transmit` extension is overloaded on `Serviceable` (returns
            // `Chain[Data]`) and `Routable` (returns `Unit`); since value-discarding
            // makes any type conform to `Unit`, the `Routable` overload is not
            // reachable by ascription, so its given is exercised directly here.
            val routable = summon[Udp.Port is Routable]
            routable.transmit(routable.connect(port, Unset), zephyrine.Stream(ascii(t"ping")))
            received.await()
        . assert(_ == t"ping")

      suite(m"TCP server and client"):
        test(m"A client reacts to the server's pushed message"):
          val port = Port[Tcp]()
          port.listen[Data](socket => ascii(t"greeting")):
            val received: Data = port.react(Data())[Data]: message =>
              Conclude(ascii(t""), message)

            bytes(received)
        . assert(_ == bytes(ascii(t"greeting")))

      suite(m"Unix domain socket server and client"):
        test(m"A server receives the bytes a client transmits"):
          val path = t"/tmp/coaxial-request-response.sock"
          java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(path.s))
          val socket = DomainSocket(path)
          val received: Promise[Text] = Promise()

          val handler = (connection: Duplex) =>
            val request = connection.source.memoize
            received.fulfill(request.utf8)
            request

          socket.listen[Data](handler):

            // The ascription selects the `Serviceable` overload of `transmit`; the
            // client half-closes after sending, so the server reads to EOF.
            val _: zephyrine.Stream[Data] over zephyrine.Credit = socket.transmit(t"request")
            received.await()
        . assert(_ == t"request")

      suite(m"Duplex connections"):
        test(m"A Duplex sends and receives over a domain socket"):
          val path = t"/tmp/coaxial-duplex.sock"
          java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(path.s))
          val socket = DomainSocket(path)

          // One refill window is the client's single message (no half-close on a duplex).
          val handler = (connection: Duplex) =>
            val source = connection.source
            val count = source.refill(zephyrine.Credit(64)).or(0)
            source.lend { region => range => region.materialize(range.capped(count)) }

          socket.listen[Data](handler):

            socket.duplex: duplex =>
              duplex.send(zephyrine.Stream(ascii(t"ping")))

              // One refill window is the server's single reply.
              val source = duplex.source
              val count = source.refill(zephyrine.Credit(64)).or(0)
              val data =
                source.lend { region => range => region.materialize(range.capped(count)) }

              bytes(data)
        . assert(_ == bytes(ascii(t"ping")))

    supervise:
      suite(m"TLS server and client"):
        import internetAccess.online

        // Throwaway self-signed identities from the JDK's own `keytool`, so the test needs no
        // checked-in key material. PKCS#12 bytes, password `secret`.
        val password: Text = t"secret"

        def identity(name: Text): Data =
          val file = java.nio.file.Files.createTempFile("coaxial-", ".p12").nn
          java.nio.file.Files.delete(file)
          val keytool = java.lang.System.getProperty("java.home").nn+"/bin/keytool"

          val process =
            ProcessBuilder
              ( keytool, "-genkeypair", "-alias", "peer", "-keyalg", "EC", "-groupname",
                "secp256r1", "-dname", s"CN=${name.s}", "-validity", "1", "-storetype",
                "PKCS12", "-keystore", file.toString, "-storepass", password.s, "-keypass",
                password.s )
            . directory(file.getParent.nn.toFile) // not the daemon's cwd, which may be gone
            . nn.redirectErrorStream(true).nn.start().nn

          val output = String(process.getInputStream.nn.readAllBytes().nn, "UTF-8")
          process.waitFor()

          // Reported here rather than as a missing file: `keytool`'s complaint is the diagnosis.
          if !java.nio.file.Files.exists(file)
          then panic(m"keytool produced no keystore for $name: ${output.tt}")

          val bytes = Array.unsafeFrozen(java.nio.file.Files.readAllBytes(file).nn)
          java.nio.file.Files.delete(file)
          bytes

        val keystore: Data = identity(t"coaxial-test")
        val certificate: Data = Tls.certificate(keystore, password).or(Data())
        val fingerprint: Data = Tls.fingerprint(certificate)

        // A second identity, for a client that authenticates itself to the listener.
        val clientStore: Data = identity(t"coaxial-client")

        val clientFingerprint: Data =
          Tls.fingerprint(Tls.certificate(clientStore, password).or(Data()))

        // One refill window is the peer's single message (no half-close on a duplex).
        def message(duplex: Duplex): Data =
          val source = duplex.source
          val count = source.refill(zephyrine.Credit(64)).or(0)
          source.lend { region => range => region.materialize(range.capped(count)) }

        def exchange(serverTls: Tls, clientTls: Tls): List[Byte] =
          val port = Port[Tcp]()
          val secure: SecurePort = SecurePort(port)
          given server: Tls = serverTls

          secure.listen[Data](message(_)):
            given client: Tls = clientTls

            SecureEndpoint(t"127.0.0.1", port.number).duplex: duplex =>
              duplex.send(zephyrine.Stream(ascii(t"ping")))
              bytes(message(duplex))

        // A refusal surfaces as a handshake exception on the client, or — under TLS 1.3, where
        // the client finishes its handshake before the listener has judged its certificate — as
        // the listener closing the connection without replying.
        def outcome(serverTls: Tls, clientTls: Tls): Text =
          try
            if exchange(serverTls, clientTls) == bytes(ascii(t"ping")) then t"connected"
            else t"refused"
          catch case error: Exception => t"refused"

        test(m"the keystore's certificate is found and fingerprinted"):
          (certificate.length > 0, fingerprint.length)
        . assert(_ == (true, 32))

        test(m"a client pinning the server's certificate completes an exchange"):
          exchange(Tls.keyed(keystore, password), TlsAcceptance().pinning(fingerprint).tls())
        . assert(_ == bytes(ascii(t"ping")))

        test(m"a client pinning a different fingerprint is refused"):
          val wrong: Data = Data.fill(32) { index => index.toByte }
          outcome(Tls.keyed(keystore, password), TlsAcceptance().pinning(wrong).tls())
        . assert(_ == t"refused")

        test(m"a strict client rejects the self-signed server"):
          outcome(Tls.keyed(keystore, password), TlsAcceptance().tls())
        . assert(_ == t"refused")

        test(m"peers pinning each other complete a mutually-authenticated exchange"):
          exchange
            ( TlsAcceptance().pinning(clientFingerprint).keyed(keystore, password, mutual = true),
              TlsAcceptance().pinning(fingerprint).keyed(clientStore, password) )
        . assert(_ == bytes(ascii(t"ping")))

        test(m"a mutual listener refuses a client presenting a different identity"):
          outcome
            ( TlsAcceptance().pinning(clientFingerprint).keyed(keystore, password, mutual = true),
              TlsAcceptance().pinning(fingerprint).keyed(keystore, password) )
        . assert(_ == t"refused")

        test(m"a mutual listener refuses a client presenting no certificate"):
          outcome
            ( TlsAcceptance().pinning(clientFingerprint).keyed(keystore, password, mutual = true),
              TlsAcceptance().pinning(fingerprint).tls() )
        . assert(_ == t"refused")

        test(m"a listener that is not mutual still admits a client without a certificate"):
          outcome
            ( TlsAcceptance().pinning(clientFingerprint).keyed(keystore, password),
              TlsAcceptance().pinning(fingerprint).tls() )
        . assert(_ == t"connected")

    supervise:
      suite(m"Datagram sockets"):
        val backend = summon[Socket.Backend]

        test(m"Reuse options let two UDP sockets share a port"):
          import socketOptions.reuseAddressSocketOption, socketOptions.reusePortSocketOption
          val port = Port[Udp]()
          val options = summon[Every[Socket.Option.Udp]].values.to(List)
          val first = backend.listenUdp(port, Unset, options)
          val second = try backend.listenUdp(port, Unset, options) finally backend.unbind(first)
          backend.unbind(second)
          true
        . assert(_ == true)

        test(m"A datagram larger than the default buffer arrives whole with DatagramSize set"):
          given Socket.Option.DatagramSize = socketOptions.datagramSize(4096)
          val port = Port[Udp]()
          val received: Promise[Int] = Promise()

          val handler = (packet: Packet) =>
            received.fulfill(packet.data.length)
            UdpResponse.Ignore

          port.listen[Data](handler):
            val routable = summon[Udp.Port is Routable]
            routable.transmit(routable.connect(port, Unset), zephyrine.Stream(Data.fill(3000)(_.toByte)))
            received.await()
        . assert(_ == 3000)

        test(m"A datagram exchange returns the server's reply"):
          val port = Port[Udp]()
          val handler = (packet: Packet) => UdpResponse.Reply(ascii(t"pong"))

          port.listen[Data](handler):
            backend.exchangeUdp(Localhost on port, Unset, List(Socket.Option.Timeout(2000)), ascii(t"ping"))
            . data.utf8
        . assert(_ == t"pong")

        test(m"A multicast subscription receives what it sends to the group"):
          import socketOptions.multicastLoopSocketOption
          val received: Promise[Text] = Promise()
          val multicast = Multicast(ip"239.255.77.77", Port[Udp]())

          // Vacuous on a host with no multicast-capable interface.
          if Multicast.interfaces(Unset) == Nil then t"hello" else
            val handler = (packet: Packet) =>
              received.fulfill(packet.data.utf8)
              Multicast.Reply.Ignore

            multicast.subscribe(handler):
              summon[Multicast.Subscription].send(ascii(t"hello"))
              received.await()
        . assert(_ == t"hello")

        test(m"A multicast subscription can answer one sender by unicast"):
          import socketOptions.multicastLoopSocketOption
          val received: Promise[Text] = Promise()
          val multicast = Multicast(ip"239.255.77.78", Port[Udp]())

          // The subscription's own looped-back "ping" is answered by unicast to its sender, which
          // is the subscription itself, so the "pong" arrives as a second packet.
          if Multicast.interfaces(Unset) == Nil then t"pong" else
            val handler = (packet: Packet) =>
              if packet.data.utf8 == t"ping" then Multicast.Reply.Unicast(ascii(t"pong")) else
                received.fulfill(packet.data.utf8)
                Multicast.Reply.Ignore

            multicast.subscribe(handler):
              summon[Multicast.Subscription].send(ascii(t"ping"))
              received.await()
        . assert(_ == t"pong")

        val example = dns"example.com"
        val question = Dns.Question(example, Dns.Type.A)
        val answer = Dns.Record(example, 60, Dns.Rdata.A(ip"10.0.0.1"))

        test(m"A DNS query returns the nameserver's answer"):
          val port = Port[Udp]()

          val nameserver = (packet: Packet) =>
            val query = packet.data.as[Dns.Message]
            UdpResponse.Reply(Dns.Message.response(query, List(answer)).in[Data])

          port.listen[Data](nameserver):
            (Localhost on port).query(Dns.Message.query(7, List(question))).answers
        . assert(_ == List(answer))

        test(m"A lookup returns the answering records"):
          val port = Port[Udp]()

          val nameserver = (packet: Packet) =>
            val query = packet.data.as[Dns.Message]
            UdpResponse.Reply(Dns.Message.response(query, List(answer)).in[Data])

          port.listen[Data](nameserver):
            (Localhost on port).lookup(example)
        . assert(_ == List(answer))

        test(m"A reply with the wrong ID is rejected"):
          val port = Port[Udp]()

          val nameserver = (packet: Packet) =>
            val query = packet.data.as[Dns.Message]
            UdpResponse.Reply(Dns.Message.response(query, List(answer)).copy(id = 8).in[Data])

          port.listen[Data](nameserver):
            capture[Dns.Error]((Localhost on port).query(Dns.Message.query(7, List(question)))).reason
        . assert(_ == Dns.Error.Reason.Mismatch(8))

        test(m"A query with no reply times out"):
          given Socket.Option.Timeout = socketOptions.timeout(200)
          val port = Port[Udp]()

          port.listen[Data]((packet: Packet) => UdpResponse.Ignore):
            capture[Dns.Error]((Localhost on port).query(Dns.Message.query(7, List(question)))).reason
        . assert(_ == Dns.Error.Reason.Timeout)

        test(m"A datagram exchange times out when no reply arrives"):
          val port = Port[Udp]()
          val handler = (packet: Packet) => UdpResponse.Ignore

          port.listen[Data](handler):
            capture[Socket.Error]:
              backend.exchangeUdp(Localhost on port, Unset, List(Socket.Option.Timeout(200)), ascii(t"ping"))
            . reason
        . assert(_ == Socket.Error.Reason.Timeout)

    suite(m"Socket options"):
      test(m"reuseAddress sets SO_REUSEADDR on a configured TCP server socket"):
        import socketOptions.reuseAddressSocketOption
        val server = jn.ServerSocket()
        configure(server, (summon[Every[Socket.Option.Tcp]].values).to(List))
        server.getOption(java.net.StandardSocketOptions.SO_REUSEADDR).nn.booleanValue
          .also(server.close())
      . assert(_ == true)

      test(m"broadcast sets SO_BROADCAST on a configured UDP socket"):
        import socketOptions.broadcastSocketOption
        val socket = jn.DatagramSocket()
        configure(socket, (summon[Every[Socket.Option.Udp]].values).to(List))
        socket.getOption(java.net.StandardSocketOptions.SO_BROADCAST).nn.booleanValue
          .also(socket.close())
      . assert(_ == true)

      test(m"a UDP-only option is collected only for UDP connections"):
        import socketOptions.broadcastSocketOption
        (summon[Every[Socket.Option.Tcp]].values, summon[Every[Socket.Option.Udp]].values)
      . assert(_ == (Nil, List(Socket.Option.Broadcast)))

      test(m"a shared option is collected for every connection type"):
        import socketOptions.reuseAddressSocketOption
        ( summon[Every[Socket.Option.Tcp]].values,
          summon[Every[Socket.Option.Udp]].values,
          summon[Every[Socket.Option.Domain]].values )
      . assert(_ == (List(Socket.Option.ReuseAddress),
                     List(Socket.Option.ReuseAddress),
                     List(Socket.Option.ReuseAddress)))

      test(m"interfaceFor resolves a local interface by its hardware address"):
        var nic: java.net.NetworkInterface | Null = null
        val interfaces = java.net.NetworkInterface.getNetworkInterfaces.nn

        while nic == null && interfaces.hasMoreElements do
          val candidate = interfaces.nextElement.nn
          if candidate.getHardwareAddress != null then nic = candidate

        // Skip on hosts where no interface exposes a hardware address.
        if nic == null then true else
          var value = 0L
          Array.unsafeFrozen(nic.getHardwareAddress.nn).readable.toSeq.each: byte => value = (value << 8) | (byte & 0xFF)
          interfaceFor(urticose.MacAddress(value)).let(_ => true).or(false)
      . assert(_ == true)

    suite(m"Native-rendering coverage"):
      test(m"coaxial's types inspect natively"):
        Inspectable.fallbacks(DomainSocket(t"/tmp/example.sock").inspect)
      . assert(_ == Nil)

      test(m"A domain socket shows its path, marked as an endpoint"):
        DomainSocket(t"/tmp/example.sock").inspect
      . assert(_ == t"⇄/tmp/example.sock")
