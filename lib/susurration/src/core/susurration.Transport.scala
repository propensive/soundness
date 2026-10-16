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
package susurration

import java.io as ji
import java.math as jm

import scala.caps
import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import coaxial.*
import contingency.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import zephyrine.*

import Ssh.Error.Reason

// The SSH transport layer (RFC 4253) over a connected `Duplex`: the identification exchange, the
// binary packet protocol, and the key exchange that turns a cleartext channel into an
// authenticated, encrypted one, with the server's identity checked against the `HostKeys` policy.
// After `connect`, `send` and `receive` move one packet's payload at a time, encrypted; the
// authentication and connection protocols are built on them.
//
// The transport is driven by one thread: the client sends a message and reads what comes back.
// That is the shape of every exchange a git or exec client makes, and it keeps this layer free
// of tasks; a reader task can be put in front of `receive` by a layer that needs one.
//
// The ciphers offered are the AEAD ones (`chacha20-poly1305@openssh.com` and `aes*-gcm@
// openssh.com`), which every current server has: they carry their own integrity, so no separate
// MAC algorithm is negotiated into use. Re-keying after the RFC's limits is not yet done.
object Transport:
  // The agreed algorithms of a key exchange.
  case class Negotiated(kex: Text, hostKey: Text, cipher: Text)

  // The cipher state of one direction, replaced at each NEWKEYS.
  private trait Codec:
    def blockSize: Int

    // Encrypts a packet whose length field is `length` and whose body (padding length, payload,
    // padding) is `body`, under sequence number `sequence`.
    def seal(sequence: Int, length: Int, body: Data): Data

    // Reads one packet from `input`: the first four bytes have been read into `head`.
    def unseal(sequence: Int, head: Data, read: Int => Data): Optional[Data]

  private object Plain extends Codec:
    def blockSize: Int = 8

    def seal(sequence: Int, length: Int, body: Data): Data =
      val writer = Wire.Writer()
      writer.uint32(length)
      writer.bytes(body)
      writer.data

    def unseal(sequence: Int, head: Data, read: Int => Data): Optional[Data] =
      read(int32(head))

  // RFC 5647 as OpenSSH does it: the length is associated data, in the clear, the nonce is the
  // derived IV whose last eight bytes count invocations, and the tag follows the ciphertext. The
  // counter advances once per packet, exactly as the sequence number does, so the nonce is a
  // function of the sequence number and the number the keys took effect at; the codec holds no
  // state.
  private class Gcm(aead: Crypto.Aead, key: Data, iv: Data, base: Int) extends Codec:
    private val fixed: Data = iv.keep(4)
    private val start: Long = int64(iv.skip(4))
    def blockSize: Int = 16

    private def nonce(sequence: Int): Data =
      join(fixed, be64(start + ((sequence.toLong - base.toLong) & 0xffffffffL)))

    def seal(sequence: Int, length: Int, body: Data): Data =
      val head = be32(length)
      join(head, aead.seal(key, nonce(sequence), head, body))

    def unseal(sequence: Int, head: Data, read: Int => Data): Optional[Data] =
      aead.unseal(key, nonce(sequence), head, read(int32(head) + 16))

  // OpenSSH's `chacha20-poly1305@openssh.com` (PROTOCOL.chacha20poly1305): the second half of the
  // 64-byte key encrypts the length under block zero of its keystream, the first half encrypts
  // the body from block one, and block zero of that keystream is the Poly1305 key for a tag over
  // the encrypted length and body. The nonce is the sequence number.
  private class ChaCha(cipher: Crypto.StreamCipher, key: Data) extends Codec:
    private val main: Data = key.keep(32)
    private val lengthKey: Data = key.skip(32)
    def blockSize: Int = 8

    private def nonce(sequence: Int): Data =
      join(Array.fill[Byte](8)(0), be32(sequence))

    def seal(sequence: Int, length: Int, body: Data): Data =
      val n = nonce(sequence)
      val head = cipher.xor(lengthKey, n, 0, be32(length))
      val encrypted = cipher.xor(main, n, 1, body)
      val polyKey = cipher.xor(main, n, 0, Array.fill[Byte](32)(0))
      val sealed0 = join(head, encrypted)
      join(sealed0, Poly1305.mac(polyKey, sealed0))

    def unseal(sequence: Int, head: Data, read: Int => Data): Optional[Data] =
      val n = nonce(sequence)
      val length = int32(cipher.xor(lengthKey, n, 0, head))

      if length < 0 || length > Wire.Limit then Unset else
        val rest = read(length + 16)
        val encrypted = rest.keep(length)
        val tag = rest.skip(length)
        val polyKey = cipher.xor(main, n, 0, Array.fill[Byte](32)(0))
        val expected = Poly1305.mac(polyKey, join(head, encrypted))
        if Array.same(expected, tag) then cipher.xor(main, n, 1, encrypted) else Unset

  private def int32(data: Data): Int =
    val bytes = Array.unsafeJvm(data)

    ((bytes(0) & 0xff) << 24) | ((bytes(1) & 0xff) << 16) | ((bytes(2) & 0xff) << 8) |
      (bytes(3) & 0xff)

  private def int64(data: Data): Long =
    (int32(data.keep(4)).toLong << 32) | (int32(data.skip(4)).toLong & 0xffffffffL)

  private def be32(value: Int): Data =
    val writer = Wire.Writer()
    writer.uint32(value)
    writer.data

  private def be64(value: Long): Data =
    val writer = Wire.Writer()
    writer.uint64(value)
    writer.data

  private def join(left: Data, right: Data): Data =
    val writer = Wire.Writer()
    writer.bytes(left)
    writer.bytes(right)
    writer.data

  // The key material of one direction after a key exchange (RFC 4253 §7.2): each of the six keys
  // is `HASH(K || H || letter || session_id)`, extended by hashing again with the material so far
  // until it is long enough.
  private def derive(shared: BigInt, exchange: Data, session: Data, letter: Char, length: Int)
    ( using Hash in Sha2[256] )
  :   Data =

    def hash(data: Data): Data = data.digest[Sha2[256]].data

    val prefix: Data =
      val writer = Wire.Writer()
      writer.mpint(shared)
      writer.bytes(exchange)
      writer.data

    val first = hash(join(prefix, join(Array(letter.toByte), session)))

    def recur(material: Data): Data =
      if material.length >= length then material.keep(length)
      else recur(join(material, hash(join(prefix, material))))

    recur(first)

// The state of one connection. A `caps.Mutable`: the sequence numbers, the codecs and the session
// identifier change as the protocol advances, and one thread drives it.
class Transport(duplex: Duplex, hostname: Text, port: Int)
  ( using crypto:     Ssh.Provider,
          algorithms: Ssh.Algorithms,
          hostKeys:   Ssh.HostKeys,
          sha256:     Hash in Sha2[256] )
extends caps.Mutable:

  import Transport.*

  // Sealed: the duplex's read side, opened once here and read only by the thread driving this
  // transport, which is the sole owner of the stream it adapts.
  // [construction-fresh] fresh InputStream over the duplex's single read side, sealed at creation
  private val input: ji.InputStream = caps.unsafe.unsafeAssumePure(duplex.source.inputStream)

  private var outgoing: Int = 0
  private var incoming: Int = 0
  private var encoder: Codec = Plain
  private var decoder: Codec = Plain
  private var sessionId0: Optional[Data] = Unset
  private var negotiated0: Optional[Negotiated] = Unset
  private var serverVersion0: Optional[Text] = Unset
  private var services: List[Text] = Nil

  // The exchange hash of the first key exchange, which identifies the session (RFC 4253 §7.2)
  // and which public-key authentication signs over.
  def sessionId: Optional[Data] = sessionId0
  def negotiated: Optional[Negotiated] = negotiated0
  def serverVersion: Optional[Text] = serverVersion0

  // The identification exchange and the first key exchange, leaving the connection encrypted.
  update def connect()(using Tactic[Ssh.Error], Diagnostics): Unit =
    // [java-boundary] fresh array returned by the JDK, retained by nobody else
    duplex.send(Stream(Array.unsafeFrozen(t"${Ssh.Version}\r\n".s.getBytes("UTF-8").nn)))
    val server = identification()
    serverVersion0 = server
    exchange(server)

  // Sends one packet with the given payload (RFC 4253 §6), padded to the cipher's block and
  // encrypted under the current keys.
  update def send(payload: Data)(using Tactic[Ssh.Error], Diagnostics): Unit =
    val block = encoder.blockSize
    // With an AEAD cipher the length is not encrypted, so the alignment excludes it, as OpenSSH
    // computes it; a cleartext packet aligns its whole length.
    val aligned = if encoder == Plain then 5 + payload.length else 1 + payload.length
    val padding0 = block - aligned % block
    val padding = if padding0 < 4 then padding0 + block else padding0

    val body: Data =
      val writer = Wire.Writer()
      writer.byte(padding.toByte)
      writer.bytes(payload)
      writer.bytes(crypto.random.bytes(padding))
      writer.data

    duplex.send(Stream(encoder.seal(outgoing, body.length, body)))
    outgoing += 1

  // Receives the next packet's payload, handling the transport's own housekeeping messages
  // (`IGNORE`, `DEBUG`, `UNIMPLEMENTED`, `EXT_INFO`) itself and raising on `DISCONNECT`.
  update def receive()(using Tactic[Ssh.Error], Diagnostics): Data =
    val head = readFully(4)
    val body = decoder.unseal(incoming, head, readFully).or(abort(Ssh.Error(Reason.Integrity)))
    incoming += 1
    val reader = Wire.Reader(body)
    given wire: Tactic[Wire.Error] = malformed
    val padding = reader.byte() & 0xff

    if padding + 1 > body.length
    then abort(Ssh.Error(Reason.Malformed(t"the padding exceeds the packet")))

    val payload = body.skip(1).keep(body.length - 1 - padding)

    if payload.length == 0 then abort(Ssh.Error(Reason.Malformed(t"an empty payload")))

    Array.unsafeJvm(payload)(0) match
      case Ssh.Message.Ignore | Ssh.Message.Debug | Ssh.Message.ExtInfo =>
        receive()

      // The server did not understand something this client sent: a defect here, not a message
      // to wait past.
      case Ssh.Message.Unimplemented =>
        abort(Ssh.Error(Reason.Unexpected(Ssh.Message.Unimplemented & 0xff)))

      case Ssh.Message.Disconnect =>
        val reader = Wire.Reader(payload)
        reader.byte()
        val code = reader.uint32()
        val text = reader.text()
        abort(Ssh.Error(Reason.Disconnected(code, text)))

      case _ =>
        payload

  // Receives a packet, which must be of the given type.
  update def expect(message: Byte)(using Tactic[Ssh.Error], Diagnostics): Wire.Reader^ =
    val payload = receive()
    val reader = Wire.Reader(payload)
    given wire: Tactic[Wire.Error] = malformed
    val kind = reader.byte()
    if kind != message then abort(Ssh.Error(Reason.Unexpected(kind & 0xff)))
    reader

  // Requests a service (`ssh-userauth`), which the server must accept. Requesting a service
  // already granted is a no-op.
  update def service(name: Text)(using Tactic[Ssh.Error], Diagnostics): Unit =
    if !services.exists(_ == name) then
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.ServiceRequest)
      writer.text(name)
      send(writer.data)
      given wire: Tactic[Wire.Error] = malformed
      val accepted = expect(Ssh.Message.ServiceAccept).text()
      if accepted != name then abort(Ssh.Error(Reason.ServiceRefused(name)))
      services = name :: services

  // Public-key authentication (RFC 4252 §7) as `user`, with each signer in turn: the server is
  // first asked whether it would accept the key at all, and only an acceptable key signs. The
  // signature covers the session identifier and the request, so it cannot be replayed on
  // another connection. Raises `Unauthenticated` when no signer is accepted.
  update def authenticate(user: Text, signers: List[Signer])(using Tactic[Ssh.Error], Diagnostics)
  :   Unit =

    given wire: Tactic[Wire.Error] = malformed
    val session = sessionId.or(abort(Ssh.Error(Reason.Malformed(t"no session yet"))))
    service(t"ssh-userauth")

    // The request, as a query (`signed` false, no signature), as the data the signature covers
    // (`signed` true, no signature) or as the real thing (`signed` true, with the signature).
    def request(signer: Signer, signed: Boolean, signature: Optional[Data]): Data =
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.UserauthRequest)
      writer.text(user)
      writer.text(t"ssh-connection")
      writer.text(t"publickey")
      writer.boolean(signed)
      writer.text(signer.algorithm)
      writer.string(signer.blob)
      signature.let(writer.string(_))
      writer.data

    // Replies until a decisive one: banners are skipped.
    def reply(): (Byte, Wire.Reader^) =
      val payload = receive()
      val reader = Wire.Reader(payload)
      val kind = reader.byte()
      if kind == Ssh.Message.UserauthBanner then reply() else (kind, reader)

    def attempt(signers: List[Signer], methods: Text): Unit = signers match
      case signer :: rest =>
        send(request(signer, false, Unset))

        reply() match
          case (Ssh.Message.UserauthPkOk, _) =>
            val signed: Data =
              val writer = Wire.Writer()
              writer.string(session)
              writer.bytes(request(signer, true, Unset))
              writer.data

            send(request(signer, true, signer.sign(signed)))

            reply() match
              case (Ssh.Message.UserauthSuccess, _) =>
                ()

              case (Ssh.Message.UserauthFailure, reader) =>
                attempt(rest, reader.names().join(t","))

              case (kind, _) =>
                abort(Ssh.Error(Reason.Unexpected(kind & 0xff)))

          case (Ssh.Message.UserauthFailure, reader) =>
            attempt(rest, reader.names().join(t","))

          case (kind, _) =>
            abort(Ssh.Error(Reason.Unexpected(kind & 0xff)))

      case _ =>
        abort(Ssh.Error(Reason.Unauthenticated(methods)))

    attempt(signers, t"")

  // Ends the connection politely (RFC 4253 §11.1).
  update def disconnect()(using Tactic[Ssh.Error], Diagnostics): Unit =
    val writer = Wire.Writer()
    writer.byte(Ssh.Message.Disconnect)
    writer.uint32(11) // SSH_DISCONNECT_BY_APPLICATION
    writer.text(t"")
    writer.text(t"")
    send(writer.data)
    duplex.close()

  private def malformed(using Tactic[Ssh.Error], Diagnostics): Tactic[Wire.Error] =
    summon[Tactic[Ssh.Error]].contramap: error => Ssh.Error(Reason.Malformed(error.message.text))

  private def readFully(count: Int)(using Tactic[Ssh.Error], Diagnostics): Data =
    val output = Array.scratch[Byte](count)

    try ji.DataInputStream(input).readFully(output)
    catch case _: ji.EOFException => abort(Ssh.Error(Reason.ClosedConnection))

    // [java-boundary] a scratch array filled by the JDK, retained by nobody else
    Array.unsafeFrozen(output)

  // The server's identification line (RFC 4253 §4.2): lines before the `SSH-` line are allowed
  // and ignored, and the line's terminator is excluded.
  private def identification()(using Tactic[Ssh.Error], Diagnostics): Text =
    def line(): Text =
      val buffer = ji.ByteArrayOutputStream()

      def recur(): Text =
        val byte = input.read()

        if byte < 0 then abort(Ssh.Error(Reason.ClosedConnection))
        else if byte == '\n' then String(buffer.toByteArray.nn, "UTF-8").tt
        else
          if byte != '\r' then buffer.write(byte)

          if buffer.size > 255
          then abort(Ssh.Error(Reason.Malformed(t"an overlong identification line")))

          recur()

      recur()

    def recur(remaining: Int): Text =
      if remaining == 0 then abort(Ssh.Error(Reason.Malformed(t"no identification line")))
      else
        val next = line()
        if next.starts(t"SSH-") then next else recur(remaining - 1)

    val version = recur(100)

    if !version.starts(t"SSH-2.0-") && !version.starts(t"SSH-1.99-")
    then abort(Ssh.Error(Reason.Malformed(t"the server speaks $version, not SSH-2.0")))

    version

  // The key exchange (RFC 4253 §7 and RFC 5656 §4 / RFC 8731): KEXINIT both ways, one ECDH
  // round trip, the exchange hash, the host key's signature over it, and NEWKEYS.
  private update def exchange(server: Text)(using Tactic[Ssh.Error], Diagnostics): Unit =
    given wire: Tactic[Wire.Error] = malformed

    val clientInit: Data =
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.KexInit)
      writer.bytes(crypto.random.bytes(16))
      writer.names(algorithms.kex)
      writer.names(algorithms.hostKey)
      writer.names(algorithms.cipher)
      writer.names(algorithms.cipher)
      writer.names(algorithms.mac)
      writer.names(algorithms.mac)
      writer.names(algorithms.compression)
      writer.names(algorithms.compression)
      writer.names(Nil)
      writer.names(Nil)
      writer.boolean(false)
      writer.uint32(0)
      writer.data

    send(clientInit)
    val serverInit = receive()
    val reader = Wire.Reader(serverInit)
    val kind = reader.byte()
    if kind != Ssh.Message.KexInit then abort(Ssh.Error(Reason.Unexpected(kind & 0xff)))
    reader.bytes(16)
    val serverKex = reader.names()
    val serverHostKey = reader.names()
    val serverCipherOut = reader.names()
    val serverCipherIn = reader.names()
    reader.names(); reader.names(); reader.names(); reader.names(); reader.names(); reader.names()
    val guess = reader.boolean()

    // The client's first supported algorithm the server also supports (RFC 4253 §7.1).
    def choose(kind: Text, ours: List[Text], theirs: List[Text]): Text =
      ours.filter { ours => theirs.exists(_ == ours) }.prim.or:
        abort(Ssh.Error(Reason.NoCommonAlgorithm(kind)))

    val kex = choose(t"key exchange", algorithms.kex, serverKex)
    val hostKeyAlgorithm = choose(t"host key", algorithms.hostKey, serverHostKey)
    val cipherOut = choose(t"cipher", algorithms.cipher, serverCipherOut)
    val cipherIn = choose(t"cipher", algorithms.cipher, serverCipherIn)
    negotiated0 = Negotiated(kex, hostKeyAlgorithm, cipherOut)

    // A wrong guess leaves a KEX packet to be ignored (RFC 4253 §7.1). The server guessed only
    // if its first algorithms are ours.
    val guessedWrong =
      serverKex.prim != algorithms.kex.prim || serverHostKey.prim != algorithms.hostKey.prim

    if guess && guessedWrong then receive()

    // ECDH: our ephemeral key, its raw public point on the wire.
    val agreement = if kex == t"ecdh-sha2-nistp256" then crypto.ecdh else crypto.x25519
    val ephemeral = agreement.generateKeyPair(256)
    val ourPoint = rawPoint(agreement.privateToPublic(ephemeral))

    val identifier: Asn1 =
      if kex == t"ecdh-sha2-nistp256"
      then Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.Ec), Asn1.ObjectId(KeyAlgorithm.P256)))
      else Asn1.Sequence(List(Asn1.ObjectId(KeyAlgorithm.X25519)))

    val init: Data =
      val writer = Wire.Writer()
      writer.byte(Ssh.Message.KexEcdhInit)
      writer.string(ourPoint)
      writer.data

    send(init)
    val reply = expect(Ssh.Message.KexEcdhReply)
    val hostKey = reply.string()
    val theirPoint = reply.string()
    val signature = reply.string()
    reply.end()

    val keyType = safely(Wire.Reader(hostKey).text()).or(t"?")

    if keyType != hostKeyTypeOf(hostKeyAlgorithm)
    then abort(Ssh.Error(Reason.Malformed(t"the host key is not a $hostKeyAlgorithm key")))

    if !hostKeys.accept(hostname, port, keyType, hostKey)
    then abort(Ssh.Error(Reason.HostKeyRejected(keyType)))

    val secret = agreement.agree(ephemeral, Spki.wrap(identifier, theirPoint))
    val shared = BigInt(jm.BigInteger(1, Array.unsafeJvm(secret)))

    val exchangeHash: Data =
      val writer = Wire.Writer()
      writer.text(Ssh.Version)
      writer.text(server)
      writer.string(clientInit)
      writer.string(serverInit)
      writer.string(hostKey)
      writer.string(ourPoint)
      writer.string(theirPoint)
      writer.mpint(shared)
      writer.data.digest[Sha2[256]].data

    if !verify(hostKeyAlgorithm, hostKey, signature, exchangeHash)
    then abort(Ssh.Error(Reason.BadSignature))

    val session = sessionId0.or(exchangeHash)
    sessionId0 = session

    send(Array(Ssh.Message.NewKeys))
    expect(Ssh.Message.NewKeys).end()

    def keys(ivLetter: Char, keyLetter: Char, cipher: Text, base: Int): Codec = cipher match
      case t"chacha20-poly1305@openssh.com" =>
        ChaCha(crypto.chaCha20, derive(shared, exchangeHash, session, keyLetter, 64))

      case t"aes128-gcm@openssh.com" =>
        Gcm(crypto.aesGcm, derive(shared, exchangeHash, session, keyLetter, 16),
          derive(shared, exchangeHash, session, ivLetter, 12), base)

      case _ =>
        Gcm(crypto.aesGcm, derive(shared, exchangeHash, session, keyLetter, 32),
          derive(shared, exchangeHash, session, ivLetter, 12), base)

    encoder = keys('A', 'C', cipherOut, outgoing)
    decoder = keys('B', 'D', cipherIn, incoming)

  private def hostKeyTypeOf(algorithm: Text): Text = algorithm match
    case t"rsa-sha2-256" | t"rsa-sha2-512" => t"ssh-rsa"
    case other                             => other

  // The BIT STRING of a SubjectPublicKeyInfo: the raw point the wire carries.
  private def rawPoint(spki: Data)(using Tactic[Ssh.Error], Diagnostics): Data =
    safely(Der(spki).as[Asn1]) match
      case Asn1.Sequence(List(_, Asn1.BitString(bits, 0))) =>
        bits

      case _ =>
        abort(Ssh.Error(Reason.Malformed(t"the ephemeral key is malformed")))

  // The server's signature over the exchange hash, in the wire form of its algorithm (RFC 8332,
  // RFC 5656 §3.1.2, RFC 8709), checked against the host key.
  private def verify(algorithm: Text, hostKey: Data, signature: Data, hash: Data)
    ( using Tactic[Ssh.Error], Diagnostics )
  :   Boolean =

    given wire: Tactic[Wire.Error] = malformed

    given publicKey: Tactic[PublicKey.Error] = summon[Tactic[Ssh.Error]].contramap: error =>
      Ssh.Error(Reason.Malformed(error.message.text))

    val spki = OpenSsh.spki(hostKey)
    val reader = Wire.Reader(signature)
    val signatureType = reader.text()
    val bytes = reader.string()

    if signatureType != algorithm then false else algorithm match
      case t"ssh-ed25519" =>
        crypto.ed25519.verify(hash, bytes, spki)

      case t"ecdsa-sha2-nistp256" =>
        val inner = Wire.Reader(bytes)
        val r = inner.mpint()
        val s = inner.mpint()
        val der: Asn1 = Asn1.Sequence(List(Asn1.Integer(r), Asn1.Integer(s)))
        crypto.ecdsa(t"SHA256").verify(hash, der.in[Der].data, spki)

      case t"rsa-sha2-256" =>
        crypto.rsaSignature(t"SHA256").verify(hash, bytes, spki)

      case t"rsa-sha2-512" =>
        crypto.rsaSignature(t"SHA512").verify(hash, bytes, spki)

      case _ =>
        false
