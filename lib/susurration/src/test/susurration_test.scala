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

import soundness.*

import strategies.throwUnsafely
import charsets.utf8Charset, codepages.utf8Codepage, textSanitizers.skipSanitizer
import dysasymptotics.linearSize
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import cloaks.heapCloak
import alphabets.hexUpperCase, alphabets.base64Standard
import socketBackends.javaBaseSockets
import logging.silentLogging
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
import threads.platformThreads
import probates.awaitProbate

import java.nio.file as jnf

object Tests extends Suite("ssh", m"Susurration tests"):
  // A plain entry point, so the suite can also run outside fume:
  // `java -cp out/susurration/test/assembly.dest/out.jar susurration.Tests`.
  def main(args: scala.Array[String]): Unit =
    val passed = java.util.concurrent.atomic.AtomicInteger(0)
    val failed = java.util.concurrent.atomic.AtomicInteger(0)
    val out = java.lang.System.out.nn

    def handle(event: probably.TestEvent): Unit = event match
      case probably.TestEvent.TestCompleted(test, _, _, outcome, _, _) =>
        (if outcome.outcome == t"pass" then passed else failed).incrementAndGet()
        out.println(t"[${outcome.outcome}] ${test.path.join(t" / ")}".s)

      case probably.TestEvent.DetailMessage(_, message)          => out.println(t"    $message".s)
      case probably.TestEvent.DetailCompare(_, expected, found, _) =>
        out.println(t"    expected: $expected".s)
        out.println(t"    found:    $found".s)

      case probably.TestEvent.RunTerminated(error, _, _) =>
        out.println(t"suite threw: ${error.components.map(_.message).join(t"; ")}".s)

      case other => out.println(other.toString)

    val status = Tests.invoke(t"", handle)
    out.println(t"${passed.get} passed, ${failed.get} failed".s)
    java.lang.System.exit(status)

  val ed25519Blob: Data =
    t"AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS".deserialize[Base64]

  val rsaBlob: Data =
    t"AAAAB3NzaC1yc2EAAAADAQABAAABAQDcz0N3WrVZdgC9lXMxggLgs1OxMHSZjTzkCPsI268siCVGLUK6K2aSzdjr0RV03JXdsgWdiabWqlkMcW3oWn4R9JvoPMTowZ3cbUIZKQGXkPk9RESxRcAxHPgIayELROpJart9MzVvPIR3cVcG5CqQ10noIFWwqaCD/m5Idm+ZLcj1VlJ9NzHm1tHRyMaeLKxBSIsRGAPkir9hrbxD+S1qfStCaGY+uwrQCBb4KobM4cwivoTe6hIkivH4JHsHu1oZuozJzGhuJ5Db6l4LnXJD5L9wdq60+npNmIBVougLtQRzrvKVTr7s2WE46jpj/8LbUHw77oYI9rr/JdW+sCyH".deserialize[Base64]

  // github.com's published Ed25519 host key, with a hashed entry for it generated by OpenSSH
  // (`ssh-keyscan -H github.com`).
  val githubKey: Text = t"ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl"

  val hosts: Text = t"""
    |# comment
    |github.com,gh ${githubKey}
    |[example.com]:2222 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS
    |*.internal,!bad.internal ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDcz0N3WrVZdgC9lXMxggLgs1OxMHSZjTzkCPsI268siCVGLUK6K2aSzdjr0RV03JXdsgWdiabWqlkMcW3oWn4R9JvoPMTowZ3cbUIZKQGXkPk9RESxRcAxHPgIayELROpJart9MzVvPIR3cVcG5CqQ10noIFWwqaCD/m5Idm+ZLcj1VlJ9NzHm1tHRyMaeLKxBSIsRGAPkir9hrbxD+S1qfStCaGY+uwrQCBb4KobM4cwivoTe6hIkivH4JHsHu1oZuozJzGhuJ5Db6l4LnXJD5L9wdq60+npNmIBVougLtQRzrvKVTr7s2WE46jpj/8LbUHw77oYI9rr/JdW+sCyH test@example
    |@revoked old.example.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS
    |@cert-authority *.corp ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS
    |this line is not a key
    |""".s.stripMargin.tt

  val githubBlob: Data =
    t"AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl".deserialize[Base64]

  val config: Text = t"""
    |# comment
    |Host work
    |  HostName git.example.com
    |  User alice
    |  Port 2222
    |  IdentityFile ~/.ssh/id_work
    |
    |Host *.example.com !git.example.com
    |    User bob
    |
    |Match host foo
    |  User ignored
    |
    |Host *
    |  IdentityFile ~/.ssh/id_ed25519
    |  User=carol
    |  HostName %h.lan
    |""".s.stripMargin.tt

  def run(): Unit =
    suite(m"Wire encoding"):
      test(m"Every field type round-trips"):
        val writer = Wire.Writer()
        writer.byte(20)
        writer.boolean(true)
        writer.uint32(-1)
        writer.uint64(1L << 40)
        writer.string(ed25519Blob)
        writer.text(t"ssh-userauth")
        writer.mpint(BigInt("123456789012345678901234567890"))
        writer.mpint(BigInt(0))
        writer.mpint(BigInt(-1))
        writer.names(List(t"curve25519-sha256", t"ecdh-sha2-nistp256"))
        writer.names(Nil)
        val reader = Wire.Reader(writer.data)
        val fields =
          ( reader.byte(), reader.boolean(), reader.uint32(), reader.uint64(),
            Array.same(reader.string(), ed25519Blob), reader.text(),
            reader.mpint(), reader.mpint(), reader.mpint(), reader.names(), reader.names() )
        reader.end()
        fields
      . assert(_ == (20.toByte, true, -1, 1L << 40, true, t"ssh-userauth",
          BigInt("123456789012345678901234567890"), BigInt(0), BigInt(-1),
          List(t"curve25519-sha256", t"ecdh-sha2-nistp256"), Nil))

      test(m"An mpint is encoded as RFC 4251 prescribes"):
        val writer = Wire.Writer()
        writer.mpint(BigInt("9a378f9b2e332a7", 16))
        writer.mpint(BigInt(0x80))
        writer.mpint(BigInt(-0x1234))
        writer.data.serialize[Hex]
      . assert(_ == t"0000000809A378F9B2E332A7" + t"00000002" + t"0080" + t"00000002" + t"EDCC")

      test(m"A truncated string is reported"):
        val writer = Wire.Writer()
        writer.uint32(10)
        writer.bytes(Array.fill[Byte](3)(0))
        capture[Wire.Error](Wire.Reader(writer.data).string()).reason
      . assert(_ == Wire.Error.Reason.Truncated)

      test(m"An oversized length is reported before any allocation"):
        val writer = Wire.Writer()
        writer.uint32(-1)
        capture[Wire.Error](Wire.Reader(writer.data).string()).reason
      . assert(_ == Wire.Error.Reason.Oversized(0xffffffffL))

      test(m"Trailing bytes are reported by end"):
        val writer = Wire.Writer()
        writer.uint32(7)
        writer.byte(1)
        val reader = Wire.Reader(writer.data)
        reader.uint32()
        capture[Wire.Error](reader.end()).reason
      . assert(_ == Wire.Error.Reason.Trailing(1))

    suite(m"known_hosts"):
      val known = KnownHosts(hosts)

      test(m"Unparseable lines are skipped"):
        known.entries.size
      . assert(_ == 5)

      test(m"A host's key is known"):
        known.verify(t"github.com", 22, t"ssh-ed25519", githubBlob)
      . assert(_ == KnownHosts.Verdict.Known)

      test(m"Hostnames match case-insensitively and by alias"):
        known.verify(t"GH", 22, t"ssh-ed25519", githubBlob)
      . assert(_ == KnownHosts.Verdict.Known)

      test(m"A different key of the same type for a known host has changed"):
        known.verify(t"github.com", 22, t"ssh-ed25519", ed25519Blob)
      . assert(_ == KnownHosts.Verdict.Changed)

      test(m"A key of another type for a known host is unknown"):
        known.verify(t"github.com", 22, t"ssh-rsa", rsaBlob)
      . assert(_ == KnownHosts.Verdict.Unknown)

      test(m"An unknown host is unknown"):
        known.verify(t"nowhere.example", 22, t"ssh-ed25519", ed25519Blob)
      . assert(_ == KnownHosts.Verdict.Unknown)

      test(m"A non-default port matches its bracketed entry only"):
        ( known.verify(t"example.com", 2222, t"ssh-ed25519", ed25519Blob),
          known.verify(t"example.com", 22, t"ssh-ed25519", ed25519Blob) )
      . assert(_ == (KnownHosts.Verdict.Known, KnownHosts.Verdict.Unknown))

      test(m"Wildcards match and negations exclude"):
        ( known.verify(t"db.internal", 22, t"ssh-rsa", rsaBlob),
          known.verify(t"bad.internal", 22, t"ssh-rsa", rsaBlob) )
      . assert(_ == (KnownHosts.Verdict.Known, KnownHosts.Verdict.Unknown))

      test(m"A revoked key is revoked"):
        known.verify(t"old.example.com", 22, t"ssh-ed25519", ed25519Blob)
      . assert(_ == KnownHosts.Verdict.Revoked)

      test(m"A certificate authority entry does not vouch for a plain key"):
        known.verify(t"x.corp", 22, t"ssh-ed25519", ed25519Blob)
      . assert(_ == KnownHosts.Verdict.Unknown)

      test(m"A hashed entry matches its host"):
        // `ssh-keyscan -H` output for `example.net` with the Ed25519 key above.
        val salt = t"c2FsdHNhbHRzYWx0c2FsdHNhbHQ=".deserialize[Base64]
        val name = Array.unsafeFrozen("example.net".getBytes("UTF-8").nn)
        val hash = summon[Crypto].hmac(t"HmacSHA1").mac(salt, name)
        val line = t"|1|${salt.serialize[Base64]}|${hash.serialize[Base64]} ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS"
        ( KnownHosts(line).verify(t"example.net", 22, t"ssh-ed25519", ed25519Blob),
          KnownHosts(line).verify(t"example.org", 22, t"ssh-ed25519", ed25519Blob) )
      . assert(_ == (KnownHosts.Verdict.Known, KnownHosts.Verdict.Unknown))

      test(m"A line is written as ssh-keyscan writes it"):
        KnownHosts.line(t"example.com", 2222, t"ssh-ed25519", ed25519Blob)
      . assert(_ == t"[example.com]:2222 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBGH6R8uL9Aww7+jnLISnQu3kiglJKYyOcKlvCJ7v+xS")

    suite(m"ssh_config"):
      val resolved = SshConfig(config)

      test(m"A named host resolves its own block, with the first value winning"):
        resolved.resolve(t"work")
      . assert(_ == SshConfig.Resolution(t"git.example.com", t"alice", 2222,
          List(t"~/.ssh/id_work", t"~/.ssh/id_ed25519")))

      test(m"A wildcard block applies, and a negation excludes"):
        (resolved.resolve(t"dev.example.com").user, resolved.resolve(t"git.example.com").user)
      . assert(_ == (t"bob", t"carol"))

      test(m"Defaults expand %h and fall back to the host itself"):
        (resolved.resolve(t"other").hostName, resolved.resolve(t"other").port)
      . assert(_ == (t"other.lan", 22))

      test(m"A Match block's contents are not applied"):
        resolved.resolve(t"foo").user
      . assert(_ == t"carol")

      test(m"An empty configuration resolves to the host itself"):
        SshConfig(t"").resolve(t"example.com")
      . assert(_ == SshConfig.Resolution(t"example.com", Unset, 22, Nil))

    // The transport is exercised against a real server: the OpenSSH `sshd` every macOS and most
    // Linux systems carry, run unprivileged on a free port with throwaway host keys, and skipped,
    // with a notice, where there is none. Each test starts its own server: fume terminates a
    // test's child processes when the test ends, so one cannot be shared across tests.
    val sshd: jnf.Path = jnf.Path.of("/usr/sbin/sshd").nn

    if !jnf.Files.isExecutable(sshd)
    then java.lang.System.out.nn.println("No /usr/sbin/sshd; skipping the transport tests.")
    else supervise:
      suite(m"Transport against sshd"):
        import internetAccess.online

        val directory = jnf.Files.createTempDirectory(jnf.Path.of("/tmp").nn, "susurration-").nn

        def file(name: Text): Text = directory.resolve(name.s).nn.toString.tt

        def keygen(kind: Text, name: Text): Unit =
          sh"ssh-keygen -q -t $kind -N ${t""} -f ${file(name)}".exec[Exit]()

        keygen(t"ed25519", t"host_ed25519")
        keygen(t"ecdsa", t"host_ecdsa")
        keygen(t"rsa", t"host_rsa")
        keygen(t"ed25519", t"user_ed25519")
        keygen(t"ecdsa", t"user_ecdsa")
        keygen(t"rsa", t"user_rsa")
        keygen(t"ed25519", t"stranger")

        def contents(name: Text): Text = jnf.Files.readString(directory.resolve(name.s).nn).nn.tt

        val authorized = List(t"user_ed25519.pub", t"user_ecdsa.pub", t"user_rsa.pub")
        val authorizedKeys = authorized.map(contents).join(t"\n").s
        jnf.Files.writeString(directory.resolve("authorized_keys").nn, authorizedKeys)

        val user: Text = java.lang.System.getProperty("user.name").nn.tt

        val configuration = t"""ListenAddress 127.0.0.1
          |HostKey ${file(t"host_ed25519")}
          |HostKey ${file(t"host_ecdsa")}
          |HostKey ${file(t"host_rsa")}
          |PidFile none
          |AuthorizedKeysFile ${file(t"authorized_keys")}
          |StrictModes no
          |PasswordAuthentication no
          |KbdInteractiveAuthentication no
          |AcceptEnv SUSURRATION_*
          |UsePAM no
          |LogLevel DEBUG1
          |""".s.stripMargin

        jnf.Files.writeString(directory.resolve("sshd_config").nn, configuration)
        val unrecorded: Text = t"ssh-ed25519 ${ed25519Blob.serialize[Base64]}"

        def hostKeyLine(name: Text): Text =
          jnf.Files.readString(directory.resolve(name.s).nn).nn.tt.trim

        // A server on a free port, for the duration of the block.
        def server[result](block: Int => result): result =
          val port = summon[Tcp is Allocatable].unused()

          val process =
            ProcessBuilder
              ( "/usr/sbin/sshd", "-D", "-p", port.toString, "-E", file(t"sshd.log").s, "-f",
                file(t"sshd_config").s )
            . start().nn

          // The server is ready when the port accepts a connection.
          def ready(attempts: Int): Boolean =
            attempts > 0 && (
              safely { java.net.Socket("127.0.0.1", port).close(); true }.or(false) ||
              { Thread.sleep(50); ready(attempts - 1) } )

          try
            if !ready(100) then abort(Ssh.Error(Ssh.Error.Reason.ClosedConnection))
            block(port)
          finally process.destroy()

        def connected[result](algorithms: Ssh.Algorithms = Ssh.Algorithms())
          ( block: Transport^ => result )
          ( using Ssh.HostKeys )
        :   result =

          given Ssh.Algorithms = algorithms

          server: port =>
            val endpoint: Endpoint[Tcp.Port] = Endpoint(t"127.0.0.1", Port[Tcp](port))

            // Connected as perihelion connects: `.duplex` requires an empty capture set, which
            // the `online`-capturing `Connectable` cannot satisfy from inside a method.
            val duplex = summon[(Endpoint[Tcp.Port] is Connectable)^].connect(endpoint, Unset)

            try
              val transport = Transport(duplex, t"127.0.0.1", port)
              transport.connect()
              try block(transport) finally transport.disconnect()
            finally duplex.close()

        given Ssh.HostKeys = Ssh.HostKeys.insecurelyAcceptAll

        test(m"The connection negotiates, verifies the host key and requests a service"):
          connected(): transport =>
            transport.service(t"ssh-userauth")
            (transport.serverVersion.let(_.starts(t"SSH-2.0-OpenSSH")), transport.negotiated)
        . assert(_ == (true, Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519",
            t"chacha20-poly1305@openssh.com")))

        for cipher <- List(t"aes256-gcm@openssh.com", t"aes128-gcm@openssh.com") do
          test(m"The connection works under $cipher"):
            connected(Ssh.Algorithms(cipher = List(cipher))): transport =>
              transport.service(t"ssh-userauth")
              transport.negotiated.let(_.cipher)
          . assert(_ == cipher)

        test(m"The connection works over ecdh-sha2-nistp256"):
          connected(Ssh.Algorithms(kex = List(t"ecdh-sha2-nistp256"))): transport =>
            transport.service(t"ssh-userauth")
            transport.negotiated.let(_.kex)
        . assert(_ == t"ecdh-sha2-nistp256")

        for hostKey <- List(t"ecdsa-sha2-nistp256", t"rsa-sha2-512", t"rsa-sha2-256") do
          test(m"The server's $hostKey host key verifies"):
            connected(Ssh.Algorithms(hostKey = List(hostKey))): transport =>
              transport.service(t"ssh-userauth")
              transport.negotiated.let(_.hostKey)
          . assert(_ == hostKey)

        test(m"A known_hosts file accepts the server's recorded key"):
          server: port =>
            val known = KnownHosts(t"[127.0.0.1]:$port ${hostKeyLine(t"host_ed25519.pub")}")
            given Ssh.HostKeys = Ssh.HostKeys.knownHosts(using known, summon)
            val endpoint: Endpoint[Tcp.Port] = Endpoint(t"127.0.0.1", Port[Tcp](port))
            val duplex = summon[(Endpoint[Tcp.Port] is Connectable)^].connect(endpoint, Unset)
            val transport = Transport(duplex, t"127.0.0.1", port)
            try
              transport.connect()
              transport.negotiated.let(_.hostKey)
            finally duplex.close()
        . assert(_ == t"ssh-ed25519")

        test(m"A known_hosts file rejects an unrecorded key"):
          server: port =>
            val known = KnownHosts(t"[127.0.0.1]:$port $unrecorded")
            given Ssh.HostKeys = Ssh.HostKeys.knownHosts(using known, summon)
            val endpoint: Endpoint[Tcp.Port] = Endpoint(t"127.0.0.1", Port[Tcp](port))
            val duplex = summon[(Endpoint[Tcp.Port] is Connectable)^].connect(endpoint, Unset)
            try capture[Ssh.Error](Transport(duplex, t"127.0.0.1", port).connect()).reason
            finally duplex.close()
        . assert(_ == Ssh.Error.Reason.HostKeyRejected(t"ssh-ed25519"))

        test(m"A server offering no common cipher is reported"):
          capture[Ssh.Error](connected(Ssh.Algorithms(cipher = List(t"none")))(_ => ())).reason
        . assert(_ == Ssh.Error.Reason.NoCommonAlgorithm(t"cipher"))

        test(m"A loaded key's wire blob is the one in its .pub file"):
          val key = contents(t"user_ed25519").read[PrivateKey[Ed25519] in Pem]
          Signer(key).blob.serialize[Base64]
        . assert: blob =>
            contents(t"user_ed25519.pub").cut(t" ") match
              case _ :: expected :: _ => blob == expected
              case _                  => false

        test(m"An Ed25519 key authenticates"):
          val key = contents(t"user_ed25519").read[PrivateKey[Ed25519] in Pem]
          connected()(_.authenticate(user, List(Signer(key))))
        . assert()

        test(m"An ECDSA key authenticates"):
          val key = contents(t"user_ecdsa").read[PrivateKey[Ecdsa[256]] in Pem]
          connected()(_.authenticate(user, List(Signer(key))))
        . assert()

        test(m"An RSA key authenticates under rsa-sha2-256"):
          val key = contents(t"user_rsa").read[PrivateKey[Rsa[3072]] in Pem]
          connected()(_.authenticate(user, List(Signer(key))))
        . assert()

        test(m"An unauthorized key is refused, naming the methods the server allows"):
          val key = contents(t"stranger").read[PrivateKey[Ed25519] in Pem]
          capture[Ssh.Error](connected()(_.authenticate(user, List(Signer(key))))).reason
        . assert(_ == Ssh.Error.Reason.Unauthenticated(t"publickey"))

        test(m"The first accepted signer of several authenticates"):
          val stranger = contents(t"stranger").read[PrivateKey[Ed25519] in Pem]
          val key = contents(t"user_ed25519").read[PrivateKey[Ed25519] in Pem]
          connected()(_.authenticate(user, List(Signer(stranger), Signer(key))))
        . assert()

        test(m"Keys held by an ssh-agent authenticate"):
          val socket = file(t"agent.sock")
          val agent = ProcessBuilder("/usr/bin/ssh-agent", "-D", "-a", socket.s).start().nn

          def bound: Boolean = jnf.Files.exists(jnf.Path.of(socket.s).nn)

          def ready(attempts: Int): Boolean =
            attempts > 0 && (bound || { Thread.sleep(50); ready(attempts - 1) })

          try
            ready(100)
            val add = ProcessBuilder("/usr/bin/ssh-add", "-q", file(t"user_ecdsa").s)
            add.environment().nn.put("SSH_AUTH_SOCK", socket.s)
            add.start().nn.waitFor()
            val signers = Agent(socket).identities()
            connected()(_.authenticate(user, signers))
            signers.map(_.algorithm)
          finally agent.destroy()
        . assert(_ == List(t"ecdsa-sha2-nistp256"))

        def sessioned[result](block: Session^ => result): result =
          server: port =>
            val key = contents(t"user_ed25519").read[PrivateKey[Ed25519] in Pem]
            val endpoint = Ssh.Endpoint(t"127.0.0.1", port, user, List(Signer(key)))
            endpoint.session: ssh ?=> block(ssh)

        test(m"A command runs and its output and status come back"):
          val result = sessioned(_.exec(t"echo hello"))
          (result.stdout.read[Text], result.stderr.length, result.status)
        . assert(_ == (t"hello\n", 0, 0))

        test(m"A command's exit status is reported"):
          sessioned(_.exec(t"exit 3").status)
        . assert(_ == 3)

        test(m"Standard error is kept apart from standard output"):
          val result = sessioned(_.exec(t"echo err 1>&2; echo out"))
          (result.stdout.read[Text], result.stderr.read[Text])
        . assert(_ == (t"out\n", t"err\n"))

        test(m"Standard input reaches the command"):
          sessioned(_.exec(t"tr a-z A-Z", t"whisper".bytestream).stdout.read[Text])
        . assert(_ == t"WHISPER")

        test(m"Megabytes pass through in both directions, across window adjustments"):
          val input: Data = Array.tabulate[Byte](3 << 20)(index => (index % 251).toByte)
          Array.same(sessioned(_.exec(t"cat", input).stdout), input)
        . assert(identity)

        test(m"An environment variable the server accepts reaches the command"):
          val environment = List((t"SUSURRATION_WORD", t"susurrus"))
          sessioned(_.exec(t"echo $$SUSURRATION_WORD", Data(), environment))
        . assert(_.stdout.read[Text] == t"susurrus\n")

        test(m"A guillotine command keeps an argument with spaces and quotes intact"):
          val argument = t"two words, a 'quote' and a $$sign"
          sessioned(_.exec(sh"printf %s $argument").stdout.read[Text])
        . assert(_ == t"two words, a 'quote' and a $$sign")

        test(m"A guillotine command's arguments are not re-split by the remote shell"):
          // `$0` is `x`; the two positional parameters are `a b` and `c`.
          sessioned(_.exec(sh"sh -c ${t"echo $$#"} x ${t"a b"} c").stdout.read[Text])
        . assert(_ == t"2\n")

        test(m"Several commands run over one connection"):
          sessioned: ssh =>
            (ssh.exec(t"echo one").stdout.read[Text], ssh.exec(t"echo two").stdout.read[Text])
        . assert(_ == (t"one\n", t"two\n"))

        test(m"A channel streams its output chunk by chunk"):
          sessioned: ssh =>
            ssh.channel: channel =>
              channel.exec(t"seq 1 200000")
              channel.eof()
              val chunks: List[Data] = channel.stdout().to[List]
              (chunks.size > 1, chunks.map(_.length).fuse(0)(state + next), channel.exitStatus)
        . assert(_ == (true, 1288895, 0))

        test(m"A refused command is reported"):
          sessioned(_.channel(_.request(t"nonsense", true)(_ => ())))
        . assert(_ == false)

        test(m"Many packets pass in both directions under each cipher"):
          // `ssh-userauth` is requested repeatedly: each request is a packet each way.
          List(t"chacha20-poly1305@openssh.com", t"aes256-gcm@openssh.com").map: cipher =>
            connected(Ssh.Algorithms(cipher = List(cipher))): transport =>
              (0 until 20).each(_ => transport.service(t"ssh-userauth"))
              true
        . assert(_ == List(true, true))
