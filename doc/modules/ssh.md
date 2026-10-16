## SSH

### About

Susurration is a native SSH-2 client. It is being built in stages: this release has the wire
encoding, the two files every client reads, `known_hosts` and `~/.ssh/config`, and the transport
layer — the key exchange and encrypted packets over a coaxial connection — and public-key
authentication; the connection protocol follows.

```scala
import soundness.*
import strategies.throwUnsafely
import providers.javaBaseProvider
import alphabets.base64Standard
```

### Host keys

A `known_hosts` file records the key each host has presented before. Reading it gives a
`KnownHosts`, and `verify` answers, for a host and the key it now presents (as the blob OpenSSH
writes after the key type), whether the key is `Known`, `Unknown`, `Changed` or `Revoked`:

```scala
val hosts = KnownHosts(t"""github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl""")
val key = t"AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl".deserialize[Base64]
hosts.verify(t"github.com", 22, t"ssh-ed25519", key)   // KnownHosts.Verdict.Known
```

Patterns (`*.example.com`, `!excluded`), `[host]:port` entries for non-default ports, hashed
`|1|…` entries and the `@revoked` and `@cert-authority` markers are all understood, and a line
that is not a key is skipped as OpenSSH skips it.

### Client configuration

`~/.ssh/config` says how to reach a host. `SshConfig` reads its `Host` blocks and resolves the
hostname, user, port and identity files for a destination, with the first value obtained winning
as in OpenSSH:

```scala
val config = SshConfig(t"""
Host work
  HostName git.example.com
  User alice
  Port 2222
  IdentityFile ~/.ssh/id_work
""")

config.resolve(t"work")   // SshConfig.Resolution(t"git.example.com", t"alice", 2222, List(t"~/.ssh/id_work"))
```

### The transport

A `Transport` over a connected `Duplex` performs the identification exchange and key exchange
with `connect`, checking the server's host key against the `Ssh.HostKeys` policy in scope — a
`KnownHosts` file, or `Ssh.HostKeys.trustOnFirstUse(known)` — and then moves encrypted packets
with `send` and `receive`. The algorithms offered are `Ssh.Algorithms`, whose default prefers
Curve25519, Ed25519 and ChaCha20-Poly1305:

<!-- doccheck: skip -->
```scala
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

Endpoint(t"github.com", Port[Tcp](22)).duplex: duplex =>
  val transport = Transport(duplex, t"github.com", 22)
  transport.connect()
  transport.service(t"ssh-userauth")
  transport.negotiated   // Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519", t"chacha20-poly1305@openssh.com")
```

### Authentication

A connected transport authenticates with public keys (RFC 4252): a `Signer` is a loaded
`PrivateKey` of a cipher OpenSSH knows, or a key held by an `ssh-agent`, which signs without ever
releasing the key. The first signer the server accepts wins:

<!-- doccheck: skip -->
```scala
import cloaks.heapCloak

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val agent = Agent(t"/tmp/ssh-agent.sock")
transport.authenticate(t"git", Signer(key) :: agent.identities())
```

### The wire encoding

Every SSH message is a sequence of the types RFC 4251 defines, read and written with
`Wire.Reader` and `Wire.Writer`:

```scala
val writer = Wire.Writer()
writer.text(t"ssh-userauth")
writer.names(List(t"curve25519-sha256", t"ecdh-sha2-nistp256"))
Wire.Reader(writer.data).text()   // t"ssh-userauth"
```
