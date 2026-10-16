## SSH

### About

Susurration is a native SSH-2 client. It is being built in stages: this release has the wire
encoding, the two files every client reads, `known_hosts` and `~/.ssh/config`, and the transport
layer — the key exchange and encrypted packets over a coaxial connection — public-key
authentication, and the connection protocol that runs commands.

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

An `Ssh.Transport` over a connected `Duplex` performs the identification exchange and key exchange
with `connect`, checking the server's host key against the `Ssh.HostKeys` policy in scope — a
`KnownHosts` file, or `Ssh.HostKeys.trustOnFirstUse(known)` — and then moves encrypted packets
with `send` and `receive`. The algorithms offered are `Ssh.Algorithms`, whose default prefers
Curve25519, Ed25519 and ChaCha20-Poly1305:

<!-- doccheck: skip -->
```scala
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

Endpoint(t"github.com", Port[Tcp](22)).duplex: duplex =>
  val transport = Ssh.Transport(duplex, t"github.com", 22)
  transport.connect()
  transport.service(t"ssh-userauth")
  transport.negotiated   // Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519", t"chacha20-poly1305@openssh.com")
```

### Running commands

Everything above is assembled by a session on an `Ssh.Endpoint`: it connects, checks the host
key against the `Ssh.HostKeys` policy in scope, authenticates with the endpoint's signers, and
lends a `Ssh.Session` that runs commands over channels of the connection protocol:

<!-- doccheck: skip -->
```scala
import cloaks.heapCloak

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val endpoint = Ssh.Endpoint(t"example.com", 22, t"alice", List(Ssh.Signer(key)))

endpoint.session: ssh ?=>
  val result = ssh.exec(t"uname -a")
  result.stdout.read[Text]   // the output
  result.status              // 0
```

`exec` writes the input, collects both output streams and waits for the exit status. The command
may be a `Text` the remote shell interprets, or a Guillotine `Command`, whose arguments are
quoted one by one so that a value with spaces or shell characters stays a single argument:

<!-- doccheck: skip -->
```scala
val branch = t"feature/one two"
endpoint.session: ssh ?=>
  ssh.exec(sh"git log -1 --format=%s $branch").stdout.read[Text]
```

For a protocol that interleaves reading and writing, `ssh.channel` lends the channel itself,
whose `stdout()` is a lazy chain of chunks and whose `write` and `eof` drive the command's input.

### Git over SSH

Octogenarian's native git backend reaches remote repositories through this client with the
`gitTransports.soundnessSsh` transport: no `ssh` binary is spawned, the host key is checked
against the `Ssh.HostKeys` policy in scope, and the identities come from `Ssh.Credentials` —
loaded keys, or everything an `ssh-agent` holds:

<!-- doccheck: skip -->
```scala
import gitBackends.soundnessGit, gitTransports.soundnessSsh

given Ssh.Credentials = Ssh.Credentials.agent(t"/tmp/ssh-agent.sock")
given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

Git.clone(t"git@github.com:propensive/soundness.git", directory)
```

### Authentication

A connected transport authenticates with public keys (RFC 4252): an `Ssh.Signer` is a loaded
`PrivateKey` of a cipher OpenSSH knows, or a key held by an `ssh-agent`, which signs without ever
releasing the key. The first signer the server accepts wins:

<!-- doccheck: skip -->
```scala
import cloaks.heapCloak

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val agent = Ssh.Agent(t"/tmp/ssh-agent.sock")
transport.authenticate(t"git", Ssh.Signer(key) :: agent.identities())
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
