## SSH

### About

This is a native SSH-2 client, built in stages. This release has the wire encoding, the two
files which every client reads, which are `known_hosts` and `~/.ssh/config`, and the transport
layer: the key exchange and encrypted packets over a connection. It has public-key
authentication, and the connection protocol which runs commands.

### On SSH

Programs usually reach SSH by spawning the `ssh` binary, and then they depend on its presence,
its version, and its configuration on the machine they happen to run on. A client in the same
process depends on none of those. It checks the host key against a policy which is stated in
the code, and takes its identities from loaded keys or from an agent.

Everything comes from the `soundness` package. The other imports choose encodings and an error
[strategy], and let the samples print their results:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import providers.javaBaseProvider
import alphabets.base64Standard
import charsets.utf8Charset
import codepages.utf8Codepage
import textSanitizers.skipSanitizer
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

The samples which parse files and encode messages run here. Those which open a connection need a
server, so they are compiled but not run.

### Host keys

A `known_hosts` file records the key which each host has presented before. Reading it gives a
`KnownHosts`. `verify` takes a host, and the key it now presents, as the blob which OpenSSH
writes after the key type, and answers with a `KnownHosts.Verdict`:

```scala
val hosts = KnownHosts(t"""github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl""")
val key = t"AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl".deserialize[Base64]
```

```scala fuel
run
##
Out.println(hosts.verify(t"github.com", 22, t"ssh-ed25519", key).inspect)
Out.println(hosts.verify(t"gitlab.com", 22, t"ssh-ed25519", key).inspect)
```

```tabular
column  Verdict
column  When
row  `Revoked`  an entry for the host carries this key with the `@revoked` marker
row  `Known`  otherwise, an entry for the host carries this key
row  `Changed`  the host has unrevoked entries of this key's type, but not this key
row  `Unknown`  the host has no unrevoked entries of this key's type
```

An entry with the `@cert-authority` marker takes no part in the verdict. The syntax of the file
is understood as OpenSSH writes it:

```tabular
column  Syntax
column  Means
row  `*.example.com`, `?`  a hostname pattern, matched case-insensitively
row  `!excluded`  a negated pattern
row  `[host]:port`  an entry for a non-default port
row  `|1|salt|hash`  a hashed entry: an HMAC-SHA1 of the hostname under the salt
row  `@revoked`  a marker revoking the line's key
row  `@cert-authority`  a marker for a certificate authority's key
```

A line which is not a key is skipped, as OpenSSH skips it.

### Client configuration

`~/.ssh/config` says how to reach a host. `SshConfig` reads its `Host` blocks, and resolves the
hostname, user, port and identity files for a destination. As in OpenSSH, the first value found
wins:

```scala
val config = SshConfig(t"""
Host work
  HostName git.example.com
  User alice
  Port 2222
  IdentityFile ~/.ssh/id_work
""")

config.resolve(t"work")
```

```scala fuel
run
##
Out.println(config.resolve(t"work").inspect)
```

### The transport

An `Ssh.Transport` over a connected `Duplex` performs the identification exchange and the key
exchange with `connect`. It checks the server's host key against the `Ssh.HostKeys` policy in
scope, which is a `KnownHosts` file, or `Ssh.HostKeys.trustOnFirstUse(known)`. It then moves
encrypted packets with `send` and `receive`. The algorithms offered are `Ssh.Algorithms`, whose
default prefers Curve25519, Ed25519 and ChaCha20-Poly1305:

```scala fuel
typecheck
##
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

Endpoint(t"github.com", Port[Tcp](22)).duplex: duplex =>
  val transport = Ssh.Transport(duplex, t"github.com", 22)
  transport.connect()
  transport.service(t"ssh-userauth")
  val negotiated = transport.negotiated
```

Against GitHub, `negotiated` would be
`Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519", t"chacha20-poly1305@openssh.com")`.

### Running commands

A session on an `Ssh.Endpoint` assembles everything above. It connects, checks the host key
against the `Ssh.HostKeys` policy in scope, authenticates with the signers of the endpoint, and
lends an `Ssh.Session` which runs commands over channels of the connection protocol. Here the
key is read from `keyText`, the content of a private key file:

```scala fuel
typecheck
##
import cloaks.heapCloak
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val endpoint = Ssh.Endpoint(t"example.com", 22, t"alice", List(Ssh.Signer(key)))

endpoint.session: ssh ?=>
  val result = ssh.exec(t"uname -a")
  val output: Text = result.stdout.read[Text]
  val status: Optional[Int] = result.status
```

`exec` writes the input, collects both output streams, and waits for the exit status. The
command may be a `Text` which the remote shell interprets, or a [`Command`](processes.md),
whose arguments are quoted one by one, so a value with spaces or shell characters stays a
single argument:

```scala fuel
typecheck
##
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val branch = t"feature/one two"
val endpoint = Ssh.Endpoint(t"example.com", 22, t"alice", Nil)

endpoint.session: ssh ?=>
  ssh.exec(sh"git log -1 --format=%s $branch").stdout.read[Text]
```

For a protocol which interleaves reading and writing, `ssh.channel` lends the channel itself.
Its `stdout()` is a lazy chain of chunks, and its `write` and `eof` drive the input of the
command.

### Git over SSH

The native [Git](git.md#choosing-a-backend) backend reaches remote repositories through this
client, with the `gitTransports.soundnessSsh` transport. No `ssh` binary is spawned. The host
key is checked against the `Ssh.HostKeys` policy in scope, and the identities come from
`Ssh.Credentials`, which are loaded keys, or everything which an `ssh-agent` holds:

```scala fuel
typecheck
##
import gitBackends.soundnessGit, gitTransports.soundnessSsh
import pathInterfaces.pathOnLinux, temporaryDirectories.javaBaseTemporaryDirectory

given Ssh.Credentials = Ssh.Credentials.agent(t"/tmp/ssh-agent.sock")
given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val directory = temporaryDirectory[Path on Linux] / "soundness"
Git.clone(t"git@github.com:propensive/soundness.git", directory)
```

### Authentication

A connected transport authenticates with public keys, as RFC 4252 describes. An `Ssh.Signer`
is a loaded `PrivateKey` of a cipher which OpenSSH knows, or a key held by an `ssh-agent`,
which signs without ever releasing the key. The first signer which the server accepts wins:

```scala fuel
typecheck
##
import cloaks.heapCloak

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val agent = Ssh.Agent(t"/tmp/ssh-agent.sock")

Endpoint(t"github.com", Port[Tcp](22)).duplex: duplex =>
  val transport = Ssh.Transport(duplex, t"github.com", 22)
  transport.connect()
  transport.authenticate(t"git", Ssh.Signer(key) :: agent.identities())
```

### The wire encoding

Every SSH message is a sequence of the types which RFC 4251 defines. They are read and written
with `Wire.Reader` and `Wire.Writer`:

```scala fuel
compile
##
val writer = Wire.Writer()
writer.text(t"ssh-userauth")
writer.names(List(t"curve25519-sha256", t"ecdh-sha2-nistp256"))

val name: Text = Wire.Reader(writer.data).text()
```

The reader gives back `t"ssh-userauth"`, the first field which was written.

### Related

- [Git](git.md) uses this client as a transport for its native backend.
- [Cryptography](cryptography.md) describes the keys which sign an authentication.
- [Sockets](sockets.md) describes the `Duplex` connection which a transport runs over.
- [Processes](processes.md) describes the `sh"…"` commands which `exec` quotes.
- [Base encoding](base-encoding.md) describes the Base64 in which keys are written.
