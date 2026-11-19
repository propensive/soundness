- ## SSH
+ ## SSH

- ### About
+ ### Présentation

- This is a native SSH-2 client, built in stages. This release has the wire encoding, the two
- files which every client reads, which are `known_hosts` and `~/.ssh/config`, and the transport
- layer: the key exchange and encrypted packets over a connection. It has public-key
- authentication, and the connection protocol which runs commands.
+ Il s'agit d'un client SSH-2 natif, construit par étapes. Cette version comprend l'encodage sur
+ le réseau, les deux fichiers que lit tout client, `known_hosts` et `~/.ssh/config`, et la
+ couche de transport : l'échange de clés et les paquets chiffrés sur une connexion. Elle
+ comprend l'authentification par clé publique, et le protocole de connexion qui exécute des
+ commandes.

- ### On SSH
+ ### SSH

- Programs usually reach SSH by spawning the `ssh` binary, and then they depend on its presence,
- its version, and its configuration on the machine they happen to run on. A client in the same
- process depends on none of those. It checks the host key against a policy which is stated in
- the code, and takes its identities from loaded keys or from an agent.
+ Les programmes accèdent d'ordinaire à SSH en lançant le binaire `ssh`, et dépendent alors de
+ sa présence, de sa version et de sa configuration sur la machine où ils se trouvent s'exécuter.
+ Un client dans le même processus ne dépend de rien de tout cela. Il vérifie la clé d'hôte selon
+ une politique énoncée dans le code, et prend ses identités dans des clés chargées ou auprès d'un
+ agent.

- Everything comes from the `soundness` package. The other imports choose encodings and an error
- [strategy], and let the samples print their results:
+ Tout provient du package `soundness`. Les autres imports choisissent des encodages et une
+ [stratégie] d'erreur, et permettent aux exemples d'afficher leurs résultats :

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

- The samples which parse files and encode messages run here. Those which open a connection need a
- server, so they are compiled but not run.
+ Les exemples qui analysent des fichiers et encodent des messages s'exécutent ici. Ceux qui
+ ouvrent une connexion ont besoin d'un serveur : ils sont donc compilés mais pas exécutés.

- ### Host keys
+ ### Clés d'hôte

- A `known_hosts` file records the key which each host has presented before. Reading it gives a
- `KnownHosts`. `verify` takes a host, and the key it now presents, as the blob which OpenSSH
- writes after the key type, and answers with a `KnownHosts.Verdict`:
+ Un fichier `known_hosts` enregistre la clé que chaque hôte a présentée auparavant. Le lire donne
+ un `KnownHosts`. `verify` prend un hôte, et la clé qu'il présente maintenant, sous la forme du
+ blob qu'OpenSSH écrit après le type de clé, et répond par un `KnownHosts.Verdict` :

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
- column  Verdict
- column  When
- row  `Revoked`  an entry for the host carries this key with the `@revoked` marker
- row  `Known`  otherwise, an entry for the host carries this key
- row  `Changed`  the host has unrevoked entries of this key's type, but not this key
- row  `Unknown`  the host has no unrevoked entries of this key's type
+ column  Verdict
+ column  Quand
+ row  `Revoked`  une entrée pour l'hôte porte cette clé avec le marqueur `@revoked`
+ row  `Known`  sinon, une entrée pour l'hôte porte cette clé
+ row  `Changed`  l'hôte a des entrées non révoquées du type de cette clé, mais pas cette clé
+ row  `Unknown`  l'hôte n'a aucune entrée non révoquée du type de cette clé
```

- An entry with the `@cert-authority` marker takes no part in the verdict. The syntax of the file
- is understood as OpenSSH writes it:
+ Une entrée portant le marqueur `@cert-authority` ne prend aucune part au verdict. La syntaxe du
+ fichier est comprise telle qu'OpenSSH l'écrit :

```tabular
- column  Syntax
- column  Means
- row  `*.example.com`, `?`  a hostname pattern, matched case-insensitively
- row  `!excluded`  a negated pattern
- row  `[host]:port`  an entry for a non-default port
- row  `|1|salt|hash`  a hashed entry: an HMAC-SHA1 of the hostname under the salt
- row  `@revoked`  a marker revoking the line's key
- row  `@cert-authority`  a marker for a certificate authority's key
+ column  Syntaxe
+ column  Signification
+ row  `*.example.com`, `?`  un motif de nom d'hôte, comparé sans tenir compte de la casse
+ row  `!excluded`  un motif nié
+ row  `[host]:port`  une entrée pour un port autre que celui par défaut
+ row  `|1|salt|hash`  une entrée hachée : un HMAC-SHA1 du nom d'hôte avec le sel
+ row  `@revoked`  un marqueur qui révoque la clé de la ligne
+ row  `@cert-authority`  un marqueur pour la clé d'une autorité de certification
```

- A line which is not a key is skipped, as OpenSSH skips it.
+ Une ligne qui n'est pas une clé est ignorée, comme OpenSSH l'ignore.

- ### Client configuration
+ ### Configuration du client

- `~/.ssh/config` says how to reach a host. `SshConfig` reads its `Host` blocks, and resolves the
- hostname, user, port and identity files for a destination. As in OpenSSH, the first value found
- wins:
+ `~/.ssh/config` dit comment atteindre un hôte. `SshConfig` lit ses blocs `Host`, et résout le
+ nom d'hôte, l'utilisateur, le port et les fichiers d'identité pour une destination. Comme dans
+ OpenSSH, la première valeur trouvée l'emporte :

```scala
val config = SshConfig(t"""
Host work
  HostName git.example.com
  User hamlet
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

- ### The transport
+ ### Le transport

- An `Ssh.Transport` over a connected `Duplex` performs the identification exchange and the key
- exchange with `connect`. It checks the server's host key against the `Ssh.HostKeys` policy in
- scope, which is a `KnownHosts` file, or `Ssh.HostKeys.trustOnFirstUse(known)`. It then moves
- encrypted packets with `send` and `receive`. The algorithms offered are `Ssh.Algorithms`, whose
- default prefers Curve25519, Ed25519 and ChaCha20-Poly1305:
+ Un `Ssh.Transport` sur un `Duplex` connecté effectue l'échange d'identification et l'échange de
+ clés avec `connect`. Il vérifie la clé d'hôte du serveur selon la politique `Ssh.HostKeys` dans
+ la portée, qui est un fichier `KnownHosts`, ou `Ssh.HostKeys.trustOnFirstUse(known)`. Il
+ transporte ensuite des paquets chiffrés avec `send` et `receive`. Les algorithmes proposés sont
+ `Ssh.Algorithms`, dont la valeur par défaut privilégie Curve25519, Ed25519 et
+ ChaCha20-Poly1305 :

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

- Against GitHub, `negotiated` would be
- `Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519", t"chacha20-poly1305@openssh.com")`.
+ Face à GitHub, `negotiated` vaudrait
+ `Transport.Negotiated(t"curve25519-sha256", t"ssh-ed25519", t"chacha20-poly1305@openssh.com")`.

- ### Running commands
+ ### Exécuter des commandes

- A session on an `Ssh.Endpoint` assembles everything above. It connects, checks the host key
- against the `Ssh.HostKeys` policy in scope, authenticates with the signers of the endpoint, and
- lends an `Ssh.Session` which runs commands over channels of the connection protocol. Here the
- key is read from `keyText`, the content of a private key file:
+ Une session sur un `Ssh.Endpoint` assemble tout ce qui précède. Elle se connecte, vérifie la clé
+ d'hôte selon la politique `Ssh.HostKeys` dans la portée, s'authentifie avec les signataires de
+ l'endpoint, et prête une `Ssh.Session` qui exécute des commandes sur des canaux du protocole de
+ connexion. Ici, la clé est lue dans `keyText`, le contenu d'un fichier de clé privée :

```scala fuel
typecheck
##
import cloaks.heapCloak
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val key = keyText.read[PrivateKey[Ed25519] in Pem]
val endpoint = Ssh.Endpoint(t"example.com", 22, t"hamlet", List(Ssh.Signer(key)))

endpoint.session: ssh ?=>
  val result = ssh.exec(t"uname -a")
  val output: Text = result.stdout.read[Text]
  val status: Optional[Int] = result.status
```

- `exec` writes the input, collects both output streams, and waits for the exit status. The
- command may be a `Text` which the remote shell interprets, or a [`Command`](processes.md),
- whose arguments are quoted one by one, so a value with spaces or shell characters stays a
- single argument:
+ `exec` écrit l'entrée, recueille les deux flux de sortie, et attend le statut de sortie. La
+ commande peut être un `Text` qu'interprète le shell distant, ou une
+ [`Command`](processes.md), dont les arguments sont mis entre guillemets un par un : une valeur
+ contenant des espaces ou des caractères du shell reste donc un seul argument :

```scala fuel
typecheck
##
import internetAccess.online, socketBackends.javaBaseSockets, logging.silentLogging

given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val branch = t"feature/one two"
val endpoint = Ssh.Endpoint(t"example.com", 22, t"hamlet", Nil)

endpoint.session: ssh ?=>
  ssh.exec(sh"git log -1 --format=%s $branch").stdout.read[Text]
```

- For a protocol which interleaves reading and writing, `ssh.channel` lends the channel itself.
- Its `stdout()` is a lazy chain of chunks, and its `write` and `eof` drive the input of the
- command.
+ Pour un protocole qui entrelace lecture et écriture, `ssh.channel` prête le canal lui-même. Son
+ `stdout()` est une chaîne paresseuse de morceaux, et ses `write` et `eof` pilotent l'entrée de
+ la commande.

- ### Git over SSH
+ ### Git sur SSH

- The native [Git](git.md#choosing-a-backend) backend reaches remote repositories through this
- client, with the `gitTransports.soundnessSsh` transport. No `ssh` binary is spawned. The host
- key is checked against the `Ssh.HostKeys` policy in scope, and the identities come from
- `Ssh.Credentials`, which are loaded keys, or everything which an `ssh-agent` holds:
+ Le backend [Git](git.md#choosing-a-backend) natif atteint les dépôts distants par ce client,
+ avec le transport `gitTransports.soundnessSsh`. Aucun binaire `ssh` n'est lancé. La clé d'hôte
+ est vérifiée selon la politique `Ssh.HostKeys` dans la portée, et les identités proviennent de
+ `Ssh.Credentials`, qui sont des clés chargées, ou tout ce que détient un `ssh-agent` :

```scala fuel
typecheck
##
import gitBackends.soundnessGit, gitTransports.soundnessSsh
import pathInterfaces.pathOnLinux, temporaryDirectories.javaBaseTemporaryDirectory

given Ssh.Credentials = Ssh.Credentials.agent(t"/tmp/ssh-agent.sock")
given Ssh.HostKeys = Ssh.HostKeys.trustOnFirstUse(KnownHosts(t""))

val directory = temporaryDirectory[Path on Local] / "soundness"
Git.clone(t"git@github.com:propensive/soundness.git", directory)
```

- ### Authentication
+ ### Authentification

- A connected transport authenticates with public keys, as RFC 4252 describes. An `Ssh.Signer`
- is a loaded `PrivateKey` of a cipher which OpenSSH knows, or a key held by an `ssh-agent`,
- which signs without ever releasing the key. The first signer which the server accepts wins:
+ Un transport connecté s'authentifie par clés publiques, comme le décrit la RFC 4252. Un
+ `Ssh.Signer` est une `PrivateKey` chargée d'un algorithme que connaît OpenSSH, ou une clé
+ détenue par un `ssh-agent`, qui signe sans jamais libérer la clé. Le premier signataire
+ qu'accepte le serveur l'emporte :

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

- ### The wire encoding
+ ### L'encodage de transmission

- Every SSH message is a sequence of the types which RFC 4251 defines. They are read and written
- with `Wire.Reader` and `Wire.Writer`:
+ Chaque message SSH est une suite des types que définit la RFC 4251. Ils se lisent et s'écrivent
+ avec `Wire.Reader` et `Wire.Writer` :

```scala fuel
compile
##
val writer = Wire.Writer()
writer.text(t"ssh-userauth")
writer.names(List(t"curve25519-sha256", t"ecdh-sha2-nistp256"))

val name: Text = Wire.Reader(writer.data).text()
```

- The reader gives back `t"ssh-userauth"`, the first field which was written.
+ Le lecteur renvoie `t"ssh-userauth"`, le premier champ qui a été écrit.

- ### Related
+ ### Voir aussi

- - [Git](git.md) uses this client as a transport for its native backend.
- - [Cryptography](cryptography.md) describes the keys which sign an authentication.
- - [Sockets](sockets.md) describes the `Duplex` connection which a transport runs over.
- - [Processes](processes.md) describes the `sh"…"` commands which `exec` quotes.
- - [Base encoding](base-encoding.md) describes the Base64 in which keys are written.
+ - [Git](git.md) utilise ce client comme transport pour son backend natif.
+ - [Cryptographie](cryptography.md) décrit les clés qui signent une authentification.
+ - [Sockets](sockets.md) décrit la connexion `Duplex` sur laquelle fonctionne un transport.
+ - [Processus](processes.md) décrit les commandes `sh"…"` que `exec` met entre guillemets.
+ - [Encodage en base](base-encoding.md) décrit le Base64 dans lequel s'écrivent les clés.
