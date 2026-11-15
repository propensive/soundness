- ## Daemons
+ ## Démons

- ### About
+ ### Présentation

- A command-line tool on the JVM pays the JVM's startup cost on every invocation, and loses
- the optimization which the just-in-time compiler had built up. That makes even a fast
- program feel slow at the shell.
+ Un outil en ligne de commande sur la JVM paie le coût de démarrage de la JVM à chaque
+ invocation, et perd l'optimisation que le compilateur à la volée avait accumulée. Cela rend
+ lent dans le shell même un programme rapide.

- Making the application resident removes the cost. The first invocation starts a daemon.
- Every later one is dispatched to the running process by a small native launcher, with its
- arguments, environment, working directory, standard streams and signals all forwarded
- faithfully.
+ Rendre l'application résidente supprime ce coût. La première invocation démarre un démon.
+ Chaque invocation suivante est transmise au processus en cours d'exécution par un petit
+ lanceur natif, avec ses arguments, son environnement, son répertoire de travail, ses flux
+ standard et ses signaux, tous fidèlement relayés.

- The change costs one word. The body of a
- [command-line application](cli.md) which is wrapped in `cli`, instead of in `application`,
- becomes a daemon. Packaging produces a single self-contained executable, which is the native
- launcher with the application inside, and the launcher can verify and apply signed upgrades
- of itself.
+ Le changement coûte un mot. Le corps d'une [application en ligne de commande](cli.md)
+ enveloppé dans `cli`, au lieu d'`application`, devient un démon. L'empaquetage produit un
+ unique exécutable autonome, qui est le lanceur natif avec l'application à l'intérieur, et le
+ lanceur peut vérifier et appliquer des mises à niveau signées de lui-même.

- ### On daemonized applications
+ ### À propos des applications en démon

- The idea is old. Nailgun kept a JVM warm for exactly this reason. But the details decide
- whether it can be trusted. Each invocation must behave *exactly* as a fresh process would:
- with its own environment and working directory, its own standard input and exit code, and
- with Ctrl-C reaching the right invocation, and not the daemon. And the daemon must manage
- itself: starting on demand, shutting down when it is idle, and surviving upgrades.
+ L'idée est ancienne. Nailgun gardait une JVM chaude exactement pour cette raison. Mais ce
+ sont les détails qui décident si l'on peut s'y fier. Chaque invocation doit se comporter
+ *exactement* comme le ferait un nouveau processus : avec son propre environnement et son
+ propre répertoire de travail, sa propre entrée standard et son propre code de sortie, et
+ avec un Ctrl-C qui atteint la bonne invocation, et non le démon. Et le démon doit se gérer
+ lui-même : démarrer à la demande, s'arrêter lorsqu'il est inactif, et survivre aux mises à
+ niveau.

- Here those details are handled in a native launcher for each platform, and in a protocol
- over a Unix domain [socket](sockets.md). The Scala application simply runs, with many
- invocations at once, each with its own faithful context. A daemon which serves each
- invocation against that invocation's own environment is
- [declarative context](../../philosophy/declarative-context.md) taken seriously: nothing about
- the caller is global.
+ Ici, ces détails sont traités dans un lanceur natif propre à chaque plateforme, et dans un
+ protocole sur un [socket](sockets.md) de domaine Unix. L'application Scala s'exécute
+ simplement, avec de nombreuses invocations à la fois, chacune avec son propre contexte
+ fidèle. Un démon qui sert chaque invocation dans l'environnement propre à cette invocation
+ est le [contexte déclaratif](../../philosophy/declarative-context.md) pris au sérieux : rien
+ de ce qui concerne l'appelant n'est global.

- The protocol is a [TEL](tel.md#bintel) schema, `ethereal-launcher`, whose text is
- `Launcher.schemaText`. Every connection which the launcher opens begins with one BinTEL
- document of that schema: an invocation with its arguments and environment, a signal, or a
- request for an exit status. The daemon answers in kind. Both sides check the schema's
- signature before reading a field, so a launcher and a daemon which were built against
- different contracts refuse each other, instead of misreading each other.
+ Le protocole est un schéma [TEL](tel.md#bintel), `ethereal-launcher`, dont le texte est
+ `Launcher.schemaText`. Chaque connexion qu'ouvre le lanceur commence par un document BinTEL
+ de ce schéma : une invocation avec ses arguments et son environnement, un signal, ou une
+ demande de statut de sortie. Le démon répond de même. Les deux côtés vérifient la signature
+ du schéma avant de lire un champ, si bien qu'un lanceur et un démon construits selon des
+ contrats différents se refusent mutuellement, au lieu de se lire de travers.

- Everything comes from the `soundness` package, beside the imports which a command-line
- application needs:
+ Tout provient du package `soundness`, à côté des imports dont a besoin une application en
+ ligne de commande :

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
import threads.virtualThreads
```

- The samples on this page define whole programs, so they are compiled but not run.
+ Les exemples de cette page définissent des programmes entiers ; ils sont donc compilés mais
+ pas exécutés.

- ### A daemon application
+ ### Une application démon

- `cli` is the daemonized counterpart of `application`. It has the same body, and the same
- structure for [completions](cli.md#tab-completions), and runs resident:
+ `cli` est l'équivalent en démon d'`application`. Il a le même corps, et la même structure
+ pour les [complétions](cli.md#tab-completions), et s'exécute en résident :

```scala
@main
def mytool(): Unit = cli:
  execute:
    Out.println(t"Hello world")
    Exit.Ok
```

- `cli` takes no arguments. The launcher forwards them, with the environment, working
- directory and standard streams of the shell which invoked it, and the body sees them through
- the same `arguments`, `Out` and `Exit` as an ordinary application.
+ `cli` ne prend aucun argument. Le lanceur les relaie, avec l'environnement, le répertoire de
+ travail et les flux standard du shell qui l'a invoqué, et le corps les voit à travers les
+ mêmes `arguments`, `Out` et `Exit` qu'une application ordinaire.

- The first run starts the daemon. Later runs connect to it, and return at the speed of a
- native tool. Tab completions gain the most: each request for completions is an invocation,
- and a resident process answers in milliseconds.
+ La première exécution démarre le démon. Les exécutions suivantes s'y connectent, et
+ répondent à la vitesse d'un outil natif. Ce sont les complétions par tabulation qui y gagnent
+ le plus : chaque demande de complétions est une invocation, et un processus résident répond
+ en quelques millisecondes.

- #### Output
+ #### Sortie

- Output must go through `Out` and `Err`. Scala's own `println` writes to the JVM's global
- `System.out`, which belongs to the *daemon* process, not to any client, so whatever it
- prints reaches no user, and is ordinarily lost. `Out` and `Err` resolve the
- [`Stdio`](streams.md#standard-streams) of the current invocation, which is the client's.
+ La sortie doit passer par `Out` et `Err`. Le `println` propre à Scala écrit dans le
+ `System.out` global de la JVM, qui appartient au processus *démon*, et non à un client, si
+ bien que tout ce qu'il affiche n'atteint aucun utilisateur, et est d'ordinaire perdu. `Out`
+ et `Err` résolvent le [`Stdio`](streams.md#standard-streams) de l'invocation courante, qui
+ est celui du client.

- When the reader of that stream goes away, as in `mytool | head -1` once `head` has exited,
- the launcher says so. The invocation's next write to the stream raises an `Egress.Error`,
- which ends the invocation with status 141, as `SIGPIPE` would end a process writing to a
- pipe.
+ Lorsque le lecteur de ce flux disparaît, comme dans `mytool | head -1` une fois que `head`
+ s'est terminé, le lanceur le signale. L'écriture suivante de l'invocation sur le flux lève
+ une `Egress.Error`, qui termine l'invocation avec le statut 141, comme `SIGPIPE` terminerait
+ un processus qui écrit dans un tube.

- Code which cannot be changed, such as a library which prints to `System.out`, is redirected
- with `Stdio.divert`. The diversion belongs to a *thread*, not to the JVM. It follows the
- thread on which it is made, the threads which are started under it, and every task which is
- spawned within it, so a daemon may divert around several clients at once, without their
- output crossing.
+ Le code qui ne peut pas être modifié, comme une bibliothèque qui affiche sur `System.out`,
+ est redirigé avec `Stdio.divert`. La redirection appartient à un *thread*, et non à la JVM.
+ Elle suit le thread sur lequel elle est faite, les threads démarrés sous elle, et chaque
+ tâche lancée en son sein, si bien qu'un démon peut rediriger autour de plusieurs clients à la
+ fois, sans que leurs sorties se croisent.

- Arguments and the values of environment variables arrive as text. A sequence of bytes in
- either which is not valid UTF-8 is delivered with U+FFFD in its place. That is the
- substitution which the JVM itself makes for its own arguments, so an application sees what it
- would have seen if it were run directly.
+ Les arguments et les valeurs des variables d'environnement arrivent sous forme de texte. Une
+ séquence d'octets de l'un ou l'autre qui n'est pas de l'UTF-8 valide est livrée avec U+FFFD
+ à sa place. C'est la substitution que fait la JVM elle-même pour ses propres arguments, si
+ bien qu'une application voit ce qu'elle aurait vu si elle avait été exécutée directement.

- ### What the invocation knows about its client
+ ### Ce que l'invocation sait de son client

- The daemon holds a socket, not the client's process, so everything which it knows about the
- invocation is what the launcher told it. The invocation's `resident`, a `Resident`, is its
- handle on the daemon, and is where that knowledge is read:
+ Le démon détient un socket, et non le processus du client, si bien que tout ce qu'il sait de
+ l'invocation est ce que le lanceur lui a dit. Le `resident` de l'invocation, un `Resident`,
+ est sa poignée sur le démon, et c'est là que ces informations se lisent :

```tabular
- column  Member
- column  Is
- row  `resident.cliInput`, `cliOutput`, `cliError`  whether each standard stream is a `Terminus.Terminal` or a `Terminus.Pipe`
- row  `resident.invokedAs`  the name the executable was invoked by: `argv[0]` as the caller supplied it
- row  `resident.umask`  the invocation's file-creation mask
- row  `resident.windowSize`  the client terminal's size, measured when the invocation began and on every resize
- row  `resident.buildId`  the running build's identifier
+ column  Membre
+ column  Est
+ row  `resident.cliInput`, `cliOutput`, `cliError`  si chaque flux standard est un `Terminus.Terminal` ou un `Terminus.Pipe`
+ row  `resident.invokedAs`  le nom sous lequel l'exécutable a été invoqué : `argv[0]` tel que l'appelant l'a fourni
+ row  `resident.umask`  le masque de création de fichiers de l'invocation
+ row  `resident.windowSize`  la taille du terminal du client, mesurée au début de l'invocation et à chaque redimensionnement
+ row  `resident.buildId`  l'identifiant du build en cours d'exécution
```

- One executable which is installed under several names, by symbolic links, can therefore
- dispatch on `invokedAs`, while every alias shares one warm daemon:
+ Un même exécutable installé sous plusieurs noms, par des liens symboliques, peut donc se
+ répartir selon `invokedAs`, tandis que tous les alias partagent un même démon chaud :

```scala
import dysasymptotics.linearSize

def multicall(): Unit = cli:
  execute:
    resident.invokedAs.let(_.cut(t"/").last) match
      case t"gunzip" => Out.println(t"decompressing")
      case _         => Out.println(t"compressing")
    Exit.Ok
```

- The [filesystem](filesystem.md#creation-masks) applies `umask` to whatever the invocation
- creates. The invocation's `Termcap` reads the same measurement as `windowSize`, so
- [tables](tables.md) and wrapped text fit the terminal as it is now.
+ Le [système de fichiers](filesystem.md#creation-masks) applique `umask` à tout ce que crée
+ l'invocation. Le `Termcap` de l'invocation lit la même mesure que `windowSize`, si bien que
+ les [tableaux](tables.md) et le texte coupé s'adaptent au terminal tel qu'il est maintenant.

- ### Signals and shutdown
+ ### Signaux et arrêt

- An invocation traps the signals which it cares about, and the response reaches the code of
- that invocation, not the shared process. A trap sees a `Signal`: the interrupt itself, and
- whatever the launcher attached to it. That is the terminal's new size, with `WINCH` and
- `CONT`. On Windows, it is the time which the system allows after an event of closing, logging
- off or shutting down, before it ends the client regardless:
+ Une invocation intercepte les signaux qui l'intéressent, et la réponse atteint le code de
+ cette invocation, et non le processus partagé. Une interception voit un `Signal` :
+ l'interruption elle-même, et ce que le lanceur y a joint. C'est la nouvelle taille du
+ terminal, avec `WINCH` et `CONT`. Sous Windows, c'est le temps que le système accorde après
+ un événement de fermeture, de déconnexion ou d'arrêt, avant de terminer le client quoi qu'il
+ arrive :

```scala
def longRunningWork(): Exit = Exit.Ok

def watch(): Unit = cli:
  execute:
    trap:
      case Signal(Interrupt.Winch, columns, rows, _) =>
        Out.println(t"now ${columns.or(0)}×${rows.or(0)}")
        SignalResponse.Accept

      case Signal(WindowsSignal.Close, _, _, deadline) =>
        SignalResponse.Accept

      case Signal(_: UnixSignal, _, _, _) =>
        SignalResponse.Accept

    longRunningWork()
```

- The daemon retires itself after six idle hours, or when its state files are removed, or on
- demand, through the built-in `'{admin}'` subcommand:
+ Le démon se retire de lui-même après six heures d'inactivité, ou lorsque ses fichiers d'état
+ sont supprimés, ou à la demande, via la sous-commande intégrée `'{admin}'` :

```tabular
- column  Subcommand
- column  Effect
- row  `pid`  reports the daemon's pid
- row  `kill`  ends the daemon at once
- row  `shutdown`  serves no further invocation, lets those in flight finish, then exits
+ column  Sous-commande
+ column  Effet
+ row  `pid`  indique le pid du démon
+ row  `kill`  termine immédiatement le démon
+ row  `shutdown`  ne sert plus aucune invocation, laisse terminer celles en cours, puis se termine
```

- A warm JVM which is held for a command that is rarely run can therefore be reclaimed,
- without finding its pid. An executable which has been rebuilt displaces its daemon in the
- same way as `shutdown`.
+ Une JVM chaude conservée pour une commande rarement exécutée peut donc être récupérée, sans
+ avoir à trouver son pid. Un exécutable qui a été reconstruit évince son démon de la même
+ manière que `shutdown`.

- ### Asking for a cooked terminal
+ ### Demander un terminal en mode canonique

- The launcher puts the terminal into raw mode before it connects, so that keypresses can be
- forwarded to an interactive [terminal session](terminal.md#keyboard-input). That is wrong
- for a command which just wants a line of input. Without the terminal driver's help there is
- no echo, and Backspace arrives as a literal byte inside the line. `cooked` asks the launcher
- for canonical mode for the duration of a block, and raw mode is restored afterwards:
+ Le lanceur met le terminal en mode brut avant de se connecter, afin que les frappes au
+ clavier puissent être relayées à une [session de terminal](terminal.md#keyboard-input)
+ interactive. C'est inadapté pour une commande qui veut simplement une ligne de saisie. Sans
+ l'aide du pilote de terminal, il n'y a pas d'écho, et Retour arrière arrive sous forme d'octet
+ littéral au sein de la ligne. `cooked` demande au lanceur le mode canonique pour la durée
+ d'un bloc, et le mode brut est rétabli ensuite :

```scala
def ask(): Unit = cli:
  execute:
    val name = resident.cooked:
      Out.println(t"Name?")
      java.io.BufferedReader(java.io.InputStreamReader(summon[Stdio].in)).readLine().nn.tt
    Out.println(t"Hello, $name")
    Exit.Ok
```

- Echo and line editing then come from the terminal driver itself. A launcher with no such
- channel, such as a pipe or an older launcher, lets the request expire harmlessly.
+ L'écho et l'édition de ligne proviennent alors du pilote de terminal lui-même. Un lanceur
+ dépourvu d'un tel canal, comme un tube ou un lanceur plus ancien, laisse la demande expirer
+ sans dommage.

- ### The message bus
+ ### Le bus de messages

- Concurrent invocations of one daemon share a typed *bus*. An invocation broadcasts a
- message, and others observe the stream. That is how "the running watch command notices that
- another invocation just changed the configuration" is expressed. The type of message is the
- type argument to `cli`, so every invocation of the daemon agrees on what can be sent:
+ Les invocations concurrentes d'un même démon partagent un *bus* typé. Une invocation diffuse
+ un message, et les autres observent le flux. C'est ainsi que s'exprime « la commande watch en
+ cours remarque qu'une autre invocation vient de modifier la configuration ». Le type de
+ message est l'argument de type de `cli`, si bien que toutes les invocations du démon
+ s'accordent sur ce qui peut être envoyé :

```scala
enum Notice:
  case ConfigChanged

def configure(): Unit = cli[Notice]:
  execute:
    resident.broadcast(Notice.ConfigChanged)
    resident.bus.each:
      case Notice.ConfigChanged => Out.println(t"another invocation changed the configuration")
    Exit.Ok
```

- ### Packaging
+ ### Empaquetage

- The distributable is assembled by the builder `xek`. It is published, with the launchers,
- from [propensive/xek](https://github.com/propensive/xek), and is installed with
- `curl -fsSL https://propensive.dev/xek | sh`. It joins the platform's native launcher, a
- small configuration record and the application's JAR into one executable file:
+ Le livrable est assemblé par le constructeur `xek`. Il est publié, avec les lanceurs, depuis
+ [propensive/xek](https://github.com/propensive/xek), et s'installe avec
+ `curl -fsSL https://propensive.dev/xek | sh`. Il réunit le lanceur natif de la plateforme,
+ un petit enregistrement de configuration et le JAR de l'application en un seul fichier
+ exécutable :

```sh
xek build mytool.jar
```

- That writes `mytool`, for the platform on which `xek` runs.
- `xek build -p linux-x64 -p windows-x64 mytool.jar` builds for other platforms.
- `xek build --polyglot mytool.jar` builds one file for all of them, which runs in `sh`, and
- in PowerShell and `cmd.exe` once it is renamed, and unpacks the right launcher where it
- runs. The flags which this page uses are:
+ Cela écrit `mytool`, pour la plateforme sur laquelle s'exécute `xek`.
+ `xek build -p linux-x64 -p windows-x64 mytool.jar` construit pour d'autres plateformes.
+ `xek build --polyglot mytool.jar` construit un seul fichier pour toutes, qui s'exécute dans
+ `sh`, et dans PowerShell et `cmd.exe` une fois renommé, et décompresse le bon lanceur là où
+ il s'exécute. Les options qu'utilise cette page sont :

```tabular
- column  Command
- column  Flag
- column  Effect
- row  `xek build`  `-p` *platform*  builds for that platform; repeatable
- row  `xek build`  `--polyglot`  builds one file for every platform
- row  `xek build`  `--build-id` *n*  bakes in the build identifier, which must increase from release to release
- row  `xek build`  `--public-key` *file*  bakes in the release key
- row  `xek build`  `--recovery-key` *file*  bakes in the recovery key
- row  `xek build`  `--app-id` *id*  bakes in the application identifier
- row  `xek keygen`  `--out` *prefix*  writes `<prefix>.seed` and `<prefix>.pub`
- row  `xek sign`  `--key` *seed*  signs with that key
- row  `xek sign`  `--in` *file*, `--out` *file*  names the binary to sign and the signed result
- row  `xek sign`  `--allow-downgrade`  permits the candidate to replace a newer build
- row  `xek sign`  `--foreign-key`  permits signing with a key the binary does not carry
+ column  Commande
+ column  Option
+ column  Effet
+ row  `xek build`  `-p` *plateforme*  construit pour cette plateforme ; répétable
+ row  `xek build`  `--polyglot`  construit un seul fichier pour toutes les plateformes
+ row  `xek build`  `--build-id` *n*  intègre l'identifiant de build, qui doit croître d'une version à l'autre
+ row  `xek build`  `--public-key` *fichier*  intègre la clé de publication
+ row  `xek build`  `--recovery-key` *fichier*  intègre la clé de récupération
+ row  `xek build`  `--app-id` *id*  intègre l'identifiant de l'application
+ row  `xek keygen`  `--out` *préfixe*  écrit `<prefix>.seed` et `<prefix>.pub`
+ row  `xek sign`  `--key` *graine*  signe avec cette clé
+ row  `xek sign`  `--in` *fichier*, `--out` *fichier*  nomme le binaire à signer et le résultat signé
+ row  `xek sign`  `--allow-downgrade`  permet au candidat de remplacer un build plus récent
+ row  `xek sign`  `--foreign-key`  permet de signer avec une clé que le binaire ne porte pas
```

- `xek --help` lists the rest.
+ `xek --help` liste les autres.

- The launcher finds or fetches a suitable JVM, and starts the daemon when none is running.
- Where a public key was built in, it accepts only signed binaries when the application
- [upgrades itself](https://en.wikipedia.org/wiki/Digital_signature) in place.
- [Packaging](packaging.md) covers the other forms of distributable.
+ Le lanceur trouve ou télécharge une JVM adéquate, et démarre le démon lorsqu'aucun n'est en
+ cours d'exécution. Lorsqu'une clé publique a été intégrée, il n'accepte que des binaires
+ signés lorsque l'application
+ [se met elle-même à niveau](https://en.wikipedia.org/wiki/Digital_signature) sur place.
+ [Empaquetage](packaging.md) couvre les autres formes de livrables.

- ### Signing a release
+ ### Signer une version

- An application can ship with a public key and an application identifier built in. The
- launcher uses them to verify any candidate upgrade, before swapping it into place.
- Verification happens in the launcher, before the JVM starts, so no Scala code in the running
- application sits on the boundary of trust.
+ Une application peut être livrée avec une clé publique et un identifiant d'application
+ intégrés. Le lanceur les utilise pour vérifier toute mise à niveau candidate, avant de la
+ mettre en place. La vérification a lieu dans le lanceur, avant le démarrage de la JVM, si
+ bien qu'aucun code Scala de l'application en cours d'exécution ne se trouve à la frontière de
+ confiance.

- `xek` also signs releases. Its signing subcommands need Java 24 or later. It generates a
- pair of keys:
+ `xek` signe aussi les versions. Ses sous-commandes de signature requièrent Java 24 ou
+ ultérieur. Il génère une paire de clés :

```sh
xek keygen --out release-keys/myapp
xek keygen --out release-keys/myapp-recovery
```

- Each writes a seed for a signing key, of 32 bytes, as `<prefix>.seed`, and an ML-DSA-44
- public key, of 1312 bytes, as `<prefix>.pub`. **The seeds belong offline**, and the recovery
- seed most of all. Anyone who holds the release seed can ship a binary which users' launchers
- will accept. The recovery key exists so that losing or leaking the release seed does not
- strand every installation. [Cryptography](cryptography.md#signing) describes ML-DSA
- signatures.
+ Chacune écrit une graine de clé de signature, de 32 octets, sous le nom `<prefix>.seed`, et
+ une clé publique ML-DSA-44, de 1312 octets, sous le nom `<prefix>.pub`. **Les graines doivent
+ rester hors ligne**, et la graine de récupération plus que toute autre. Quiconque détient la
+ graine de publication peut livrer un binaire que les lanceurs des utilisateurs accepteront.
+ La clé de récupération existe pour que la perte ou la fuite de la graine de publication ne
+ laisse pas en plan chaque installation. [Cryptographie](cryptography.md#signing) décrit les
+ signatures ML-DSA.

- Each release is then built and signed, in two steps. The build bakes in a build identifier,
- both public keys, and the application's identifier:
+ Chaque version est ensuite construite et signée, en deux étapes. Le build intègre un
+ identifiant de build, les deux clés publiques, et l'identifiant de l'application :

```sh
xek build --build-id 42 --public-key release-keys/myapp.pub \
  --recovery-key release-keys/myapp-recovery.pub --app-id example/myapp \
  dist/myapp.jar dist/myapp
```

- `--build-id` is a 64-bit number which must increase from release to release. The launcher
- rejects a candidate which is not newer than itself. A candidate which was built for another
- application identifier is rejected, even when it is signed with the same key. Omitting
- `--public-key` and `--app-id` produces a binary whose launcher rejects *every* upgrade,
- which is the right default for a local build, where the upgrade path is never exercised.
+ `--build-id` est un nombre de 64 bits qui doit croître d'une version à l'autre. Le lanceur
+ rejette un candidat qui n'est pas plus récent que lui-même. Un candidat construit pour un
+ autre identifiant d'application est rejeté, même s'il est signé avec la même clé. Omettre
+ `--public-key` et `--app-id` produit un binaire dont le lanceur rejette *toute* mise à
+ niveau, ce qui est le bon comportement par défaut pour un build local, où le chemin de mise à
+ niveau n'est jamais emprunté.

- Signing then produces the file to distribute:
+ La signature produit ensuite le fichier à distribuer :

```sh
xek sign --key release-keys/myapp.seed --in dist/myapp --out dist/myapp.signed
```

- That output is at once a valid executable and a valid candidate for upgrade.
+ Cette sortie est à la fois un exécutable valide et un candidat valide à la mise à niveau.

- #### Staging an upgrade
+ #### Préparer une mise à niveau

- The application stages a candidate by passing `Upgrade.stage` any source of bytes: a file,
- an [HTTP response](http-client.md), a [stream](streams.md). It writes the candidate as
- `<name>/.pending`, in the application's data directory, and returns. The launcher verifies
- it at the start of the next invocation, swaps it into place, and starts a new daemon in
- place of the old one, so nothing is interrupted:
+ L'application prépare un candidat en passant à `Upgrade.stage` n'importe quelle source
+ d'octets : un fichier, une [réponse HTTP](http-client.md), un [flux](streams.md). Elle écrit
+ le candidat sous le nom `<name>/.pending`, dans le répertoire de données de l'application, et
+ rend la main. Le lanceur le vérifie au début de l'invocation suivante, le met en place, et
+ démarre un nouveau démon à la place de l'ancien, si bien que rien n'est interrompu :

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import httpBackends.javaNetHttp
import strategies.throwUnsafely

def upgrade(): Text raises Upgrade.Error =
  if !Upgrade.enabled then t"this build cannot be upgraded"
  else if Upgrade.pending then t"the upgrade will be applied the next time the command is run"
  else
    Upgrade.stage(url"https://releases.example.com/myapp.signed".fetch())
    t"the upgrade will be applied the next time the command is run"
```

- `Upgrade.enabled` is false in a build with no key, or no application identifier, whose
- launcher would refuse any candidate. `resident.buildId` is the identifier of the running
- build, to compare with that of a release on offer.
+ `Upgrade.enabled` est faux dans un build sans clé, ou sans identifiant d'application, dont le
+ lanceur refuserait tout candidat. `resident.buildId` est l'identifiant du build en cours
+ d'exécution, à comparer avec celui d'une version proposée.

- The launcher records what it did with a candidate, and `Upgrade.outcome` reads the record
- back: the `result`, the build identifiers `candidate` and `previous`, and `when` it
- happened. The result is one of:
+ Le lanceur enregistre ce qu'il a fait d'un candidat, et `Upgrade.outcome` relit cet
+ enregistrement : le `result`, les identifiants de build `candidate` et `previous`, et le
+ moment `when` où cela s'est produit. Le résultat est l'un des suivants :

```tabular
- column  Result
- column  Meaning
- row  `Applied`  the upgrade was applied
- row  `Disabled`  the running executable does not accept upgrades
- row  `NoRecord`  the candidate is not an XEK executable
- row  `WrongApplication`  the candidate was built for another application
- row  `BadSignature`  the candidate's signature does not verify
- row  `NotNewer`  the candidate is not newer than the running executable
- row  `SwapFailed`  the candidate could not be swapped into place
+ column  Résultat
+ column  Signification
+ row  `Applied`  la mise à niveau a été appliquée
+ row  `Disabled`  l'exécutable en cours d'exécution n'accepte pas de mises à niveau
+ row  `NoRecord`  le candidat n'est pas un exécutable XEK
+ row  `WrongApplication`  le candidat a été construit pour une autre application
+ row  `BadSignature`  la signature du candidat ne se vérifie pas
+ row  `NotNewer`  le candidat n'est pas plus récent que l'exécutable en cours d'exécution
+ row  `SwapFailed`  le candidat n'a pas pu être mis en place
```

- `Upgrade.acknowledge()` clears the record, so that each outcome is reported once:
+ `Upgrade.acknowledge()` efface l'enregistrement, afin que chaque résultat ne soit signalé
+ qu'une fois :

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem

def report(): Optional[Message] =
  Upgrade.outcome.let: outcome =>
    Upgrade.acknowledge()
    m"${outcome.result} (build ${outcome.candidate.show})"
```

- #### What the signature covers
+ #### Ce que couvre la signature

- What the signature covers is chosen so that each part of it defeats a specific attack:
+ Ce que couvre la signature est choisi de sorte que chacune de ses parties déjoue une attaque
+ précise :

```tabular
- column  Signed part
- column  Prevents
- row  the launcher's own code  tampering with the launcher
- row  the bundled JAR  tampering with the application
- row  the build identifier  replaying an older, legitimately-signed release
- row  the application identifier  installing another application's release in its place
- row  the flag byte permitting a downgrade  turning it on after the fact
- row  the keys themselves  substituting a different release key into an otherwise-legitimate binary
+ column  Partie signée
+ column  Empêche
+ row  le code propre du lanceur  la falsification du lanceur
+ row  le JAR intégré  la falsification de l'application
+ row  l'identifiant de build  le rejeu d'une version plus ancienne, légitimement signée
+ row  l'identifiant de l'application  l'installation à sa place de la version d'une autre application
+ row  l'octet d'indicateur autorisant un retour en arrière  son activation après coup
+ row  les clés elles-mêmes  la substitution d'une autre clé de publication dans un binaire par ailleurs légitime
```

- A deliberate rollback, such as shipping 42 over a broken 43, is signed with
- `--allow-downgrade`, which sets that flag inside the signed payload.
+ Un retour en arrière délibéré, comme livrer la 42 par-dessus une 43 défectueuse, est signé
+ avec `--allow-downgrade`, qui active cet indicateur à l'intérieur de la charge utile signée.

- The keys which a candidate is checked against are always those of the *running* binary, so
- rotation of keys works as a chain. A release which is signed with the old key may carry a
- new key as its `--public-key`, after which releases are signed with the new one. `xek sign`
- asks for `--foreign-key` to sign with a key which the binary does not carry, since that is
- otherwise almost always a mistake. A release which is signed with the recovery key is
- accepted too, and may replace either key.
+ Les clés par rapport auxquelles un candidat est vérifié sont toujours celles du binaire *en
+ cours d'exécution*, si bien que la rotation des clés fonctionne comme une chaîne. Une version
+ signée avec l'ancienne clé peut porter une nouvelle clé comme `--public-key`, après quoi les
+ versions sont signées avec la nouvelle. `xek sign` demande `--foreign-key` pour signer avec
+ une clé que le binaire ne porte pas, puisque c'est sinon presque toujours une erreur. Une
+ version signée avec la clé de récupération est acceptée aussi, et peut remplacer l'une ou
+ l'autre clé.

- ### Related
+ ### Voir aussi

- - [Command-line applications](cli.md) describes the body which `cli` runs.
- - [Environment](environment.md#overriding-for-a-block) explains how each invocation sees its
-   own environment.
- - [Concurrency](concurrency.md) runs the invocations side by side.
- - [Packaging](packaging.md) builds other kinds of distributable.
- - [HTTP server](http-server.md), [LSP](lsp.md), [processes](processes.md) and [service discovery](service-discovery.md) describe what a daemon serves, launches and advertises.
+ - [Applications en ligne de commande](cli.md) décrit le corps qu'exécute `cli`.
+ - [Environnement](environment.md#overriding-for-a-block) explique comment chaque invocation
+   voit son propre environnement.
+ - [Concurrence](concurrency.md) exécute les invocations côte à côte.
+ - [Empaquetage](packaging.md) construit d'autres genres de livrables.
+ - [Serveur HTTP](http-server.md), [LSP](lsp.md), [processus](processes.md) et
+   [découverte de services](service-discovery.md) décrivent ce qu'un démon sert, lance et
+   annonce.
