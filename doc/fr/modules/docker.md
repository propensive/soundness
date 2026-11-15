- ## Containers
+ ## Conteneurs

- ### About
+ ### Présentation

- An [OCI](https://opencontainers.org/) container image is assembled in memory from filesystem
- layers and a configuration, and rendered as a standard OCI archive which any container runtime
- can load, with its content digests computed correctly. A separate client speaks to a running
- [containerd](https://containerd.io/) daemon over gRPC. It lists and creates containers, images
- and namespaces, and starts and stops the tasks which run inside containers.
+ Une image de conteneur [OCI](https://opencontainers.org/) est assemblée en mémoire à partir de
+ couches de système de fichiers et d'une configuration, puis rendue sous forme d'archive OCI
+ standard que n'importe quel runtime de conteneurs peut charger, avec ses condensés de contenu
+ correctement calculés. Un client distinct dialogue avec un démon
+ [containerd](https://containerd.io/) en cours d'exécution via gRPC. Il liste et crée des
+ conteneurs, des images et des espaces de noms, et démarre et arrête les tâches qui s'exécutent
+ dans les conteneurs.

- An image and everything in it, its layers, its manifest and its configuration, are [immutable]
- typed values. Each container, task and namespace on the daemon is a typed value too, and not a
- bag of JSON. The two halves are independent. An image can be built without a daemon, and a
- daemon can be managed without building an image.
+ Une image et tout ce qu'elle contient, ses couches, son manifeste et sa configuration, sont
+ des valeurs typées [immuables][immuable]. Chaque conteneur, tâche et espace de noms du démon
+ est aussi une valeur typée, et non un sac de JSON. Les deux moitiés sont indépendantes. On
+ peut construire une image sans démon, et administrer un démon sans construire d'image.

- ### On containers
+ ### À propos des conteneurs

- A container image is not a single file. It is a stack of filesystem
- [layers](https://en.wikipedia.org/wiki/OverlayFS), plus a configuration which says how to run
- them. Each piece is addressed by the
- [digest](https://en.wikipedia.org/wiki/Content-addressable_storage) of its bytes. The format is
- exact. A layer has one digest for its uncompressed tar and another for its compressed blob, and
- a manifest ties them together by those digests. Get a digest wrong, and the image is silently
- invalid.
+ Une image de conteneur n'est pas un fichier unique. C'est une pile de
+ [couches](https://en.wikipedia.org/wiki/OverlayFS) de système de fichiers, plus une
+ configuration qui indique comment les exécuter. Chaque élément est adressé par
+ le [condensé](https://en.wikipedia.org/wiki/Content-addressable_storage) de ses octets. Le
+ format est exact. Une couche a un condensé pour son tar non compressé et un autre pour son
+ blob compressé, et un manifeste les relie par ces condensés. Qu'un condensé soit faux,
+ et l'image est invalide sans que rien ne le signale.

- Running such an image is the job of a daemon. containerd, the runtime beneath Docker and
- Kubernetes, is driven over a gRPC protocol on a local socket.
+ Exécuter une telle image est le travail d'un démon. containerd, le runtime sous-jacent à
+ Docker et Kubernetes, se pilote via un protocole gRPC sur un socket local.

- Here the image format is a set of values whose digests are computed for you, and the protocol of
- the daemon is a set of methods which exchange typed values. Building an image as a value, and not
- by running a tool, is [direct style](../../philosophy/direct-style.md): the steps are ordinary code,
- checked as it compiles.
+ Ici, le format d'image est un ensemble de valeurs dont les condensés sont calculés pour
+ vous, et le protocole du démon est un ensemble de méthodes qui échangent des valeurs typées.
+ Construire une image comme une valeur, et non en exécutant un outil, relève du
+ [style direct](../../philosophy/direct-style.md) : les étapes sont du code ordinaire,
+ vérifié à la compilation.

- Building an image draws on [archives](archives.md) for its tar layers. Everything comes from the
- `soundness` package, and the last import lets the samples print their results:
+ La construction d'une image s'appuie sur les [archives](archives.md) pour ses couches tar.
+ Tout provient du package `soundness`, et le dernier import permet aux exemples d'afficher
+ leurs résultats :

```scala
import soundness.*

import alphabets.hexLowerCase
import codepages.utf8Codepage
import formatting.compactJsonFormatting
import providers.javaBaseProvider
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Building an image
+ ### Construire une image

- A layer is a tar archive of files. Given a `Tarfile`, `Oci.Layer` wraps it and computes the
- digests which the format requires:
+ Une couche est une archive tar de fichiers. À partir d'un `Tarfile`, `Oci.Layer` l'enveloppe
+ et calcule les condensés qu'exige le format :

```scala
def entry(name: Text, content: Text): Tar.Entry =
  Tar.Entry.File
   ( path  = name.as[Relative on Tar],
     mode  = UnixMode(),
     user  = UnixUser(0),
     group = UnixGroup(0),
     mtime = 0.bits.u32,
     data  = Archive.Body(content.in[Data]) )

val layer = Oci.Layer(Tarfile(List(entry(t"hello.txt", t"hello world\n"))))
```

- An `Oci.Image` assembles one or more layers with a configuration, which holds the command to
- run, the environment and the working directory:
+ Une `Oci.Image` assemble une ou plusieurs couches avec une configuration, qui contient la
+ commande à exécuter, l'environnement et le répertoire de travail :

```scala
val image = Oci.Image(List(layer), config = Oci.ContainerConfig(Cmd = List(t"/bin/sh")))
```

- ### What an image contains
+ ### Ce que contient une image

- The assembled image exposes each part which the format defines. A layer reports both of its
- digests. `diffId` is the digest of the uncompressed tar, and `digest` is the digest of the
- compressed blob:
+ L'image assemblée expose chaque partie que définit le format. Une couche indique ses deux
+ condensés. `diffId` est le condensé du tar non compressé, et `digest` celle du blob compressé :

```scala fuel
run
##
Out.println(layer.diffId.show)
Out.println(layer.digest.show)
```

- The image carries the manifest which references them, with the media type of each layer and of
- the configuration:
+ L'image porte le manifeste qui les référence, avec le type de média de chaque couche et de
+ la configuration :

```scala fuel
run
##
image.manifest.layers.each(descriptor => Out.println(descriptor.mediaType.show))
Out.println(image.manifest.config.mediaType.show)
```

- `image.archive` is the whole image as an OCI tar layout, which is the form a runtime imports.
- Read as bytes and written to a file, it produces an image which `docker load` or containerd
- will accept.
+ `image.archive` est l'image entière sous forme de disposition tar OCI, qui est la forme
+ qu'importe un runtime. Lue comme des octets et écrite dans un fichier, elle produit une
+ image que `docker load` ou containerd accepteront.

- ### WebAssembly workloads
+ ### Charges de travail WebAssembly

- A container image describes a filesystem, but `runc`, which unpacks and runs one, is not the
- only way to run a workload. A [Wasm](https://webassembly.org/) component needs no root
- filesystem at all. A runtime such as [wasmtime](https://wasmtime.dev/), reached through a
- containerd shim like `io.containerd.wasmtime.v1`, instantiates it directly. What changes is
- not the distribution format, but what the configuration says the artifact *is*.
+ Une image de conteneur décrit un système de fichiers, mais `runc`, qui en décompresse et en
+ exécute un, n'est pas le seul moyen d'exécuter une charge de travail. Un composant
+ [Wasm](https://webassembly.org/) n'a besoin d'aucun système de fichiers racine. Un runtime
+ comme [wasmtime](https://wasmtime.dev/), atteint au travers d'un shim containerd comme
+ `io.containerd.wasmtime.v1`, l'instancie directement. Ce qui change n'est pas le format de
+ distribution, mais ce que la configuration dit que l'artefact *est*.

- `Oci.Image.wasm` assembles that form, a
- [Wasm OCI Artifact](https://tag-runtime.cncf.io/wgs/wasm/deliverables/wasm-oci-artifact/):
+ `Oci.Image.wasm` assemble cette forme, un
+ [Wasm OCI Artifact](https://tag-runtime.cncf.io/wgs/wasm/deliverables/wasm-oci-artifact/) :

```scala
val component: Data = hex"0061736d01000000"

val artifact =
  Oci.Image.wasm
   ( component,
     exports = List(t"wasi:http/incoming-handler@0.2.0"),
     imports = List(t"wasi:io/streams@0.2.0"),
     target  = t"wasi:http/proxy@0.2.0" )
```

- The manifest, index and archive are the ordinary ones, so the same registries and tools carry
- it. Only two things differ:
+ Le manifeste, l'index et l'archive sont les mêmes que d'ordinaire ; les mêmes registres et
+ outils les transportent donc. Seules deux choses diffèrent :

```tabular
- column  Part
- column  Container image
- column  Wasm artifact
- row  config  a filesystem's run configuration  `application/vnd.wasm.config.v0+json`, describing a component
- row  layers  compressed tars, one or more  the component itself, `application/wasm`
- row  layer digests  `diffId` and `digest` differ  `diffId` and `digest` coincide, since it is stored uncompressed
+ column  Partie
+ column  Image de conteneur
+ column  Artefact Wasm
+ row  config  la configuration d'exécution d'un système de fichiers  `application/vnd.wasm.config.v0+json`, décrivant un composant
+ row  couches  des tars compressés, un ou plusieurs  le composant lui-même, `application/wasm`
+ row  condensés des couches  `diffId` et `digest` diffèrent  `diffId` et `digest` coïncident, puisqu'il est stocké non compressé
```

- The configuration describes the component, with the WASI generation as its `os`:
+ La configuration décrit le composant, avec la génération WASI comme `os` :

```scala fuel
run
##
Out.println(artifact.wasmConfig.let(_.os).inspect)
Out.println(artifact.wasmConfig.let(_.architecture).inspect)
```

- The `exports` and `imports` are the Component Model interfaces which the workload offers and
- needs. Recording them makes the image self-describing. A host can tell whether it can satisfy
- the workload's capabilities before fetching the component.
+ Les `exports` et `imports` sont les interfaces du Component Model que la charge de travail
+ offre et dont elle a besoin. Les enregistrer rend l'image auto-descriptive. Un hôte peut
+ savoir s'il peut satisfaire les capacités de la charge de travail avant de récupérer le
+ composant.

- They are usually not written by hand, but read from the WIT world which the component was linked
- against, which is the authoritative statement of the same contract:
+ On ne les écrit généralement pas à la main : on les lit dans le monde WIT contre lequel le
+ composant a été lié, qui est l'énoncé faisant autorité du même contrat :

```scala
val wit = t"""package example:service;

world http {
  import wasi:io/streams@0.2.0;
  export wasi:http/incoming-handler@0.2.0;
}"""

WitDialect.worlds(wit)(t"http").let: world =>
  Oci.Image.wasm(component, exports = world.exports, imports = world.imports)
```

- The [compiler](compiler.md#wasi-components) does exactly this on its edge which produces an OCI
- image. A compilation can go from source to a distributable artifact along one path, and nothing
- states the contract twice.
+ Le [compilateur](compiler.md#wasi-components) fait exactement cela sur sa branche qui
+ produit une image OCI. Une compilation peut aller du code source à un artefact distribuable
+ par un seul chemin, et rien n'énonce le contrat deux fois.

- ### Reading an image
+ ### Lire une image

- An existing OCI archive, which is a file or a block of bytes, is read by *opening* it. Its
- contents are available for the duration of a scope, and no longer:
+ Une archive OCI existante, qui est un fichier ou un bloc d'octets, se lit en l'*ouvrant*.
+ Son contenu est disponible pendant la durée d'une portée, et pas au-delà :

```scala
val archive: Data = image.archive.source[Data].memoize
```

```scala fuel
run
##
archive.open[Oci.Image](): handle ?=>
  handle.index.manifests.each(descriptor => Out.println(descriptor.mediaType.show))
  handle.manifest.layers.each(descriptor => Out.println(descriptor.digest.show))
  Out.println(handle.imageConfig.config.let(_.Cmd).inspect)
```

- `handle.verified(descriptor)` reads a layer and checks its digest. The three ways to reach a
- layer make the cost explicit:
+ `handle.verified(descriptor)` lit une couche et vérifie son condensé. Les trois manières
+ d'atteindre une couche rendent le coût explicite :

```tabular
- column  Method
- column  Yields
- column  Checks the digest
- row  `compressed`  the stored bytes, untouched, as a stream  no
- row  `layer`  the uncompressed tar, as a stream  no
- row  `verified`  the stored bytes, gathered eagerly  yes, and the size
+ column  Méthode
+ column  Produit
+ column  Vérifie le condensé
+ row  `compressed`  les octets stockés, intacts, sous forme de flux  non
+ row  `layer`  le tar non compressé, sous forme de flux  non
+ row  `verified`  les octets stockés, rassemblés immédiatement  oui, ainsi que la taille
```

- `verified` raises an `Oci.Error` if the bytes disagree with the digest or the size which the
- manifest declares. Checking a stream would mean draining it, which is why `verified` gathers
- the bytes first.
+ `verified` lève une `Oci.Error` si les octets ne concordent pas avec le condensé ou la
+ taille que déclare le manifeste. Vérifier un flux impliquerait de le vider, c'est pourquoi
+ `verified` rassemble d'abord les octets.

- A reader which does not already know which kind of artifact it has opened asks for `config`. It
- dispatches on the media type of the config descriptor. Telling a component from a filesystem is
- a question which the archive answers, and not one the caller has to have known:
+ Un lecteur qui ne sait pas déjà quel type d'artefact il a ouvert demande `config`. Celle-ci
+ répartit selon le type de média du descripteur de configuration. Distinguer un composant
+ d'un système de fichiers est une question à laquelle l'archive répond, et non une chose que
+ l'appelant aurait dû savoir :

```scala
archive.open[Oci.Image](): handle ?=>
  handle.config match
    case config: Oci.WasmConfig   => config.component
    case config: Oci.Image.Config => config.rootfs
```

- ### Connecting to a daemon
+ ### Se connecter à un démon

- The rest of this page needs a running daemon, so its samples are compiled but not run.
+ La suite de cette page nécessite un démon en cours d'exécution ; ses exemples sont donc
+ compilés mais pas exécutés.

- A `Containerd` client connects to a daemon over an HTTP/2 endpoint on its socket, bound to a
- namespace. The connection holds background work, so it lives inside a supervised scope:
+ Un client `Containerd` se connecte à un démon via un endpoint HTTP/2 sur son socket, lié à
+ un espace de noms. La connexion maintient du travail en arrière-plan ; elle vit donc dans
+ une portée supervisée :

```scala
import probates.awaitProbate
import threads.virtualThreads

def report[endpoint](endpoint: Http2.Endpoint[endpoint]): Unit = supervise:
  val client = Containerd(endpoint, t"default")
  client.version()
```

- Here `endpoint` is an HTTP/2 endpoint over containerd's Unix socket, established through the
- HTTP/2 and [socket](sockets.md) support, which the client speaks [gRPC](rpc.md) over. Every
- call to the daemon may fail, in the socket, the protocol or the request itself, so the calls
- draw on the error [strategy] in scope.
+ Ici, `endpoint` est un endpoint HTTP/2 sur le socket Unix de containerd, établi grâce à la
+ prise en charge de HTTP/2 et des [sockets](sockets.md), et par-dessus lequel le client parle
+ [gRPC](rpc.md). Chaque appel au démon peut échouer, dans le socket, le protocole ou la
+ requête elle-même ; les appels s'appuient donc sur la [stratégie] d'erreur dans la portée.

- ### Containers, images and namespaces
+ ### Conteneurs, images et espaces de noms

- The client lists and inspects what the daemon holds, and creates and deletes it. Each result is
- a typed record:
+ Le client liste et inspecte ce que contient le démon, et le crée et le supprime. Chaque
+ résultat est un enregistrement typé :

```scala
def inspect(client: Containerd)(using Monitor): Unit =
  val containers = client.containers()
  val one = client.container(t"web")
  (one.id, one.image, one.labels)

  client.images()
  client.namespaces()
```

- These are the methods for each kind of resource:
+ Voici les méthodes pour chaque type de ressource :

```tabular
- column  Resource
- column  List
- column  Inspect
- column  Create
- column  Delete
- row  container  `containers()`  `container(id)`  `createContainer(container)`  `deleteContainer(id)`
- row
-   cell  namespace
-   cell  `namespaces()`
-   cell
-   cell  `createNamespace(namespace)`
-   cell  `deleteNamespace(name)`
- row
-   cell  image
-   cell  `images()`
-   cell  `image(name)`
-   cell
-   cell  `deleteImage(name)`
- row  task  `tasks()`  `task(id)`  `createTask(id, rootfs)`  `deleteTask(id)`
+ column  Ressource
+ column  Lister
+ column  Inspecter
+ column  Créer
+ column  Supprimer
+ row  conteneur  `containers()`  `container(id)`  `createContainer(container)`  `deleteContainer(id)`
+ row
+   cell  espace de noms
+   cell  `namespaces()`
+   cell
+   cell  `createNamespace(namespace)`
+   cell  `deleteNamespace(name)`
+ row
+   cell  image
+   cell  `images()`
+   cell  `image(name)`
+   cell
+   cell  `deleteImage(name)`
+ row  tâche  `tasks()`  `task(id)`  `createTask(id, rootfs)`  `deleteTask(id)`
```

- ### Tasks
+ ### Tâches

- A container is a definition. A *task* is a running instance of one. A task is created from a
- container with the root filesystem to mount, then started, waited on and killed:
+ Un conteneur est une définition. Une *tâche* en est une instance en cours d'exécution. Une
+ tâche est créée à partir d'un conteneur avec le système de fichiers racine à monter, puis
+ démarrée, attendue et tuée :

```scala
def run(client: Containerd)(using Monitor): Unit =
  val rootfs = List(Containerd.Mount(t"overlay", t"overlay", t"/", List(t"lowerdir=/a")))

  client.createTask(t"web", rootfs)
  val pid = client.startTask(t"web")
  client.waitTask(t"web")
  client.killTask(t"web", signal = 15)
```

- `client.task(t"web")` returns the current `Workload` of the task, whose `state` reports whether
- it is created, running, stopped or paused.
+ `client.task(t"web")` renvoie le `Workload` courant de la tâche, dont le `state` indique si
+ elle est créée, en cours d'exécution, arrêtée ou en pause.

- ### Related
+ ### Voir aussi

- - [Archives](archives.md) describes the tar archives which layers are made of.
- - [Compiler](compiler.md#wasi-components) produces the WASI components and OCI images which a
-   toolchain links.
- - [Hashing](hashing.md) describes the digests which address every part of an image.
- - [RPC](rpc.md) describes the gRPC which the client speaks to containerd.
- - [Sockets](sockets.md) describes the Unix socket which the daemon listens on.
- - [Concurrency](concurrency.md) describes `supervise`, which the client lives inside.
- - [Compression](compression.md) describes the gzip which layers are compressed with, and [processes](processes.md) the commands a task runs.
+ - [Archives](archives.md) décrit les archives tar dont sont faites les couches.
+ - [Compilateur](compiler.md#wasi-components) produit les composants WASI et les images OCI
+   que lie une toolchain.
+ - [Hachage](hashing.md) décrit les condensés qui adressent chaque partie d'une image.
+ - [RPC](rpc.md) décrit le gRPC que le client parle à containerd.
+ - [Sockets](sockets.md) décrit le socket Unix sur lequel écoute le démon.
+ - [Concurrence](concurrency.md) décrit `supervise`, à l'intérieur duquel vit le client.
+ - [Compression](compression.md) décrit le gzip avec lequel les couches sont compressées, et
+   les [processus](processes.md) les commandes qu'exécute une tâche.
