- ## Packaging
+ ## Empaquetage

- ### About
+ ### Présentation

- An application built with these libraries is a JAR, and a JAR which carries every dependency it
- needs is large. A command-line tool of a few hundred lines ships tens of megabytes of library
- code, most of it already published somewhere public. Burdock makes that JAR *slim*, without
- making it fragile, in three steps:
+ Une application construite avec ces bibliothèques est un JAR, et un JAR qui transporte
+ toutes les dépendances dont il a besoin est volumineux. Un outil en ligne de commande de
+ quelques centaines de lignes embarque des dizaines de mégaoctets de code de bibliothèques,
+ dont l'essentiel est déjà publié quelque part publiquement. Burdock rend ce JAR *allégé*,
+ sans le rendre fragile, en trois étapes :

```tabular
- column  When
- column  Step
- column  Does
- row  at compiletime  `externalize`  records which dependencies the application was built against, as content hashes rather than coordinates
- row  after the build  `repackage`  rewrites the JAR, replacing every dependency with a public home by a reference to it, and keeping anything without one
- row  at runtime  the bootstrap loader  fetches the referenced dependencies, verifies each against its hash, caches it, and starts the application
+ column  Quand
+ column  Étape
+ column  Rôle
+ row  à la compilation  `externalize`  enregistre les dépendances avec lesquelles l'application a été construite, sous forme d'empreintes de contenu plutôt que de coordonnées
+ row  après le build  `repackage`  réécrit le JAR, en remplaçant chaque dépendance qui a un emplacement public par une référence à celui-ci, et en conservant tout ce qui n'en a pas
+ row  à l'exécution  le chargeur d'amorçage  récupère les dépendances référencées, vérifie chacune par rapport à son empreinte, la met en cache, et démarre l'application
```

- So the first launch downloads, every later launch is instant, and a dependency which has changed
- since the build cannot be loaded.
+ Ainsi, le premier lancement télécharge, chaque lancement ultérieur est instantané, et une
+ dépendance qui a changé depuis le build ne peut pas être chargée.

- ### On content hashes
+ ### À propos des empreintes de contenu

- Build tools identify a dependency by coordinates, which are a group, a name and a version, and
- trust that the same coordinates resolve to the same bytes everywhere. Burdock identifies it by
- the SHA-256 of the JAR which the compiler actually read. That is the stronger claim. A reference
- in a repackaged JAR names the exact bytes, and the bootstrap refuses anything else, whether a
- registry served a different build under the same version, or a download was tampered with on
- the way.
+ Les outils de build identifient une dépendance par des coordonnées, qui sont un groupe, un
+ nom et une version, et font confiance au fait que les mêmes coordonnées se résolvent partout
+ en les mêmes octets. Burdock l'identifie par le SHA-256 du JAR que le compilateur a
+ réellement lu. C'est l'affirmation la plus forte. Une référence dans un JAR réempaqueté
+ nomme les octets exacts, et l'amorçage refuse tout le reste, qu'un registre ait servi un
+ build différent sous la même version, ou qu'un téléchargement ait été altéré en chemin.

- The hash also separates *where* a dependency lives from *what* it is. The repackager asks
- [deps.dev](https://deps.dev/) which Maven artifact has that hash, and consults the release
- assets of any GitHub repositories named as hints. A dependency found in neither is inlined from
- the local cache. So the completeness invariant holds: every dependency is either referenced or
- present. A hash which is neither stops the repackage with an error, and does not produce a JAR
- which will fail later.
+ L'empreinte sépare aussi l'endroit *où* se trouve une dépendance de ce *qu'elle est*. Le
+ réempaqueteur demande à [deps.dev](https://deps.dev/) quel artefact Maven a cette empreinte,
+ et consulte les fichiers de version de tous les dépôts GitHub indiqués comme pistes. Une
+ dépendance trouvée dans aucun des deux est intégrée à partir du cache local. L'invariant de
+ complétude est donc respecté : chaque dépendance est soit référencée, soit présente. Une
+ empreinte qui n'est ni l'une ni l'autre arrête le réempaquetage avec une erreur, et ne
+ produit pas un JAR qui échouera plus tard.

- ### Externalizing an application
+ ### Externaliser une application

- The application wraps its entry point in `externalize`. At [runtime] the macro leaves the body
- alone. At [compiletime] it captures the classpath of the build, hashes each dependency JAR,
- hard-links it into the Burdock cache at `~/.cache/soundness/jars/<sha256>.jar`, so the bytes
- stay retrievable by hash on the build machine, and embeds the list of hashes as the resource
- `META-INF/burdock.deps` in the compiled output:
+ L'application enveloppe son point d'entrée dans `externalize`. [À l'exécution][exécution],
+ la macro laisse le corps intact. [À la compilation][compilation], elle capture le classpath
+ du build, calcule l'empreinte de chaque JAR de dépendance, crée un lien physique vers lui dans
+ le cache de Burdock à `~/.cache/soundness/jars/<sha256>.jar`, afin que les octets restent
+ récupérables par empreinte sur la machine de build, et intègre la liste des empreintes comme
+ ressource `META-INF/burdock.deps` dans la sortie compilée :

- <!-- doccheck: skip -->
+ <!-- doccheck: skip -->
```scala
import soundness.*
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap

@main
def myApp(): Unit = externalize:
  Out.println(t"hello, world")
```

- Nothing else about the application changes. It is still built, tested and run as an ordinary
- JAR, and the embedded resource is inert until the repackager reads it.
+ Rien d'autre ne change dans l'application. Elle est toujours construite, testée et exécutée
+ comme un JAR ordinaire, et la ressource intégrée reste inerte jusqu'à ce que le
+ réempaqueteur la lise.

- ### Repackaging
+ ### Réempaquetage

- The `soundness.repackage` entry point is launched against the application JAR. It locates the
- JAR by finding the `META-INF/burdock.deps` resource on its own classpath, and rewrites it in
- place:
+ Le point d'entrée `soundness.repackage` est lancé sur le JAR de l'application. Il localise le
+ JAR en trouvant la ressource `META-INF/burdock.deps` dans son propre classpath, et le réécrit
+ sur place :

```sh
java -cp app.jar:burdock.jar soundness.repackage --github propensive/soundness
```

- Each `--github owner/repo` hint names a repository whose release assets are a source of
- downloads. The option may be repeated, or its values separated by commas. GitHub keeps no
- global index by hash, so the hints are indexed up front, with a few requests for each
- repository, honoring a `GITHUB_TOKEN` environment variable if one is set. They are consulted
- before deps.dev, since a repository which the user named takes precedence over the global
- index. In the rewritten JAR, each class meets one of three fates:
+ Chaque piste `--github owner/repo` nomme un dépôt dont les fichiers de version sont une
+ source de téléchargements. L'option peut être répétée, ou ses valeurs séparées par des
+ virgules. GitHub ne tient aucun index global par empreinte ; les pistes sont donc indexées
+ au préalable, avec quelques requêtes par dépôt, en tenant compte d'une variable
+ d'environnement `GITHUB_TOKEN` si elle est définie. Elles sont consultées avant deps.dev,
+ puisqu'un dépôt nommé par l'utilisateur a priorité sur l'index global. Dans le JAR réécrit,
+ chaque classe connaît l'un de trois sorts :

```tabular
- column  Classes of
- column  Are
- row  the application  kept
- row  a dependency it could resolve  dropped, and referenced instead
- row  a dependency it could not resolve  inlined
+ column  Classes de
+ column  Sont
+ row  l'application  conservées
+ row  une dépendance qu'il a pu résoudre  retirées, et référencées à la place
+ row  une dépendance qu'il n'a pas pu résoudre  intégrées
```

- It carries exactly one extra class, `burdock.Bootstrap`, which is the runtime loader. The
- manifest records the arrangement:
+ Il porte exactement une classe supplémentaire, `burdock.Bootstrap`, qui est le chargeur
+ d'exécution. Le manifeste enregistre l'arrangement :

```tabular
- column  Attribute
- column  Holds
- row  `Main-Class`  `burdock.Bootstrap`
- row  `Burdock-Main`  the application's own entry point, which the bootstrap starts once the classpath is assembled
- row  `Burdock-Require`  each externalized dependency as `<sha256>:<url>`, in classpath order
- row  `Burdock-Verbosity`  what the bootstrap prints while it fetches: `silent`, as the repackager writes it, or `error`, `info` or `debug`
+ column  Attribut
+ column  Contient
+ row  `Main-Class`  `burdock.Bootstrap`
+ row  `Burdock-Main`  le point d'entrée propre à l'application, que l'amorçage démarre une fois le classpath assemblé
+ row  `Burdock-Require`  chaque dépendance externalisée sous la forme `<sha256>:<url>`, dans l'ordre du classpath
+ row  `Burdock-Verbosity`  ce qu'affiche l'amorçage pendant les récupérations : `silent`, tel que l'écrit le réempaqueteur, ou `error`, `info` ou `debug`
```

- The command prints a summary, with the entries read and written, the classes kept, inlined and
- stripped, and each externalized requirement. It draws a progress bar while it resolves, only on
- a real terminal, so redirected output stays clean.
+ La commande affiche un résumé, avec les entrées lues et écrites, les classes conservées,
+ intégrées et retirées, et chaque exigence externalisée. Elle dessine une barre de progression
+ pendant la résolution, uniquement sur un vrai terminal, afin qu'une sortie redirigée reste
+ propre.

- The same work is available as a library call, `Repackager.repackage`. It takes the input and
- output paths, a `Repackager.Resolver` from hash to URL, a `Repackager.CacheReader` for the local
- cache, the bytes of the bootstrap class and a progress callback, and returns a
- `Repackager.Summary` rather than printing anything. The command-line entry point is a thin
- rendering of that call, and `DepsDev.mavenUrl` and `GitHub.index` are the two resolvers it
- composes.
+ Le même travail est disponible sous forme d'appel de bibliothèque, `Repackager.repackage`. Il
+ prend les chemins d'entrée et de sortie, un `Repackager.Resolver` de l'empreinte vers l'URL,
+ un `Repackager.CacheReader` pour le cache local, les octets de la classe d'amorçage et un
+ callback de progression, et renvoie un `Repackager.Summary` plutôt que d'afficher quoi que ce
+ soit. Le point d'entrée en ligne de commande est une mince mise en forme de cet appel, et
+ `DepsDev.mavenUrl` et `GitHub.index` sont les deux résolveurs qu'il compose.

- ### Running a repackaged JAR
+ ### Exécuter un JAR réempaqueté

- A repackaged JAR runs with `java -jar app.jar`, exactly as before. The bootstrap reads
- `Burdock-Require`, and downloads every requirement which is not already in
- `~/.cache/soundness/jars/`, in parallel, bounded to a handful of connections for each host. It
- verifies the SHA-256 of each download against the hash in the manifest, builds the classpath in
- the order of the manifest, and invokes `Burdock-Main`. A checksum mismatch stops the launch. The
- application never starts against bytes other than the ones it was built with.
+ Un JAR réempaqueté s'exécute avec `java -jar app.jar`, exactement comme avant. L'amorçage lit
+ `Burdock-Require`, et télécharge chaque exigence qui n'est pas déjà dans
+ `~/.cache/soundness/jars/`, en parallèle, avec au plus quelques connexions par hôte. Il
+ vérifie le SHA-256 de chaque téléchargement par rapport à l'empreinte du manifeste, construit
+ le classpath dans l'ordre du manifeste, et invoque `Burdock-Main`. Une somme de contrôle
+ discordante arrête le lancement. L'application ne démarre jamais sur d'autres octets que
+ ceux avec lesquels elle a été construite.

- A launcher which supervises startup, as the [daemon](daemons.md) launcher does, can watch the
- progress of the download by setting the `burdock.progress` system property to a file path. The
- bootstrap writes `<completed> <total> <bytes>` to it atomically as it goes. A launcher which
- cannot read the file falls back to its fixed deadline.
+ Un lanceur qui supervise le démarrage, comme le fait le lanceur des [démons](daemons.md),
+ peut suivre la progression du téléchargement en définissant la propriété système
+ `burdock.progress` sur un chemin de fichier. L'amorçage y écrit atomiquement
+ `<completed> <total> <bytes>` au fur et à mesure. Un lanceur qui ne peut pas lire le fichier
+ se replie sur son délai fixe.

- ### Errors
+ ### Erreurs

- Repackaging reports its failures with two errors, and a lookup which finds nothing with a third:
+ Le réempaquetage signale ses échecs par deux erreurs, et une recherche qui ne trouve rien par
+ une troisième :

```tabular
- column  Error
- column  Raised when
- row  `Repackager.UserError`  the JAR was not built with Burdock, is a directory rather than a file, an argument is malformed, or a GitHub repository's releases cannot be listed
- row  `Repackager.RepackageError`  the JAR cannot be rewritten: it has no manifest or no `Main-Class`, or a dependency is neither published nor cached
- row  `DepsDev.Unresolved`  a deps.dev lookup finds nothing, which the repackager treats as "not published" rather than as a failure
+ column  Erreur
+ column  Levée lorsque
+ row  `Repackager.UserError`  le JAR n'a pas été construit avec Burdock, est un répertoire plutôt qu'un fichier, un argument est mal formé, ou les versions d'un dépôt GitHub ne peuvent pas être listées
+ row  `Repackager.RepackageError`  le JAR ne peut pas être réécrit : il n'a pas de manifeste ou pas de `Main-Class`, ou une dépendance n'est ni publiée ni en cache
+ row  `DepsDev.Unresolved`  une recherche sur deps.dev ne trouve rien, ce que le réempaqueteur traite comme « non publiée » plutôt que comme un échec
```

- Resolution is logged through `DepsEvent`, with one event for each hash resolved, so a run can be
- audited afterwards.
+ La résolution est journalisée au moyen de `DepsEvent`, avec un événement pour chaque
+ empreinte résolue ; une exécution peut donc être auditée après coup.

- ### Related
+ ### Voir aussi

- - [Archives](archives.md) describes the ZIP format which a JAR is, and the prefix which makes
-   it executable.
- - [Compiler](compiler.md#producing-artifacts) builds the JAR which is then repackaged.
- - [Daemons](daemons.md) and [LSP](lsp.md#a-thin-launcher) describe the launchers which ship
-   as thin JARs.
- - [Hashing](hashing.md) describes the SHA-256 digests which identify each dependency.
- - [Classpath](classpath.md) describes the classpath which the bootstrap assembles.
- - [CLI](cli.md) and [versioning](versioning.md) describe the applications and manifests which are packaged.
+ - [Archives](archives.md) décrit le format ZIP qu'est un JAR, et le préfixe qui le rend
+   exécutable.
+ - [Compilateur](compiler.md#producing-artifacts) construit le JAR qui est ensuite
+   réempaqueté.
+ - [Démons](daemons.md) et [LSP](lsp.md#a-thin-launcher) décrivent les lanceurs distribués
+   sous forme de JAR allégés.
+ - [Hachage](hashing.md) décrit les empreintes SHA-256 qui identifient chaque dépendance.
+ - [Classpath](classpath.md) décrit le classpath qu'assemble l'amorçage.
+ - [CLI](cli.md) et [versions](versioning.md) décrivent les applications et les manifestes
+   qui sont empaquetés.
