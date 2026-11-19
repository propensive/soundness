- ## Filesystem
+ ## Système de fichiers

- ### About
+ ### Présentation

- Files on a disk are read and written through the typed [paths](paths.md) which already
- describe them. A `Path on Linux` has the operations which touch the disk: opening it to read
- or write, creating it as a file or a directory, listing its children, copying, moving and
- deleting. Each operation declares the errors it may raise.
+ Les fichiers sur un disque se lisent et s'écrivent à travers les [chemins](paths.md) typés
+ qui les décrivent déjà. Un `Path on Linux` dispose des opérations qui touchent le disque :
+ l'ouvrir pour lire ou écrire, le créer comme fichier ou comme répertoire, lister ses
+ enfants, copier, déplacer et supprimer. Chaque opération déclare les erreurs qu'elle peut
+ lever.

- How an operation behaves in the awkward cases is decided by policies, chosen in scope. They
- say, for example, whether a copy overwrites an existing file, and whether a delete descends
- into a directory.
+ Le comportement d'une opération dans les cas délicats est décidé par des politiques,
+ choisies dans la portée. Elles disent, par exemple, si une copie écrase un fichier
+ existant, et si une suppression descend dans un répertoire.

- Beyond individual files, the standard directories of a system, such as the home directory,
- the cache and `/usr/share`, are named values. A directory can be watched for changes. A file
- can be locked, mapped into memory, tagged with extended attributes, or searched for along a
- list of directories.
+ Au-delà des fichiers individuels, les répertoires standard d'un système, comme le
+ répertoire personnel, le cache et `/usr/share`, sont des valeurs nommées. Un répertoire
+ peut être surveillé pour détecter les changements. Un fichier peut être verrouillé,
+ projeté en mémoire, tagué avec des attributs étendus, ou recherché le long d'une liste
+ de répertoires.

- ### On the filesystem
+ ### À propos du système de fichiers

- The JVM's filesystem API is full of untyped edges. A path is a string, or an untyped object.
- A missing file is a `null` here and an exception there. The crucial questions of policy are
- buried in flag arguments, or fixed by the method which is chosen, and are invisible in the
- type: does this copy replace what is already there, and does this delete empty a whole tree?
- A program which gets one wrong destroys data.
+ L'API de système de fichiers de la JVM regorge d'aspérités non typées. Un chemin est une
+ chaîne, ou un objet non typé. Un fichier absent est un `null` ici et une exception là. Les
+ questions de politique cruciales sont enfouies dans des arguments de type indicateur, ou
+ fixées par le choix de la méthode, et sont invisibles dans le type : cette copie
+ remplace-t-elle ce qui s'y trouve déjà, et cette suppression vide-t-elle toute une
+ arborescence ? Un programme qui se trompe sur l'une d'elles détruit des données.

- Here these are lifted into the types:
+ Ici, elles sont relevées au niveau des types :

- - A path carries its platform, so its rules are known.
- - An operation declares `raises Io.Error`, and the error has a specific reason: a missing
-   file, a permission denied, a directory which is not empty.
- - Each policy is an explicit [contextual value], so overwriting, or deleting recursively, is a
-   decision which the code states, not a default it inherits.
+ - Un chemin porte sa plateforme, si bien que ses règles sont connues.
+ - Une opération déclare `raises Io.Error`, et l'erreur a une raison précise : un fichier
+   absent, une permission refusée, un répertoire qui n'est pas vide.
+ - Chaque politique est une [valeur contextuelle] explicite, de sorte qu'écraser, ou
+   supprimer récursivement, est une décision que le code énonce, et non un comportement par
+   défaut dont il hérite.

- Everything comes from the `soundness` package. The capabilities of the system, and the
- policies which the operations need, are brought into scope:
+ Tout provient du package `soundness`. Les capacités du système, et les politiques dont ont
+ besoin les opérations, sont amenées dans la portée :

```scala
import soundness.*
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.moveAtomically
import environments.javaBaseEnvironment
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap

import charsets.utf8Charset
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import filesystemOptions.overwritePreexisting
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.javaBaseTemporaryDirectory
import textSanitizers.strictSanitizer
```

```tabular
- column  Imports
- column  Decide
- row  `filesystemBackends.javaBaseFilesystem`  which implementation touches the disk: here, `java.nio`
- row  `pathInterfaces.pathOnLinux`  the type of path which the standard directories are given as
- row  the five from `filesystemOptions`  the answers to the questions of policy, described under [Copying, moving and deleting](#copying-moving-and-deleting)
- row  `charsets`, `codepages`, `textSanitizers`  how bytes become text, and text becomes bytes; [Characters](characters.md) describes them
- row  `environments`, `systems`, `temporaryDirectories`  where the [environment](environment.md), system properties and temporary directory come from
- row  `strategies.throwUnsafely`  that an error is thrown as an exception; [Errors](errors.md#choosing-a-strategy) has the other strategies
- row  `stdios`, `termcapDefinitions`  where the samples print their results
+ column  Imports
+ column  Décident
+ row  `filesystemBackends.javaBaseFilesystem`  quelle implémentation touche le disque : ici, `java.nio`
+ row  `pathInterfaces.pathOnLinux`  le type de chemin sous lequel sont donnés les répertoires standard
+ row  les cinq de `filesystemOptions`  les réponses aux questions de politique, décrites sous [Copier, déplacer et supprimer](#copying-moving-and-deleting)
+ row  `charsets`, `codepages`, `textSanitizers`  comment les octets deviennent du texte, et le texte des octets ; [Caractères](characters.md) les décrit
+ row  `environments`, `systems`, `temporaryDirectories`  d'où viennent l'[environnement](environment.md), les propriétés système et le répertoire temporaire
+ row  `strategies.throwUnsafely`  qu'une erreur est levée comme une exception ; [Erreurs](errors.md#choosing-a-strategy) présente les autres stratégies
+ row  `stdios`, `termcapDefinitions`  où les exemples affichent leurs résultats
```

- The samples work in a directory of their own, beneath the system's temporary directory:
+ Les exemples travaillent dans un répertoire qui leur est propre, sous le répertoire
+ temporaire du système :

```scala
val directory = temporaryDirectory[Path on Linux] / "soundness-filesystem-tutorial"
val file = directory / "notes.txt"
```

- ### Files and directories
+ ### Fichiers et répertoires

- A path names a location. Whether it is a file or a directory is a matter of what is created
- there. `create[Directory]` and `create[File]` bring an entry into being:
+ Un chemin nomme un emplacement. Qu'il s'agisse d'un fichier ou d'un répertoire dépend de ce
+ qui y est créé. `create[Directory]` et `create[File]` font exister une entrée :

```scala fuel
typecheck
##
directory.create[Directory]()
file.create[File]()
```

- By default, creating refuses to destroy anything. Creating over something which exists, or
- under a parent which does not, is an `Io.Error`. Where either is the intent, a flag says so,
- at the place where it happens, instead of an import saying it for the whole file:
+ Par défaut, la création refuse de détruire quoi que ce soit. Créer par-dessus quelque
+ chose qui existe, ou sous un parent qui n'existe pas, est une `Io.Error`. Lorsque l'un ou
+ l'autre est voulu, une option le dit, à l'endroit où cela se produit, au lieu qu'un import
+ le dise pour tout le fichier :

```scala fuel
run
##
directory.create[Directory](CreateFlag.Replace, CreateFlag.Parents)
file.create[File](CreateFlag.Replace)
(directory / "deep" / "nested").create[Directory](CreateFlag.Parents)

Out.println(file.existent().inspect)
Out.println((directory / "deep" / "nested").existent().inspect)
```

- `Fifo` and `Sock` are entries too, and are created in the same way.
+ `Fifo` et `Sock` sont aussi des entrées, et se créent de la même façon.

- ### Reading and writing
+ ### Lire et écrire

- Where a whole small file is wanted, `read` and `write` act directly on the path:
+ Lorsqu'on veut un petit fichier en entier, `read` et `write` agissent directement sur le
+ chemin :

```scala fuel
run
##
file.write(t"Hello world")
Out.println(file.read[Text])
```

- `read` takes the type to read the file as. `Text` is decoded with the charset in scope, and a
- type with a decoder for a format reads in the same way, so `file.read[Json]` parses a
- [JSON](json.md#parsing) file.
+ `read` prend le type sous lequel lire le fichier. `Text` est décodé avec le jeu de
+ caractères dans la portée, et un type doté d'un décodeur pour un format se lit de la même
+ façon, si bien que `file.read[Json]` analyse un fichier [JSON](json.md#parsing).

- A larger file is read and written by *opening* it. `open` names the form to open the path as,
- and the mode to open it in, and runs a block with a handle. Inside the block, `file` is the
- open handle:
+ Un fichier plus volumineux se lit et s'écrit en l'*ouvrant*. `open` nomme la forme sous
+ laquelle ouvrir le chemin, et le mode dans lequel l'ouvrir, et exécute un bloc avec une
+ poignée. À l'intérieur du bloc, `file` est la poignée ouverte :

```scala fuel
run
##
file.open[File](Write, OpenFlag.Create): handle ?=>
  handle.write(Chain(t"Hello, world".in[Data]))

Out.println(file.open[File]()(file.read[Text]))
```

- The handle exists only for the duration of the block, so the file's descriptor cannot outlive
- the scope which owns it. This is the shape described under
- [delimited scopes](../../philosophy/delimited-scopes.md), and
- [capture checking](../../philosophy/capture-checking.md) enforces it.
+ La poignée n'existe que pendant la durée du bloc, si bien que le descripteur du fichier ne
+ peut pas survivre à la portée qui le possède. C'est la forme décrite sous
+ [portées délimitées](../../philosophy/delimited-scopes.md), et le
+ [contrôle des captures](../../philosophy/capture-checking.md) l'impose.

- The mode is not merely a flag at runtime. It is carried in the handle's type, as a set of
- *grants*. Opening with the default mode, `Read`, gives a handle which grants only reading, and
- `Read & Write` grants both. Writing through a handle which was opened only for reading does
- not compile:
+ Le mode n'est pas un simple indicateur à l'exécution. Il est porté dans le type de la poignée,
+ sous forme d'un ensemble d'*autorisations*. Ouvrir avec le mode par défaut, `Read`, donne
+ une poignée qui n'autorise que la lecture, et `Read & Write` autorise les deux. Écrire à
+ travers une poignée qui n'a été ouverte qu'en lecture ne compile pas :

```scala fuel
compile
errors show
##
file.open[File](): handle ?=>
  handle.write(Chain(t"Hello, world".in[Data]))
```

- A whole class of mistakes thereby becomes a compile error: writing through a read-only handle,
- and holding a descriptor past its scope.
+ Toute une classe d'erreurs devient ainsi une erreur de compilation : écrire à travers une
+ poignée en lecture seule, et conserver un descripteur au-delà de sa portée.

- Opening a path which names a directory confines what may be reached through it. Paths which
- are derived from the handle live on a *fresh plane* of their own. `..` does not compile on
- such a path, and a path obtained from one open directory cannot be written under another:
+ Ouvrir un chemin qui nomme un répertoire restreint ce qui peut être atteint à travers lui.
+ Les chemins dérivés de la poignée vivent sur un *plan neuf* qui leur est propre. `..` ne
+ compile pas sur un tel chemin, et un chemin obtenu à partir d'un répertoire ouvert ne peut
+ pas être écrit sous un autre :

```scala fuel
run
##
val names = directory.open[Directory](Read & Write): dir ?=>
  (dir / "greeting.txt").overwrite(t"Hello directory")
  dir.base.entries.map(_.name)

Out.println(names.to[List].has(t"greeting.txt").inspect)
```

- ### Exclusive and shared access
+ ### Accès exclusif et partagé

- `Exclusive` is a third grant, beside `Read` and `Write`, and it means what it says. No other
- scope in the program may hold an overlapping path open while it lasts. Overlap is by
- containment, not by equality: a directory and something beneath it overlap, and two siblings
- do not.
+ `Exclusive` est une troisième autorisation, à côté de `Read` et `Write`, et elle signifie
+ ce qu'elle dit. Aucune autre portée du programme ne peut tenir ouvert un chemin qui se
+ chevauche avec le sien tant qu'elle dure. Le chevauchement s'entend par inclusion, non par
+ égalité : un répertoire et quelque chose qui se trouve sous lui se chevauchent, deux frères
+ non.

```scala fuel
typecheck
##
directory.open[Directory](Read & Exclusive): dir ?=>
  ()
```

- Two ordinary reads may coexist. An exclusive open conflicts with an overlapping open in either
- direction, whether the exclusive scope is the outer or the inner one. The conflict is an
- `Io.Error` whose reason is `Busy`. It is raised at the second open, and not discovered later
- as corruption. The claim is released when the scope ends, however it ends.
+ Deux lectures ordinaires peuvent coexister. Une ouverture exclusive entre en conflit avec
+ une ouverture qui la chevauche dans un sens comme dans l'autre, que la portée exclusive soit
+ l'externe ou l'interne. Le conflit est une `Io.Error` dont la raison est `Busy`. Elle est
+ levée à la seconde ouverture, et non découverte plus tard sous forme de corruption. La
+ réservation est libérée à la fin de la portée, quelle que soit la façon dont celle-ci se
+ termine.

- For a *file*, as opposed to a directory, `Exclusive` also takes a lock from the operating
- system, so exclusivity holds against other processes, and not merely within this one. `Shared`
- is the reader's counterpart. Any number of shared holders may coexist, and an exclusive open
- waits for them, or fails at once if the lock is requested without blocking. A lock may also
- cover a range of bytes instead of the whole file, for the record-oriented layouts which
- databases use.
+ Pour un *fichier*, par opposition à un répertoire, `Exclusive` prend aussi un verrou auprès
+ du système d'exploitation, si bien que l'exclusivité vaut vis-à-vis des autres processus,
+ et pas seulement au sein de celui-ci. `Shared` est le pendant du lecteur. Un nombre
+ quelconque de détenteurs partagés peut coexister, et une ouverture exclusive les attend, ou
+ échoue immédiatement si le verrou est demandé sans blocage. Un verrou peut aussi couvrir une
+ plage d'octets au lieu du fichier entier, pour les dispositions orientées enregistrements
+ qu'utilisent les bases de données.

- The four grants, side by side:
+ Les quatre autorisations, côte à côte :

```tabular
- column  Grant
- column  Permits
- column  Conflicts with
- row  `Read`  reading, and is the default mode  an overlapping `Exclusive` open
- row  `Write`  writing  an overlapping `Exclusive` open
- row  `Exclusive`  no other overlapping open in the program, and for a file an OS lock  any overlapping open, as `Busy`
- row  `Shared`  for a file, a shared OS lock, held alongside any number of others  an `Exclusive` open, which waits for it
+ column  Autorisation
+ column  Permet
+ column  Entre en conflit avec
+ row  `Read`  la lecture, et c'est le mode par défaut  une ouverture `Exclusive` qui la chevauche
+ row  `Write`  l'écriture  une ouverture `Exclusive` qui la chevauche
+ row  `Exclusive`  aucune autre ouverture chevauchante dans le programme, et pour un fichier un verrou du système d'exploitation  toute ouverture qui la chevauche, sous forme de `Busy`
+ row  `Shared`  pour un fichier, un verrou partagé du système d'exploitation, détenu aux côtés d'un nombre quelconque d'autres  une ouverture `Exclusive`, qui l'attend
```

- ### Creating as a scope
+ ### La création comme portée

- `create` is the counterpart of `open`. Where opening acts on something which exists, creating
- brings it into being, and gives a handle over it for the same scope. Content written within
- the scope is committed when the block completes, and a scope which fails leaves nothing
- behind:
+ `create` est le pendant d'`open`. Là où l'ouverture agit sur quelque chose qui existe, la
+ création le fait exister, et donne une poignée sur lui pour la même portée. Le contenu
+ écrit dans la portée est validé lorsque le bloc se termine, et une portée qui échoue ne
+ laisse rien derrière elle :

```scala fuel
typecheck
##
val target = directory / "payload.bin"

target.create[File](): handle ?=>
  handle.write(Chain(t"payload".in[Data]))

(directory / "inner").create[Directory](): dir ?=>
  (dir / "inner.txt").overwrite(t"hello")
```

- A *scratch* directory is created and removed by its scope, whether the scope succeeds or
- fails. A scratch *file* is likewise, for a throwaway index or a test fixture. Its handle also
- knows its `path`, for whatever must be told where the file is:
+ Un répertoire *temporaire* est créé et supprimé par sa portée, que celle-ci
+ réussisse ou échoue. Il en va de même d'un *fichier* temporaire, pour un index jetable ou une
+ fixture de test. Sa poignée connaît aussi son `path`, pour tout ce à qui il faut indiquer
+ où se trouve le fichier :

```scala fuel
run
##
directory.open[Scratch](Read & Write): scratch ?=>
  (scratch / "file.txt").overwrite(t"data")

val content = directory.open[Scratch.File](Read & Write): file ?=>
  file.write(Chain(t"data".in[Data]))
  file.path.read[Text]

Out.println(content)
```

- ### Creation masks
+ ### Masques de création

- A process creates files under its *umask*: the permission bits which the operating system
- withholds from everything it creates. Ordinarily that is the end of the matter. But a process
- which serves several callers at once, such as a [daemon](daemons.md), has one umask and many
- invocations, each with its own.
+ Un processus crée des fichiers sous son *umask* : les bits de permission que le système
+ d'exploitation retire de tout ce qu'il crée. D'ordinaire, l'affaire s'arrête là. Mais un
+ processus qui sert plusieurs appelants à la fois, comme un [démon](daemons.md), a un seul
+ umask et de nombreuses invocations, chacune avec le sien.

- The mask can therefore be a contextual value. A `Umask` in scope is applied to every file and
- directory which is created while it applies. It is requested at creation, so the entry never
- exists with wider permissions than were asked for. Where none is in scope, `Umask.process`
- applies, which is the operating system's own, exactly as before:
+ Le masque peut donc être une valeur contextuelle. Un `Umask` dans la portée est appliqué à
+ chaque fichier et répertoire créé tant qu'il s'applique. Il est demandé à la création, de
+ sorte que l'entrée n'existe jamais avec des permissions plus larges que celles demandées.
+ Lorsqu'aucun n'est dans la portée, `Umask.process` s'applique, c'est-à-dire celui du
+ système d'exploitation, exactement comme auparavant :

```scala
def privately(): Unit =
  given umask: Umask = Umask(0x3f)

  (directory / "secret.txt").create[File](): handle ?=>
    handle.write(Chain(t"payload".in[Data]))
```

- `0x3f` is the mask written `077` in octal, which leaves a file readable and writable by its
- owner alone. A mask is parsed from the octal form which `umask` prints, with
- `Umask.parse(t"022")`, and `octal` writes it back in the same form. A daemon's service handle
- is a `Umask.Provider`, so within a `cli` block the invocation's mask is already in scope, with
- no further declaration.
+ `0x3f` est le masque qui s'écrit `077` en octal, qui laisse un fichier lisible et
+ modifiable par son seul propriétaire. Un masque s'analyse à partir de la forme octale
+ qu'affiche `umask`, avec `Umask.parse(t"022")`, et `octal` le réécrit sous la même forme.
+ La poignée de service d'un démon est un `Umask.Provider`, si bien que dans un bloc `cli`,
+ le masque de l'invocation est déjà dans la portée, sans autre déclaration.

- ### The opening pattern, generally
+ ### Le schéma d'ouverture, en général

- `open` is not specific to files. A great many things share the same shape. A value says
- *where* something is, whether a path, a buffer in memory or a URL, and getting at its contents
- is a distinct, scoped act. Naming that pattern once means that it reads the same wherever it
- applies.
+ `open` n'est pas propre aux fichiers. Un grand nombre de choses partagent la même forme.
+ Une valeur dit *où* se trouve quelque chose, qu'il s'agisse d'un chemin, d'un tampon en
+ mémoire ou d'une URL, et accéder à son contenu est un acte distinct, délimité par une
+ portée. Nommer ce schéma une fois pour toutes fait qu'il se lit de la même façon partout
+ où il s'applique.

- An `Openable` instance relates a target to a *form*. That is why the form is a type argument,
- and is not implied by the target. The same path opens as a `File`, as a `Directory`, as a
- `Zip` [archive](archives.md), as a [PDF](pdf.md) or as an [image](images.md), and each is a
- different handle with different operations. The form may be omitted where a target has only
- one instance. Where it has several, the ambiguity is reported, with the alternatives listed.
+ Une instance `Openable` relie une cible à une *forme*. C'est pourquoi la forme est un
+ argument de type, et n'est pas impliquée par la cible. Le même chemin s'ouvre comme un
+ `File`, comme un `Directory`, comme une [archive](archives.md) `Zip`, comme un
+ [PDF](pdf.md) ou comme une [image](images.md), et chacun est une poignée différente aux
+ opérations différentes. La forme peut être omise lorsqu'une cible n'a qu'une instance.
+ Lorsqu'elle en a plusieurs, l'ambiguïté est signalée, avec la liste des alternatives.

- Behind the pattern are the [typeclasses][typeclass] `Openable`, `Creatable` and `Sessional`,
- for `open`, `create` and `session`. The mode which the first two take is a combination of
- `Grant`s.
+ Derrière ce schéma se trouvent les [classes de types][classe de types] `Openable`,
+ `Creatable` et `Sessional`, pour `open`, `create` et `session`. Le mode que prennent les deux
+ premières est une combinaison de `Grant`s.

- The forms which this page describes are:
+ Les formes que décrit cette page sont :

```tabular
- column  Form
- column  The handle gives
- row  `File`  the file's contents, to read or write as the grants allow
- row  `Directory`  paths confined beneath the directory, on a fresh plane
- row  `Scratch`  a directory removed when the scope ends
- row  `Scratch.File`  a file removed when the scope ends, and its `path`
- row  `Ram`  positional reads and writes over a memory-mapped file, of any size
- row  `Watch`  a stream of the changes beneath a directory
+ column  Forme
+ column  La poignée donne
+ row  `File`  le contenu du fichier, à lire ou écrire selon ce que permettent les autorisations
+ row  `Directory`  des chemins confinés sous le répertoire, sur un plan neuf
+ row  `Scratch`  un répertoire supprimé à la fin de la portée
+ row  `Scratch.File`  un fichier supprimé à la fin de la portée, et son `path`
+ row  `Ram`  des lectures et écritures positionnelles sur un fichier projeté en mémoire, de n'importe quelle taille
+ row  `Watch`  un flux des changements sous un répertoire
```

- Three things follow uniformly:
+ Trois choses en découlent uniformément :

- - The *mode*, which is `Read`, `Write`, `Exclusive`, `Shared` or a combination of them, is
-   carried in the handle's type, so the grants which are requested are the operations which
-   are permitted.
- - Flags after the mode belong to the instance. They are specific to the kind of thing being
-   opened, and an irrelevant flag does not typecheck.
- - The handle is a [capability] confined to the block, so neither it, nor anything derived from
-   it, can escape.
+ - Le *mode*, qui est `Read`, `Write`, `Exclusive`, `Shared` ou une combinaison de ceux-ci,
+   est porté dans le type de la poignée, de sorte que les autorisations demandées sont les
+   opérations permises.
+ - Les options qui suivent le mode appartiennent à l'instance. Elles sont propres au genre
+   de chose ouverte, et une option sans rapport ne passe pas la vérification des types.
+ - La poignée est une [capacité] confinée au bloc, si bien que ni elle, ni rien de ce qui en
+   dérive, ne peut s'échapper.

- `create` is the same pattern for something which does not yet exist. `session` is the same
- pattern for a target whose access is a conversation with something running, such as a
- [browser](web-automation.md) or a [program being debugged](debugging.md), and not a handle
- over stored bytes. In each case the scope is the lifetime, and the end of the block is the end
- of the access.
+ `create` est le même schéma pour quelque chose qui n'existe pas encore. `session` est le
+ même schéma pour une cible dont l'accès est une conversation avec quelque chose en cours
+ d'exécution, comme un [navigateur](web-automation.md) ou un
+ [programme en cours de débogage](debugging.md), et non une poignée sur des octets stockés.
+ Dans chaque cas, la portée est la durée de vie, et la fin du bloc est la fin de l'accès.

- ### Positional access
+ ### Accès positionnel

- A file which is opened as `Ram` is mapped into memory, and serves reads and writes at any
- position, without streaming through it. The `Write` grant is required to write, as everywhere
- else:
+ Un fichier ouvert comme `Ram` est projeté en mémoire, et sert des lectures et des écritures
+ à n'importe quelle position, sans le parcourir en flux. L'autorisation `Write` est requise
+ pour écrire, comme partout ailleurs :

```scala fuel
run
##
file.write(t"Hello world")

val middle = file.open[Ram](): ram ?=>
  ram(2, 3)

file.open[Ram](Read & Write): ram ?=>
  ram(3L) = t"XYZ".in[Data]

Out.println(middle.utf8)
Out.println(file.read[Text])
```

- `mapped` maps a whole file for reading, and lends a `Terrain` view of it for the length of a
- block. A `Terrain` is an `Expanse`, with its `size` and its copying `read`, whose bytes are in
- memory. Nothing is copied: integers are loaded from the mapped memory in place, little-endian
- and at any offset, and a `slice` is a narrower view of the same memory. Several processes
- which map one file share a single copy of it, in the operating system's page cache, which is
- what makes a mapped file a way of sharing a large index between programs:
+ `mapped` projette un fichier entier en lecture, et en prête une vue `Terrain` pendant la
+ durée d'un bloc. Un `Terrain` est un `Expanse`, avec sa `size` et son `read` qui copie, dont
+ les octets sont en mémoire. Rien n'est copié : les entiers sont chargés sur place depuis la
+ mémoire projetée, en petit-boutiste et à n'importe quel décalage, et une `slice` est une vue
+ plus étroite de la même mémoire. Plusieurs processus qui projettent un même fichier en
+ partagent une seule copie, dans le cache de pages du système d'exploitation, ce qui fait
+ d'un fichier projeté un moyen de partager un gros index entre programmes :

```scala fuel
run
##
file.write(t"0123456789ABCDEF")

val word = file.mapped(_.u32(0L))

val letters = file.mapped: view =>
  val tail = view.slice(10L, 6L)
  tail.read(0L, 3).utf8

Out.println(word.show)
Out.println(letters)
```

- The loads are these:
+ Les chargements sont les suivants :

```tabular
- column  Method
- column  Gives
- row  `u8`, `u16`  an unsigned byte or 16-bit integer, as an `Int`
- row  `u32`  an unsigned 32-bit integer, as a `Long`
- row  `i32`  the same 32 bits as a signed `Int`
- row  `u64`  a 64-bit integer, as a `Long`
- row  `slice`  a view of part of the memory, with no copy
- row  `read`  some of the bytes as `Data`, for a value which must outlive the view
- row  `same`  whether the bytes at an offset are those of some `Data`, compared in place
- row  `compare`  the order of some bytes relative to some `Data`, compared in place
+ column  Méthode
+ column  Donne
+ row  `u8`, `u16`  un octet ou un entier 16 bits non signé, sous forme d'`Int`
+ row  `u32`  un entier 32 bits non signé, sous forme de `Long`
+ row  `i32`  les mêmes 32 bits sous forme d'`Int` signé
+ row  `u64`  un entier 64 bits, sous forme de `Long`
+ row  `slice`  une vue d'une partie de la mémoire, sans copie
+ row  `read`  une partie des octets sous forme de `Data`, pour une valeur qui doit survivre à la vue
+ row  `same`  si les octets à un décalage sont ceux de certaines `Data`, comparés sur place
+ row  `compare`  l'ordre de certains octets par rapport à certaines `Data`, comparés sur place
```

- The file is unmapped when the block ends. The view, and every slice of it, is a [capability]
- confined to the block, so a load from memory which is no longer mapped cannot be written. A
- method which receives a view declares it as `Terrain^`. The same loads are
- available on `Data`, with a big-endian `u16be`, `i32be`, `u32be` and `u64be` beside them, so a
- parser reads a format the same way from either.
+ Le fichier est déprojeté à la fin du bloc. La vue, et chacune de ses tranches, est une
+ [capacité] confinée au bloc, si bien qu'on ne peut pas écrire de chargement depuis une
+ mémoire qui n'est plus projetée. Une méthode qui reçoit une vue la déclare comme
+ `Terrain^`. Les mêmes chargements sont disponibles sur `Data`, avec à côté des versions
+ gros-boutistes `u16be`, `i32be`, `u32be` et `u64be`, de sorte qu'un analyseur lit un
+ format de la même façon depuis l'un ou l'autre.

- A mapped file should be replaced, by moving a new file onto its name, and not rewritten in
- place: a view goes on seeing the file it mapped, unchanged, for as long as it is held. Mapping
- is available on the JVM and on Scala Native; WASI has no memory mapping, and reports
- `Unsupported`.
+ Un fichier projeté doit être remplacé, en déplaçant un nouveau fichier sur son nom, et non
+ réécrit sur place : une vue continue de voir le fichier qu'elle a projeté, inchangé, aussi
+ longtemps qu'elle est conservée. La projection est disponible sur la JVM et sur Scala
+ Native ; WASI n'a pas de projection en mémoire, et signale `Unsupported`.

- `Terrain(data)` is a view of bytes which are already in memory, for reading a format which is
- designed for a mapped file from `Data` instead. A path is `Viewable`, the typeclass of a value
- which can lend a view of its bytes, so a format which is read in place is opened straight from
- its file. [RaTEL](tel.md#ratel) is such a format, a TEL document laid out so that one record is
- found without reading the rest, and `file.open[Ratel]()` maps the file for the length of its
- block.
+ `Terrain(data)` est une vue d'octets déjà en mémoire, pour lire à partir de `Data` un
+ format conçu pour un fichier projeté. Un chemin est `Viewable`, la classe de types d'une
+ valeur capable de prêter une vue de ses octets, de sorte qu'un format lu sur place s'ouvre
+ directement depuis son fichier. [RaTEL](tel.md#ratel) est un tel format, un document TEL
+ disposé de sorte qu'un enregistrement se trouve sans lire le reste, et
+ `file.open[Ratel]()` projette le fichier pendant la durée de son bloc.

- `hold` maps a whole file for changing where it lies, and lends a `Holding`: a `Terrain` which
- is stored to as well as loaded from. `set8`, `set16`, `set32` and `set64` store an integer at
- an offset, `write` stores some `Data`, `zero` clears a range and `move` copies one within the
- file, and `expand` makes the holding longer, and the file with it. A file which is not there is
- created, at the size which `hold` is given. An exclusive lock is held on the file for the
- length of the block, so a second `hold` of the same file, in this program or another, is
- refused with `Busy`:
+ `hold` projette un fichier entier pour le modifier là où il se trouve, et prête un `Holding` :
+ un `Terrain` dans lequel on stocke aussi bien qu'on charge. `set8`, `set16`, `set32` et `set64`
+ stockent un entier à un décalage, `write` stocke des `Data`, `zero` efface une plage et `move`
+ en copie une à l'intérieur du fichier, et `expand` allonge la tenure, et le fichier avec lui. Un
+ fichier qui n'existe pas est créé, à la taille donnée à `hold`. Un verrou exclusif est tenu
+ sur le fichier pendant la durée du bloc, si bien qu'un second `hold` du même fichier, dans ce
+ programme ou dans un autre, est refusé avec `Busy` :

```scala fuel
run
##
val ledger = directory / "ledger"

ledger.hold(16L): holding =>
  holding.set32(0L, 1599L)
  holding.expand(32L)
  holding.set64(24L, 1616L)

val first = ledger.mapped(_.u32(0L))
val last = ledger.mapped(_.u64(24L))
Out.println(t"${first.show}, then ${last.show}")
```

- A holding hands out no address into its memory, since `expand` may move it: every load and store
- is at an offset, and a `slice` of a holding reads through it, so it stays good afterwards. What
- was stored is in the file when the block ends. It is what a format which is altered in place
- is altered through, as an image of [RaTEL](tel.md#ratel) is by the program which owns it.
- `Holding(size)` is one on the heap, for an image which is not yet a file. A file is mapped for
- changing on the JVM and on Scala Native; WASI, which can neither map nor lock a file, reports
- `Unsupported`.
+ Une tenure ne donne aucune adresse dans sa mémoire, puisque `expand` peut la déplacer : chaque
+ chargement et chaque stockage se fait à un décalage, et une `slice` d'une tenure lit à travers
+ elle, si bien qu'elle reste valable ensuite. Ce qui a été stocké est dans le fichier lorsque le
+ bloc se termine. C'est par elle qu'un format modifié sur place est modifié, comme l'est une image
+ [RaTEL](tel.md#ratel) par le programme qui la possède. `Holding(size)` en est une sur le tas, pour
+ une image qui n'est pas encore un fichier. Un fichier est projeté pour modification sur la JVM et
+ sur Scala Native ; WASI, qui ne peut ni projeter ni verrouiller un fichier, signale `Unsupported`.

- ### Publishing versions
+ ### Publier des versions

- A mapped file is a way for one program to share a large value with others, without a copy in
- each. The program which produces the value *owns* its name, and publishes one version after
- another under it. The programs which use it map whichever version is current, and move on to
- the next when they choose to. Nothing arbitrates between them, because two rules are kept:
+ Un fichier projeté est un moyen pour un programme de partager une grosse valeur avec
+ d'autres, sans en avoir une copie dans chacun. Le programme qui produit la valeur
+ *possède* son nom, et publie une version après l'autre sous ce nom. Les programmes qui
+ l'utilisent projettent la version courante, quelle qu'elle soit, et passent à la suivante
+ quand ils le décident. Rien n'arbitre entre eux, car deux règles sont respectées :

- - A published file never changes. Each version is written beside the name and moved over it
-   in one step, so a reader which has mapped a version goes on seeing it, whole.
- - A name has one owner. `claim` holds an operating-system lock on `<name>.lock` for as long as
-   its block runs, and a second owner, in this process or another, is refused with `Busy`.
+ - Un fichier publié ne change jamais. Chaque version est écrite à côté du nom et déplacée
+   dessus en une seule étape, si bien qu'un lecteur qui a projeté une version continue de
+   la voir, entière.
+ - Un nom a un seul propriétaire. `claim` détient un verrou du système d'exploitation sur
+   `<name>.lock` aussi longtemps que son bloc s'exécute, et un second propriétaire, dans ce
+   processus ou un autre, est refusé avec `Busy`.

- `claim` gives an `Owner`, whose `publish` writes a value in whatever form its `Encodable in Data`
- instance gives, and returns the `Edition` it published:
+ `claim` donne un `Owner`, dont le `publish` écrit une valeur sous la forme que donne son
+ instance `Encodable in Data`, et renvoie l'`Edition` qu'il a publiée :

```scala fuel
run
##
val edition = file.claim[Text]: owner =>
  owner.publish(t"first edition")

Out.println(file.read[Text])
```

- `observe` gives an `Observer`, which reads what the owner publishes. Its `map` lends a
- `Terrain` view of the version it holds, and its `refresh` moves it to the version which the
- name now leads to, and says whether there was one:
+ `observe` donne un `Observer`, qui lit ce que le propriétaire publie. Son `map` prête une vue
+ `Terrain` de la version qu'il détient, et son `refresh` le fait passer à la version vers
+ laquelle mène désormais le nom, et indique s'il y en avait une :

```scala fuel
run
##
file.claim[Text](_.publish(t"one"))

file.observe[Text]: observer =>
  file.claim[Text](_.publish(t"two"))
  val before = observer.map(_.u8(0L))
  val moved = observer.refresh()
  val after = observer.map(_.u8(0L))
  Out.println(t"$before, then $after")
```

- An observer takes no lock, so it never delays the owner, and the owner never delays it. A view
- which is in use when a `refresh` happens, on this thread or another, stays valid until its
- block ends. Two editions are equal exactly when they are the same version of the file; they
- have no order.
+ Un observateur ne prend aucun verrou, si bien qu'il ne retarde jamais le propriétaire, et que le
+ propriétaire ne le retarde jamais. Une vue en cours d'utilisation lorsqu'un `refresh` a lieu,
+ sur ce thread ou un autre, reste valide jusqu'à la fin de son bloc. Deux éditions sont égales
+ exactement lorsqu'elles sont la même version du fichier ; elles n'ont pas d'ordre.

- The type which `claim` and `observe` are given is what is published under the name. For a
- form which is read in place, an observer's `open` maps the current version and opens it as
- that type, in one block: an observer of a `Ratel of Playbill` opens an image of a `Playbill`,
- and refuses one of anything else, as [RaTEL](tel.md#ratel) shows.
+ Le type donné à `claim` et à `observe` est ce qui est publié sous le nom. Pour une forme qui se
+ lit sur place, le `open` d'un observateur projette la version courante et l'ouvre comme ce type,
+ en un seul bloc : un observateur d'un `Ratel of Playbill` ouvre une image d'un `Playbill`, et
+ refuse celle de toute autre chose, comme le montre [RaTEL](tel.md#ratel).

- An observer keeps the version it has mapped in existence, even after a newer one has replaced
- it under the name, so a long-lived observer should `refresh` when it learns that there is
- something new, whether from a message or from [watching](#watching-for-changes) the name. The
- lock is advisory, and is dependable between cooperating programs on one machine's own
- filesystems, but not over a network filesystem.
+ Un observateur maintient en existence la version qu'il a projetée, même après qu'une plus
+ récente l'a remplacée sous le nom, si bien qu'un observateur de longue durée doit appeler
+ `refresh` lorsqu'il apprend qu'il y a du nouveau, que ce soit par un message ou en
+ [surveillant](#watching-for-changes) le nom. Le verrou est consultatif, et fiable entre des
+ programmes coopérants sur les systèmes de fichiers propres à une machine, mais pas sur un
+ système de fichiers en réseau.

- A `Slice` is a window onto part of a file, with an offset and an extent. It is opened for
- reading or writing like a file of its own. A region of a large file can therefore be streamed
- in or out without the rest being touched, and a write cannot stray outside its window.
+ Une `Slice` est une fenêtre sur une partie d'un fichier, avec un décalage et une étendue.
+ Elle s'ouvre en lecture ou en écriture comme un fichier à part entière. Une région d'un
+ gros fichier peut donc être lue ou écrite en flux sans toucher au reste, et une écriture ne
+ peut pas sortir de sa fenêtre.

- ### Copying, moving and deleting
+ ### Copier, déplacer et supprimer

- A path is copied, moved, linked or deleted with operations which name the destination, or act
- in place:
+ Un chemin se copie, se déplace, se lie ou se supprime avec des opérations qui nomment la
+ destination, ou qui agissent sur place :

```scala fuel
run
##
file.write(t"Hello world")
file.copyTo(directory / "backup.txt")
file.moveTo(directory / "renamed.txt")
(directory / "backup.txt").delete()

Out.println(file.existent().inspect)
Out.println((directory / "renamed.txt").existent().inspect)
Out.println((directory / "backup.txt").existent().inspect)
```

- Each consults the policies in scope for the awkward cases, and each may raise an `Io.Error`.
+ Chacune consulte les politiques dans la portée pour les cas délicats, et chacune peut
+ lever une `Io.Error`.

- Those policies are not defaults which can be left alone. Moving a file onto a path where
- something already exists either destroys that thing, or refuses to. Neither answer is right in
- general, and choosing one silently would make the wrong programs compile. So `moveTo` requires
- either `overwritePreexisting` or `failOnPreexisting` in scope, and calling it with neither is
- a compile error. That is instructive, as well as cautious: a reader who had not realized that
- the question needed an answer is told that it does.
+ Ces politiques ne sont pas des valeurs par défaut que l'on peut ignorer. Déplacer un
+ fichier vers un chemin où quelque chose existe déjà, soit détruit cette chose, soit refuse
+ de le faire. Aucune des deux réponses n'est juste en général, et en choisir une en silence
+ ferait compiler les mauvais programmes. C'est pourquoi `moveTo` exige soit
+ `overwritePreexisting`, soit `failOnPreexisting` dans la portée, et l'appeler sans aucun
+ des deux est une erreur de compilation. C'est instructif autant que prudent : un lecteur
+ qui ne s'était pas rendu compte que la question appelait une réponse apprend qu'elle en
+ appelle une.

- The choice also changes what must be handled. With `failOnPreexisting` in scope, a collision
- is a failure to deal with. With `overwritePreexisting`, it cannot arise, and the obligation
- goes away with it.
+ Le choix modifie aussi ce qui doit être traité. Avec `failOnPreexisting` dans la portée,
+ une collision est un échec à traiter. Avec `overwritePreexisting`, elle ne peut pas
+ survenir, et l'obligation disparaît avec elle.

- The policies are contextual values, so they can be imported for a whole file, or narrowed to
- a single block. Each answers one question, and comes as one of a pair, from the package
- `filesystemOptions`:
+ Les politiques sont des valeurs contextuelles, elles peuvent donc être importées pour tout
+ un fichier, ou restreintes à un seul bloc. Chacune répond à une question, et fait partie
+ d'une paire, issue du package `filesystemOptions` :

```tabular
- column  Question
- column  Yes
- column  No
- row  Replace what is already at the destination?  `overwritePreexisting`  `failOnPreexisting`
- row  Empty a directory before deleting it?  `deleteRecursively`  `deleteOnlyEmpty`
- row  Create missing parent directories?  `createNonexistentParents`  `requireParents`
- row  Follow symbolic links?  `dereferenceSymlinks`  `preserveSymlinks`
- row  Move atomically?  `moveAtomically`  `moveNonAtomically`
+ column  Question
+ column  Oui
+ column  Non
+ row  Remplacer ce qui se trouve déjà à la destination ?  `overwritePreexisting`  `failOnPreexisting`
+ row  Vider un répertoire avant de le supprimer ?  `deleteRecursively`  `deleteOnlyEmpty`
+ row  Créer les répertoires parents manquants ?  `createNonexistentParents`  `requireParents`
+ row  Suivre les liens symboliques ?  `dereferenceSymlinks`  `preserveSymlinks`
+ row  Déplacer atomiquement ?  `moveAtomically`  `moveNonAtomically`
```

- Replacing an existing entry deletes it first, so `overwritePreexisting` needs one of the
- deletion policies in scope as well.
+ Remplacer une entrée existante la supprime d'abord, si bien qu'`overwritePreexisting`
+ nécessite aussi l'une des politiques de suppression dans la portée.

- Each operation consults the policies which bear on it:
+ Chaque opération consulte les politiques qui la concernent :

```tabular
- column  Operation
- column  Acts on
- column  Policies consulted
- row  `copyTo(path)`  the path given  replacing, parents, symbolic links
- row  `copyInto(directory)`  a child of the directory, keeping the name  replacing, symbolic links
- row  `moveTo(path)`  the path given  replacing, parents, symbolic links, atomicity
- row  `moveInto(directory)`  a child of the directory, keeping the name  replacing, symbolic links, atomicity
- row  `symlinkTo(path)`  a new link at the path given, pointing here  replacing, parents
- row  `symlinkInto(directory)`  a new link in the directory, keeping the name  replacing
- row  `delete()`, `wipe()`  the path itself  deletion
+ column  Opération
+ column  Agit sur
+ column  Politiques consultées
+ row  `copyTo(path)`  le chemin donné  remplacement, parents, liens symboliques
+ row  `copyInto(directory)`  un enfant du répertoire, en conservant le nom  remplacement, liens symboliques
+ row  `moveTo(path)`  le chemin donné  remplacement, parents, liens symboliques, atomicité
+ row  `moveInto(directory)`  un enfant du répertoire, en conservant le nom  remplacement, liens symboliques, atomicité
+ row  `symlinkTo(path)`  un nouveau lien au chemin donné, pointant ici  remplacement, parents
+ row  `symlinkInto(directory)`  un nouveau lien dans le répertoire, en conservant le nom  remplacement
+ row  `delete()`, `wipe()`  le chemin lui-même  suppression
```

- The `…Into` forms create any missing parents themselves. Deleting a directory which has
- children is an `Io.Error`, whose reason is `DirectoryNotEmpty`, unless `deleteRecursively` is
- in scope. Recursive deletion is thus a policy which must be enabled deliberately.
+ Les formes `…Into` créent elles-mêmes les parents manquants. Supprimer un répertoire qui a
+ des enfants est une `Io.Error`, dont la raison est `DirectoryNotEmpty`, à moins que
+ `deleteRecursively` ne soit dans la portée. La suppression récursive est donc une
+ politique qui doit être activée délibérément.

- ### Listing and inspecting
+ ### Lister et inspecter

- `children` gives a directory's immediate children, and `descendants` gives its whole subtree.
- A path reports whether it exists, its size, and what kind of entry it is. A `glob` selects
- children by a pattern, in which `*` matches within a name, and a whole segment `**` spans
- directories:
+ `children` donne les enfants immédiats d'un répertoire, et `descendants` toute son
+ arborescence. Un chemin indique s'il existe, sa taille, et quel genre d'entrée il est. Un
+ `glob` sélectionne des enfants selon un motif, dans lequel `*` correspond à l'intérieur
+ d'un nom, et un segment entier `**` traverse les répertoires :

```scala fuel
run
##
(directory / "renamed.txt").write(t"Hello world")

Out.println((directory / "renamed.txt").existent().inspect)
Out.println((directory / "renamed.txt").filesize().inspect)
Out.println(directory.children.map(_.name).to[List].has(t"renamed.txt").inspect)
Out.println(directory.glob(Glob.parse(t"*.txt")).map(_.name).to[List].has(t"renamed.txt").inspect)
```

- Two paths can name one entry, through symbolic links, or through `/tmp` and `/private/tmp` on
- macOS. `canonical` is the entry's path as the filesystem itself knows it, with every link
- resolved. That is the path which a registry keyed by path should store:
+ Deux chemins peuvent nommer une même entrée, à travers des liens symboliques, ou à travers
+ `/tmp` et `/private/tmp` sous macOS. `canonical` est le chemin de l'entrée tel que le
+ système de fichiers lui-même le connaît, chaque lien étant résolu. C'est le chemin que
+ doit stocker un registre indexé par chemin :

```scala fuel
typecheck
##
val canonical = (directory / "renamed.txt").canonical()
```

- [Patterns](patterns.md) describes globs.
+ [Motifs](patterns.md) décrit les globs.

- ### Content-addressed storage
+ ### Stockage adressé par contenu

- A directory of blobs, each named by the digest of its own bytes, is a *content-addressed
- store*. A blob is written once, verified as it is written, and found by anything which knows
- its digest. `Cas` is that store. The [hash algorithm](hashing.md) is in its type, and in the
- type of every digest it accepts, so a SHA-256 store cannot be asked for a BLAKE3 digest:
+ Un répertoire de blobs, chacun nommé par le condensé de ses propres octets, est un
+ *stockage adressé par contenu*. Un blob est écrit une seule fois, vérifié au moment de son
+ écriture, et trouvé par quiconque connaît son condensé. `Cas` est ce stockage.
+ L'[algorithme de hachage](hashing.md) figure dans son type, et dans le type de chaque
+ condensé qu'il accepte, si bien qu'on ne peut pas demander un condensé BLAKE3 à un
+ stockage SHA-256 :

```scala
import providers.javaBaseProvider

val store: Cas in Sha2[256] on (Path on Linux) = Cas[Sha2[256]](directory, t".blob")
```

```scala fuel
run
##
val digest: Digest in Sha2[256] = store() = t"some bytes".in[Data]

Out.println(store(digest).let(_.utf8).or(t"missing"))
Out.println(store.has(digest).inspect)
Out.println(store.missing(List(digest)).inspect)
```

- A store has these operations:
+ Un stockage dispose des opérations suivantes :

```tabular
- column  Expression
- column  Does
- row  `store() = data`  stores the data under its own digest, and returns it; nothing if the blob is already there
- row  `store(digest) = data`  the same, after checking that the data has that digest, raising a `Cas.Error` if not
- row  `store(digest)`  the bytes, or `Unset`
- row  `store.has(digest)`  whether the blob is stored
- row  `store.missing(digests)`  those of a list of digests not yet stored
- row  `store.digests`  every digest stored
- row  `store.prune(digests)`  deletes every blob not in the set, and returns how many it deleted
+ column  Expression
+ column  Effet
+ row  `store() = data`  stocke les données sous leur propre condensé, et renvoie celui-ci ; ne fait rien si le blob est déjà présent
+ row  `store(digest) = data`  la même chose, après avoir vérifié que les données ont ce condensé, en levant une `Cas.Error` sinon
+ row  `store(digest)`  les octets, ou `Unset`
+ row  `store.has(digest)`  si le blob est stocké
+ row  `store.missing(digests)`  ceux d'une liste de condensés qui ne sont pas encore stockés
+ row  `store.digests`  tous les condensés stockés
+ row  `store.prune(digests)`  supprime chaque blob absent de l'ensemble, et renvoie le nombre de blobs supprimés
```

- `prune` is how a cache is trimmed to what is still referenced.
+ `prune` est la façon de réduire un cache à ce qui est encore référencé.

- A blob is written to a sibling with a unique name, and moved into place atomically, so a
- reader never sees a partial blob. The store's root is a `Path` here, but its type is the type
- of that root, and a `Cas.Storage` instance for another kind of root makes a store over it
- with the same interface. As a [collection](collections.md), a store is its digests:
- `store.size`, `store.has(digest)`, `store.at(digest)` and traversal all work on it.
+ Un blob est écrit dans un fichier voisin au nom unique, puis déplacé atomiquement à sa
+ place, si bien qu'un lecteur ne voit jamais un blob partiel. La racine du stockage est ici
+ un `Path`, mais son type est celui de cette racine, et une instance `Cas.Storage` pour un
+ autre genre de racine crée sur celle-ci un stockage doté de la même interface. En tant que
+ [collection](collections.md), un stockage est l'ensemble de ses condensés : `store.size`,
+ `store.has(digest)`, `store.at(digest)` et le parcours fonctionnent tous sur lui.

- ### Extended attributes and volumes
+ ### Attributs étendus et volumes

- The filesystem which a path sits on is a typed fact about it. `Ext4`, `Btrfs`, `Apfs`, `Ntfs`
- and `Dos` are the *axes* which a path can be matched against. A filesystem which supports
- extended attributes lets a path carry named metadata beyond its contents:
+ Le système de fichiers sur lequel se trouve un chemin est un fait typé le concernant.
+ `Ext4`, `Btrfs`, `Apfs`, `Ntfs` et `Dos` sont les *axes* auxquels un chemin peut être
+ confronté. Un système de fichiers qui prend en charge les attributs étendus permet à un
+ chemin de porter des métadonnées nommées en plus de son contenu :

```scala fuel
typecheck
##
val attributes = file match
  case Apfs(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case Ext4(path) => path.attribute(t"origin", t"soundness".in[Data]) yet path.attributes()
  case _          => Nil
```

- On btrfs, `subvolume` reports the subvolume which a path belongs to, and `subvolumeRoot`
- reports whether it is the top of one.
+ Sur btrfs, `subvolume` indique le sous-volume auquel appartient un chemin, et
+ `subvolumeRoot` indique s'il en est le sommet.

- ### Standard directories
+ ### Répertoires standard

- The conventional locations of a system are named values, resolved against the
- [environment](environment.md). The home directory and the paths beneath it, and the system
- directories under the root, are reached by navigating and applying:
+ Les emplacements conventionnels d'un système sont des valeurs nommées, résolues par
+ rapport à l'[environnement](environment.md). Le répertoire personnel et les chemins situés
+ en dessous, ainsi que les répertoires système sous la racine, s'atteignent par navigation
+ et application :

```scala fuel
typecheck
##
val home = Home[Path on Linux]()
val cache = Home.Cache[Path on Linux]()
val bin = Home.Local.Bin[Path on Linux]()
val share = Base.Usr.Share[Path on Linux]()
```

- Each object is named for the directory it stands for, capitalized, with any leading `.`
- dropped, since `.` is the separator between members. So `$HOME/.local/bin` is
- `Home.Local.Bin`, and `/var/lib` is `Base.Var.Lib`. Writing a standard location in this way,
- and not as text, means that a mistyped directory does not compile:
+ Chaque objet est nommé d'après le répertoire qu'il représente, avec une majuscule
+ initiale, sans l'éventuel `.` initial, puisque `.` est le séparateur entre membres. Ainsi,
+ `$HOME/.local/bin` s'écrit `Home.Local.Bin`, et `/var/lib` `Base.Var.Lib`. Écrire un
+ emplacement standard de cette façon, et non sous forme de texte, fait qu'un répertoire mal
+ orthographié ne compile pas :

```scala fuel
compile
errors show
##
Home.Cashe[Path on Linux]()
```

- These are the layouts, and whether `readOnly` marks each as not meant to be written to:
+ Voici les dispositions, et si `readOnly` marque chacune comme non destinée à l'écriture :

```tabular
- column  Layout
- column  Directory
- column  Read-only
- row  `Home`  `~`  no
- row  `Home.Cache`  `~/.cache`  no
- row  `Home.Config`  `~/.config`  no
- row  `Home.Local`  `~/.local`  no
- row  `Home.Local.Bin`  `~/.local/bin`  no
- row  `Home.Local.Lib`  `~/.local/lib`  no
- row  `Home.Local.Share`  `~/.local/share`  no
- row  `Home.Local.State`  `~/.local/state`  no
- row  `Base`  `/`  no
- row  `Base.Boot`  `/boot`  yes
- row  `Base.Dev`  `/dev`  no
- row  `Base.Dev.Shm`  `/dev/shm`  no
- row  `Base.Efi`  `/efi`  yes
- row  `Base.Etc`  `/etc`  no
- row  `Base.Home`  `/home`  no
- row  `Base.Proc`  `/proc`  no
- row  `Base.Proc(pid)`  `/proc/<pid>`  yes
- row  `Base.Proc.Sys`  `/proc/sys`  yes
- row  `Base.Root`  `/root`  no
- row  `Base.Run`  `/run`  no
- row  `Base.Run.Log`  `/run/log`  no
- row  `Base.Run.User`  `/run/user`  no
- row  `Base.Srv`  `/srv`  no
- row  `Base.Sys`  `/sys`  yes
- row  `Base.Tmp`  `/tmp`  no
- row  `Base.Usr`  `/usr`  yes
- row  `Base.Usr.Bin`  `/usr/bin`  yes
- row  `Base.Usr.Include`  `/usr/include`  yes
- row  `Base.Usr.Lib`  `/usr/lib`  yes
- row  `Base.Usr.Share`  `/usr/share`  yes
- row  `Base.Usr.Share.Doc`  `/usr/share/doc`  yes
- row  `Base.Usr.Share.Factory`  `/usr/share/factory`  yes
- row  `Base.Usr.Share.Factory.Etc`  `/usr/share/factory/etc`  yes
- row  `Base.Usr.Share.Factory.Var`  `/usr/share/factory/var`  yes
- row  `Base.Var`  `/var`  no
- row  `Base.Var.Cache`  `/var/cache`  no
- row  `Base.Var.Lib`  `/var/lib`  no
- row  `Base.Var.Log`  `/var/log`  no
- row  `Base.Var.Spool`  `/var/spool`  no
- row  `Base.Var.Tmp`  `/var/tmp`  no
+ column  Disposition
+ column  Répertoire
+ column  Lecture seule
+ row  `Home`  `~`  non
+ row  `Home.Cache`  `~/.cache`  non
+ row  `Home.Config`  `~/.config`  non
+ row  `Home.Local`  `~/.local`  non
+ row  `Home.Local.Bin`  `~/.local/bin`  non
+ row  `Home.Local.Lib`  `~/.local/lib`  non
+ row  `Home.Local.Share`  `~/.local/share`  non
+ row  `Home.Local.State`  `~/.local/state`  non
+ row  `Base`  `/`  non
+ row  `Base.Boot`  `/boot`  oui
+ row  `Base.Dev`  `/dev`  non
+ row  `Base.Dev.Shm`  `/dev/shm`  non
+ row  `Base.Efi`  `/efi`  oui
+ row  `Base.Etc`  `/etc`  non
+ row  `Base.Home`  `/home`  non
+ row  `Base.Proc`  `/proc`  non
+ row  `Base.Proc(pid)`  `/proc/<pid>`  oui
+ row  `Base.Proc.Sys`  `/proc/sys`  oui
+ row  `Base.Root`  `/root`  non
+ row  `Base.Run`  `/run`  non
+ row  `Base.Run.Log`  `/run/log`  non
+ row  `Base.Run.User`  `/run/user`  non
+ row  `Base.Srv`  `/srv`  non
+ row  `Base.Sys`  `/sys`  oui
+ row  `Base.Tmp`  `/tmp`  non
+ row  `Base.Usr`  `/usr`  oui
+ row  `Base.Usr.Bin`  `/usr/bin`  oui
+ row  `Base.Usr.Include`  `/usr/include`  oui
+ row  `Base.Usr.Lib`  `/usr/lib`  oui
+ row  `Base.Usr.Share`  `/usr/share`  oui
+ row  `Base.Usr.Share.Doc`  `/usr/share/doc`  oui
+ row  `Base.Usr.Share.Factory`  `/usr/share/factory`  oui
+ row  `Base.Usr.Share.Factory.Etc`  `/usr/share/factory/etc`  oui
+ row  `Base.Usr.Share.Factory.Var`  `/usr/share/factory/var`  oui
+ row  `Base.Var`  `/var`  non
+ row  `Base.Var.Cache`  `/var/cache`  non
+ row  `Base.Var.Lib`  `/var/lib`  non
+ row  `Base.Var.Log`  `/var/log`  non
+ row  `Base.Var.Spool`  `/var/spool`  non
+ row  `Base.Var.Tmp`  `/var/tmp`  non
```

- A layout is a fixed place. `Home.Config` is always `$HOME/.config`, and only `$HOME` is read
- from the environment. The locations which the
- [XDG specification](environment.md#standard-directories) lets the user move, such as
- `$XDG_CONFIG_HOME`, are given by `Xdg` instead. Applying a layout constructs a value of
- whichever directory type is asked for, so the same names serve a `Path on Linux` and any
- other representation.
+ Une disposition est un emplacement fixe. `Home.Config` vaut toujours `$HOME/.config`, et
+ seul `$HOME` est lu dans l'environnement. Les emplacements que la
+ [spécification XDG](environment.md#standard-directories) permet à l'utilisateur de
+ déplacer, comme `$XDG_CONFIG_HOME`, sont plutôt donnés par `Xdg`. Appliquer une
+ disposition construit une valeur du type de répertoire demandé, quel qu'il soit, si bien
+ que les mêmes noms servent pour un `Path on Linux` et pour toute autre représentation.

- ### Search paths
+ ### Chemins de recherche

- Many files are looked up not at one location, but along a list of them: an icon in whichever
- of the XDG data directories holds it, or a configuration file in the first of several places.
- A `Searchpaths.Stems` instance names the directories to try, in order. A relative path on the
- corresponding plane is resolved with `search` to the first of them which holds it, or to
- `Unset`:
+ De nombreux fichiers ne se recherchent pas à un seul emplacement, mais le long d'une liste
+ d'emplacements : une icône dans celui des répertoires de données XDG qui la contient, ou un
+ fichier de configuration au premier de plusieurs endroits. Une instance
+ `Searchpaths.Stems` nomme les répertoires à essayer, dans l'ordre. Un chemin relatif sur le
+ plan correspondant est résolu avec `search` vers le premier d'entre eux qui le contient, ou
+ vers `Unset` :

```scala fuel
typecheck
##
import galilei.search

given stems: (Searchpaths.Stems on Xdg.Data onto Linux) = new Searchpaths.Stems:
  type Plane = Xdg.Data
  type Target = Linux
  val stems: List[Path on Linux] = List(directory, directory / "inner")

val found = (% / "inner.txt").on[Xdg.Data].search()
```

- `dataSearch` and `configSearch` supply the XDG search paths for data and configuration, from
- the environment, so a program's resources are found wherever the user's system keeps them.
+ `dataSearch` et `configSearch` fournissent les chemins de recherche XDG pour les données et
+ la configuration, à partir de l'environnement, si bien que les ressources d'un programme
+ sont trouvées où que le système de l'utilisateur les conserve.

- ### Watching for changes
+ ### Surveiller les changements

- A directory is watched by opening it as `Watch`, which gives a [stream](streams.md) of
- `Watch.Event`s: a file created, modified or deleted, or a directory created. The registration
- lasts exactly as long as the block:
+ Un répertoire se surveille en l'ouvrant comme `Watch`, ce qui donne un [flux](streams.md)
+ de `Watch.Event`s : un fichier créé, modifié ou supprimé, ou un répertoire créé.
+ L'inscription dure exactement aussi longtemps que le bloc :

```scala
def observe(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.stream.each:
      case Watch.Event.NewFile(dir, file) => Out.println(t"created $file")
      case Watch.Event.Modify(dir, file)  => Out.println(t"modified $file")
      case Watch.Event.Delete(dir, file)  => Out.println(t"deleted $file")
      case _                              => ()
```

- Several paths are watched together by opening a list of them, which gives one stream of
- events across all of them.
+ Plusieurs chemins se surveillent ensemble en ouvrant une liste de chemins, ce qui donne un
+ seul flux d'événements pour l'ensemble.

- Editors save in bursts, and a rebuild wants one notification for each burst, not one for each
- event. `batches` gives a `Watch.Batch` each time no event has arrived for the given quiet
- period, and its `paths` lists every changed path once:
+ Les éditeurs enregistrent par rafales, et une reconstruction veut une notification par
+ rafale, et non une par événement. `batches` donne un `Watch.Batch` chaque fois qu'aucun
+ événement n'est arrivé pendant la période de calme donnée, et son `paths` liste chaque
+ chemin modifié une seule fois :

```scala
def rebuild(changed: List[Path on Linux]): Unit = ()

def observeBatches(): Unit =
  directory.open[Watch](): watcher ?=>
    watcher.batches(0.2*Second).each: batch =>
      rebuild(batch.paths[Path on Linux])
```

- The default watcher uses the operating system's own notifications of changes to files. Where
- those are unavailable, `watchers.polling` checks at an interval instead.
+ Le surveillant par défaut utilise les notifications de modification de fichiers propres au
+ système d'exploitation. Là où elles ne sont pas disponibles, `watchers.polling` vérifie
+ plutôt à intervalles réguliers.

- ### Choosing a backend
+ ### Choisir un backend

- Nothing above names a platform's API. The primitive operations which a filesystem must offer,
- which are stat, open, read, write, list, link and delete, are gathered into a
- `FilesystemBackend` for a plane, and everything else is defined in terms of them.
+ Rien de ce qui précède ne nomme l'API d'une plateforme. Les opérations primitives qu'un
+ système de fichiers doit offrir, à savoir stat, open, read, write, list, link et delete,
+ sont regroupées dans un `FilesystemBackend` pour un plan, et tout le reste est défini en
+ fonction d'elles.

- The `java.nio` implementation is `filesystemBackends.javaBaseFilesystem`. A WASI
- implementation, over `wasi:filesystem`, is supplied for WebAssembly components, so the same
- code reads and writes files on the JVM and inside a component. An operation which a backend
- cannot support raises an `Io.Error` whose reason is `Unsupported`. It is never approximated.
- Narrowing the platform's surface to a seam this small is
- [decoupling](../../philosophy/decoupling.md) applied within one library.
+ L'implémentation `java.nio` est `filesystemBackends.javaBaseFilesystem`. Une implémentation
+ WASI, au-dessus de `wasi:filesystem`, est fournie pour les composants WebAssembly, si bien
+ que le même code lit et écrit des fichiers sur la JVM et à l'intérieur d'un composant. Une
+ opération qu'un backend ne peut pas prendre en charge lève une `Io.Error` dont la raison
+ est `Unsupported`. Elle n'est jamais approximée. Réduire la surface de la plateforme à une
+ jonction aussi étroite, c'est le [découplage](../../philosophy/decoupling.md) appliqué au
+ sein d'une seule bibliothèque.

- ### Related
+ ### Voir aussi

- - [Paths](paths.md) describes the paths which these operations act on.
- - [Streams](streams.md) reads and writes files a part at a time.
- - [Archives](archives.md) opens a path as a ZIP or tar archive.
- - [Compression](compression.md#compressing-a-stream) writes a compressed file.
- - [Hashing](hashing.md#hashing-a-stream) computes the checksum of a file or a directory.
+ - [Chemins](paths.md) décrit les chemins sur lesquels agissent ces opérations.
+ - [Flux](streams.md) lit et écrit des fichiers morceau par morceau.
+ - [Archives](archives.md) ouvre un chemin comme une archive ZIP ou tar.
+ - [Compression](compression.md#compressing-a-stream) écrit un fichier compressé.
+ - [Hachage](hashing.md#hashing-a-stream) calcule la somme de contrôle d'un fichier ou d'un
+   répertoire.
