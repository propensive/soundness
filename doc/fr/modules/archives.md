- ## Archives
+ ## Archives

- ### About
+ ### Présentation

- The two archive formats of everyday computing,
- [ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format)) and
- [tar](https://en.wikipedia.org/wiki/Tar_(computing)), are read and written as typed values. A
- `Zipfile` or a `Tarfile` is a sequence of entries, each with a validated archive-relative path
- and streamed content. An archive of any size is read entry by entry, and written without being
- assembled in memory. A ZIP entry inflates as it is read, and a tar entry parses straight off the
- underlying source. Tar entries carry the Unix metadata which the format exists to preserve,
- which is modes, owners and modification times, as typed values.
+ Les deux formats d'archive de l'informatique de tous les jours,
+ [ZIP](https://en.wikipedia.org/wiki/ZIP_(file_format)) et
+ [tar](https://en.wikipedia.org/wiki/Tar_(computing)), se lisent et s'écrivent sous forme de
+ valeurs typées. Un `Zipfile` ou un `Tarfile` est une suite d'entrées, chacune dotée d'un chemin
+ validé relatif à l'archive et d'un contenu diffusé en flux. Une archive de n'importe quelle
+ taille se lit entrée par entrée, et s'écrit sans être assemblée en mémoire. Une entrée ZIP se
+ décompresse à mesure qu'elle est lue, et une entrée tar s'analyse directement depuis la source
+ sous-jacente. Les entrées tar portent les métadonnées Unix que le format a pour raison d'être de
+ préserver, à savoir les modes, les propriétaires et les dates de modification, sous forme de
+ valeurs typées.

- ### On archives
+ ### Sur les archives

- Archives are streams which pretend to be filesystems, and APIs for them tend to expose one
- pretense or the other badly: either a mutable random-access object which must be opened,
- mutated and remembered to close, or a raw stream of headers and bytes. Entry names are plain
- strings, so nothing prevents the classic mistakes, which are absolute paths and `..` traversal,
- and metadata is an afterthought of integers.
+ Les archives sont des flux qui se font passer pour des systèmes de fichiers, et leurs API ont
+ tendance à mal exposer l'une ou l'autre de ces facettes : soit un objet mutable à accès
+ aléatoire qu'il faut ouvrir, modifier et penser à fermer, soit un flux brut d'en-têtes et
+ d'octets. Les noms d'entrées sont de simples chaînes ; rien n'empêche donc les erreurs
+ classiques, à savoir les chemins absolus et la traversée par `..`, et les métadonnées ne sont
+ qu'une suite d'entiers ajoutée après coup.

- Here an archive is data. Entries are [immutable] values. Their paths are typed archive-relative
- [paths](paths.md). Reading, writing
- and compressing are the same streaming operations used everywhere else. Reading an archive
- lazily, through a scope which owns the file, is the shape described under
- [delimited scopes](../../philosophy/delimited-scopes.md).
+ Ici, une archive est une donnée. Les entrées sont des valeurs [immuables][immuable]. Leurs
+ chemins sont des [chemins](paths.md) typés relatifs à l'archive. La lecture, l'écriture et la
+ compression sont les mêmes opérations de flux qu'utilisées partout ailleurs. Lire une archive
+ paresseusement, par une portée qui possède le fichier, est la forme décrite sous
+ [portées délimitées](../../philosophy/delimited-scopes.md).

- Everything comes from the `soundness` package. Reading an archive from disk logs an event, and
- reading or writing one can fail, so an error [strategy] and a logging choice are in scope. The
- last import lets the samples print their results, and the samples work in a directory of their
- own:
+ Tout provient du package `soundness`. Lire une archive depuis le disque journalise un événement,
+ et lire ou écrire une archive peut échouer ; une [stratégie] d'erreur et un choix de
+ journalisation sont donc dans la portée. Le dernier import permet aux exemples d'afficher leurs
+ résultats, et les exemples travaillent dans un répertoire qui leur est propre :

```scala
import soundness.*
import filesystemOptions.overwritePreexisting

import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import logging.silentLogging
import pathInterfaces.pathOnLinux
import strategies.throwUnsafely
import temporaryDirectories.javaBaseTemporaryDirectory
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

val work = temporaryDirectory[Path on Local] / "archives" / Uuid().show
work.create[Directory](CreateFlag.Parents)
```

- Each format has its own archive and entry types, its own handle when opened, and its own error:
+ Chaque format a ses propres types d'archive et d'entrée, sa propre poignée une fois ouvert, et
+ sa propre erreur :

```tabular
- column  Format
- column  Archive
- column  Entry
- column  Open as
- column  Handle
- column  Open flags
- column  Error
- row  ZIP  `Zipfile`  `Zip.Entry`  `Zip`  `zipfile`  none  `Zip.Error`
- row  JAR  `Zipfile`  `Zip.Entry`  `Jar`  `zipfile`, with `manifest`  none  `Zip.Error`
- row  tar  `Tarfile`  `Tar.Entry`  `Tar`  `tar`  `Tar.Flag.Gzip`, `Zlib`, `Deflate`  `Tar.Error`
- row  ar  `Arfile`  `Ar.Entry`  `Ar`  `ar`  none  `Ar.Error`
+ column  Format
+ column  Archive
+ column  Entrée
+ column  Ouvert en
+ column  Poignée
+ column  Indicateurs d'ouverture
+ column  Erreur
+ row  ZIP  `Zipfile`  `Zip.Entry`  `Zip`  `zipfile`  aucun  `Zip.Error`
+ row  JAR  `Zipfile`  `Zip.Entry`  `Jar`  `zipfile`, avec `manifest`  aucun  `Zip.Error`
+ row  tar  `Tarfile`  `Tar.Entry`  `Tar`  `tar`  `Tar.Flag.Gzip`, `Zlib`, `Deflate`  `Tar.Error`
+ row  ar  `Arfile`  `Ar.Entry`  `Ar`  `ar`  aucun  `Ar.Error`
```

- ### ZIP
+ ### ZIP

- A ZIP archive is a `Zipfile` of entries. Each entry is an archive-relative path and a source of
- content. The archive streams as bytes, so writing one to disk is writing any other value.
- Compression is a policy in scope, which deflates by default, and stores where deflation would
- not help:
+ Une archive ZIP est un `Zipfile` d'entrées. Chaque entrée est un chemin relatif à l'archive et
+ une source de contenu. L'archive se diffuse en flux sous forme d'octets ; en écrire une sur
+ disque revient donc à écrire n'importe quelle autre valeur. La compression est une politique de
+ la portée, qui compresse par défaut avec deflate, et stocke tel quel là où la compression
+ n'aiderait pas :

```scala
val zipPath = work / "hello.zip"
val entry = Zip.Entry(t"hello.txt".as[Path on Zip], t"Hello world".in[Data])
zipPath.write(Zipfile(List(entry)))
```

- The path of an entry is a `Path on Zip`, which is relative to the root of the archive.
+ Le chemin d'une entrée est un `Path on Zip`, relatif à la racine de l'archive.

- An archive reads from a path or from bytes. An entry is looked up by its path, and its content
- reads as any type:
+ Une archive se lit depuis un chemin ou depuis des octets. Une entrée se recherche par son
+ chemin, et son contenu se lit dans n'importe quel type :

```scala fuel
run
##
val hello = Zipfile.read(zipPath)

hello.entries.each(entry => Out.println(entry.ref.encode))
Out.println(hello.entry(t"hello.txt".as[Path on Zip]).read[Text])
```

- An entry read from an archive carries the metadata of its header around its content as well.
- Writing it back reproduces the original archive byte for byte, except for the ZIP64 records,
- which are rebuilt:
+ Une entrée lue depuis une archive porte aussi, autour de son contenu, les métadonnées de son
+ en-tête. La réécrire reproduit l'archive d'origine octet pour octet, à l'exception des
+ enregistrements ZIP64, qui sont reconstruits :

```tabular
- column  Field
- column  Holds
- row  `flags`  the general-purpose flags
- row  `versionMadeBy`  the "version made by"
- row  `localVersion`  the "version needed" of the local header
- row  `centralVersion`  the "version needed" of the central directory
- row  `internalAttributes`  the internal attributes
- row  `externalAttributes`  the external attributes, where a Unix mode lives
- row  `localExtra`  the local header's extra field
- row  `centralExtra`  the central directory's extra field
+ column  Champ
+ column  Contient
+ row  `flags`  les indicateurs à usage général
+ row  `versionMadeBy`  la « version made by »
+ row  `localVersion`  la « version needed » de l'en-tête local
+ row  `centralVersion`  la « version needed » du répertoire central
+ row  `internalAttributes`  les attributs internes
+ row  `externalAttributes`  les attributs externes, où réside un mode Unix
+ row  `localExtra`  le champ supplémentaire de l'en-tête local
+ row  `centralExtra`  le champ supplémentaire du répertoire central
```

- Every one of those fields defaults to `Unset` on a new entry, which means the value the library
- would choose itself. Bit 3 of `flags` marks an entry as *streamed*. Its CRC and sizes are
- repeated in a data descriptor after the payload, which is how an archive is written to a sink
- which cannot seek.
+ Chacun de ces champs vaut par défaut `Unset` sur une nouvelle entrée, ce qui signifie la valeur
+ que la bibliothèque choisirait d'elle-même. Le bit 3 de `flags` marque une entrée comme *diffusée
+ en flux*. Son CRC et ses tailles sont répétés dans un descripteur de données après la charge
+ utile, ce qui permet d'écrire une archive dans un puits qui ne peut pas se repositionner.

- `Zipfile` also takes an optional `prefix` of raw bytes to place before the archive proper. The
- ZIP format locates its own directory from the end of the file, not the start, so an archive
- concatenated onto arbitrary leading bytes still reads correctly. That is how a self-extracting
- or directly-executable archive is made, and how the [packaged](packaging.md) launchers carry
- their own JAR. `Zipfile.rebase` corrects the one field of a very large (ZIP64) archive which a
- prefix does invalidate.
+ `Zipfile` prend aussi un `prefix` optionnel d'octets bruts à placer avant l'archive proprement
+ dite. Le format ZIP localise son propre répertoire à partir de la fin du fichier, et non du
+ début ; une archive concaténée à des octets de tête arbitraires se lit donc toujours
+ correctement. C'est ainsi qu'on fabrique une archive auto-extractible ou directement
+ exécutable, et que les lanceurs [empaquetés](packaging.md) transportent leur propre JAR.
+ `Zipfile.rebase` corrige l'unique champ d'une très grande archive (ZIP64) qu'un préfixe
+ invalide effectivement.

- An archive can also be *opened*, which makes its entries available for the duration of a scope,
- and closes the underlying source at the end of it, as with a
- [file](filesystem.md#reading-and-writing). Inside the scope, `zipfile` is the open handle:
+ Une archive peut aussi être *ouverte*, ce qui rend ses entrées disponibles pendant la durée
+ d'une portée, et ferme la source sous-jacente à la fin de celle-ci, comme pour un
+ [fichier](filesystem.md#reading-and-writing). Dans la portée, `zipfile` est la poignée ouverte :

```scala fuel
run
##
import dysasymptotics.linearSize

zipPath.open[Zip]():
  Out.println(zipfile.entries.size.show)
```

- A JAR is a ZIP with a manifest. Opening one as `Jar` gives the same handle, refined with the main
- attributes parsed from `META-INF/MANIFEST.MF`, with continuation lines rejoined as the
- specification requires. An archive without a manifest simply has no attributes:
+ Un JAR est un ZIP doté d'un manifeste. En ouvrir un en tant que `Jar` donne la même poignée,
+ raffinée avec les attributs principaux analysés depuis `META-INF/MANIFEST.MF`, les lignes de
+ continuation étant rejointes comme l'exige la spécification. Une archive sans manifeste n'a
+ simplement pas d'attributs :

```scala fuel
run
##
val manifest = Jar.manifest(t"Manifest-Version" -> t"1.0")
val jarPath = work / "hello.jar"
jarPath.write(Zipfile(List(manifest, entry)))

jarPath.open[Jar]():
  Out.println(zipfile.manifest.inspect)
```

- Errors are `Zip.Error`s whose reason says what was wrong: `NotFound` for an entry which is not
- there, `DuplicateEntry` when two entries share a path, and `MissingEocd` when the bytes are not a
- ZIP archive at all:
+ Les erreurs sont des `Zip.Error` dont la raison indique ce qui n'allait pas : `NotFound` pour
+ une entrée absente, `DuplicateEntry` quand deux entrées partagent un chemin, et `MissingEocd`
+ quand les octets ne sont pas du tout une archive ZIP :

```scala fuel
run
errors show
##
hello.entry(t"missing.txt".as[Path on Zip])
```

- ### Tar
+ ### Tar

- A tar entry is one of the typed cases of the format: a file, a directory, a hard or symbolic
- link, a character or block device, or a FIFO, with its Unix metadata spelled out. `Tar.Entry`
- builds the common case, a file, with a default mode and owner, stamped with the current time.
- `Tar.Entry.at` takes the timestamp as any instant type. The case classes take everything
- explicitly:
+ Une entrée tar est l'un des cas typés du format : un fichier, un répertoire, un lien physique
+ ou symbolique, un périphérique caractère ou bloc, ou une FIFO, avec ses métadonnées Unix
+ explicitées. `Tar.Entry` construit le cas courant, un fichier, avec un mode et un propriétaire
+ par défaut, horodaté à l'heure actuelle. `Tar.Entry.at` prend l'horodatage sous n'importe quel
+ type d'instant. Les case classes prennent tout explicitement :

```scala
val readme = Tar.Entry(t"README".as[Relative on Tar], t"Read me first".in[Data])

val script = Tar.Entry.File
  ( path  = t"bin/run".as[Relative on Tar],
    mode  = UnixMode(ownerExec = true, groupExec = true, otherExec = true),
    user  = UnixUser(1000, t"hamlet"),
    group = UnixGroup(1000, t"hamlet"),
    mtime = 0.bits.u32,
    data  = Archive.Body(t"#!/bin/sh\necho hello\n".in[Data]) )
```

- A `Tarfile` of entries streams as tar blocks, or as the compressed forms in which the format
- usually travels:
+ Un `Tarfile` d'entrées se diffuse en flux sous forme de blocs tar, ou sous les formes
+ compressées dans lesquelles le format voyage habituellement :

```scala
val tarball = Tarfile(List(readme, script))
val gzipped = tarball.gzip
val zlibbed = tarball.zlib
val deflated = tarball.deflate
```

- Reading runs the other way. `Tarfile.read` parses entries lazily, straight off a stream of
- bytes. Consuming one entry advances the source past it, so an archive is never materialized. A
- compressed archive is simply a decompressed stream, read the same way:
+ La lecture fonctionne en sens inverse. `Tarfile.read` analyse les entrées paresseusement,
+ directement depuis un flux d'octets. Consommer une entrée fait avancer la source au-delà ; une
+ archive n'est donc jamais matérialisée. Une archive compressée est simplement un flux
+ décompressé, lu de la même façon :

```scala fuel
run
##
Tarfile.read(tarball.source[Data]).each(entry => Out.println(entry.entryName))
Tarfile.read(tarball.gzip.decompress[Gzip]).each(entry => Out.println(entry.entryName))
```

- Reading an archive this way is single-pass and single-owner. Consume the entries in order, on
- one thread. An entry which is passed over remains readable, because its body is memoized when
- the sequence advances, but the sequence itself cannot be replayed.
+ Lire une archive de cette façon se fait en une seule passe et avec un seul propriétaire.
+ Consommez les entrées dans l'ordre, sur un seul thread. Une entrée sautée reste lisible, car son
+ corps est mémoïsé quand la séquence avance, mais la séquence elle-même ne peut être rejouée.

- Opening the archive as `Tar` scopes the underlying source, as a ZIP archive is scoped, and takes
- the compression as a flag. `tar` is the open handle:
+ Ouvrir l'archive en tant que `Tar` délimite la portée de la source sous-jacente, comme pour une
+ archive ZIP, et prend la compression comme indicateur. `tar` est la poignée ouverte :

```scala fuel
run
##
val tarPath = work / "hello.tar.gz"
tarPath.create[File](): handle ?=>
  handle.write(tarball.gzip.memoize)

tarPath.open[Tar](Tar.Flag.Gzip):
  tar.entries.each(entry => Out.println(entry.entryName))
```

- A tarball unpacks onto the [filesystem](filesystem.md) with `extractTo`:
+ Une archive tar se déballe sur le [système de fichiers](filesystem.md) avec `extractTo` :

```scala fuel
run
##
val unpacked: Path on Local = work / "unpacked"
tarball.extractTo(unpacked)

Out.println((unpacked / "README").open[File]()(file.read[Text]))
```

- Long names are handled in POSIX's pax form by default, or in GNU's, chosen when the archive is
- built, as `Tarfile(entries, LongNameFormat.Gnu)`. Sparse files and pax extended headers
- round-trip faithfully. A malformed archive raises a `Tar.Error` which names the fault: a bad
- checksum, a header field which cannot be parsed, or a truncated body.
+ Les noms longs sont gérés sous la forme pax de POSIX par défaut, ou sous celle de GNU, choisie à
+ la construction de l'archive, comme `Tarfile(entries, LongNameFormat.Gnu)`. Les fichiers
+ creux et les en-têtes étendus pax font l'aller-retour fidèlement. Une archive mal formée lève
+ une `Tar.Error` qui nomme le défaut : une somme de contrôle incorrecte, un champ d'en-tête
+ inanalysable, ou un corps tronqué.

- ### Archiving a directory
+ ### Archiver un répertoire

- A directory tree archives in any of the formats with `archive`, with the format chosen as a
- type:
+ Une arborescence de répertoires s'archive dans n'importe lequel des formats avec `archive`, le
+ format étant choisi sous forme de type :

```scala
import filesystemOptions.preserveSymlinks
```

```scala fuel
run
##
unpacked.archive[Tar]().entries.each(entry => Out.println(entry.entryName))
unpacked.archive[Zip]().entries.each(entry => Out.println(entry.ref.encode))
unpacked.archive[Ar]().entries.each(entry => Out.println(entry.name))
```

- The result is the in-memory archive, a `Tarfile`, `Zipfile` or `Arfile`, to stream, compress or
- write wherever it is wanted. Every descendant is visited in a fixed order, with the children of
- each directory sorted, so the same tree always produces the same bytes. Tar records each entry
- as its own kind, with the mode, owner and timestamp which the filesystem reports. The formats
- differ in what they can store:
+ Le résultat est l'archive en mémoire, un `Tarfile`, un `Zipfile` ou un `Arfile`, à diffuser,
+ compresser ou écrire là où on le souhaite. Chaque descendant est visité dans un ordre fixe, les
+ enfants de chaque répertoire étant triés ; la même arborescence produit donc toujours les mêmes
+ octets. Tar enregistre chaque entrée selon sa propre nature, avec le mode, le propriétaire et
+ l'horodatage que signale le système de fichiers. Les formats diffèrent par ce qu'ils peuvent
+ stocker :

```tabular
- column  Format
- column  Result
- column  Symbolic links
- column  Directories
- column  Flags
- row  `Tar`  `Tarfile`  stored or followed, by the `filesystemOptions` symlink policy  stored as entries  `Archive.Flag`, `LongNameFormat`
- row  `Zip`  `Zipfile`  followed  stored as entries  `Zip.Compression`
- row  `Ar`  `Arfile`  followed  flattened, each file named by its slash-joined relative path  `Archive.Flag`
+ column  Format
+ column  Résultat
+ column  Liens symboliques
+ column  Répertoires
+ column  Indicateurs
+ row  `Tar`  `Tarfile`  stockés ou suivis, selon la politique de liens symboliques de `filesystemOptions`  stockés comme entrées  `Archive.Flag`, `LongNameFormat`
+ row  `Zip`  `Zipfile`  suivis  stockés comme entrées  `Zip.Compression`
+ row  `Ar`  `Arfile`  suivis  aplatis, chaque fichier étant nommé par son chemin relatif joint par des barres obliques  `Archive.Flag`
```

- Flags tune the archiving. `Archive.Flag.Owner` and `Archive.Flag.Mtime` replace the owner and
- timestamp of every entry. They are `tar --owner`, `--group` and `--mtime`, the knobs of a
- reproducible build. A `LongNameFormat` chooses tar's convention for long names. A ZIP takes a
- `Zip.Compression` instead:
+ Des indicateurs ajustent l'archivage. `Archive.Flag.Owner` et `Archive.Flag.Mtime` remplacent le
+ propriétaire et l'horodatage de chaque entrée. Ce sont les `tar --owner`, `--group` et
+ `--mtime`, les réglages d'un build reproductible. Un `LongNameFormat` choisit la convention de
+ tar pour les noms longs. Un ZIP prend plutôt une `Zip.Compression` :

```scala
val reproducible = unpacked.archive[Tar](LongNameFormat.Gnu, Archive.Flag.Owner(UnixUser(0), UnixGroup(0)))
val stored = unpacked.archive[Zip](Zip.Compression.Stored)
```

- ### Ar
+ ### Ar

- `ar` is the container of every `*.deb` package and every `*.a` static library: an eight-byte
- magic, then a flat run of members, each a sixty-byte header followed inline by its payload. It
- carries no entry types, no checksum and no end-of-archive marker, so a member is just a name,
- its Unix metadata and a body. `Ar.Entry` builds one. The `mode` is the raw octal field, such as
- `100644`, since `ar` stores the file-type bits beside the permissions:
+ `ar` est le conteneur de chaque package `*.deb` et de chaque bibliothèque statique `*.a` : un
+ nombre magique de huit octets, puis une suite plate de membres, chacun étant un en-tête de
+ soixante octets suivi directement de sa charge utile. Il ne porte ni types d'entrées, ni somme
+ de contrôle, ni marqueur de fin d'archive ; un membre n'est donc qu'un nom, ses métadonnées Unix
+ et un corps. `Ar.Entry` en construit un. Le `mode` est le champ octal brut, comme `100644`,
+ puisque `ar` stocke les bits de type de fichier à côté des permissions :

```scala
val control =
  Ar.Entry(t"control.tar.xz", Archive.Body(t"...".in[Data]), 0L, UnixUser(0), UnixGroup(0), 33188)
val deb = Arfile(List(control))
```

- Reading mirrors `Tarfile.read`. Entries parse lazily off a stream of bytes, single-pass and in
- order, with the body of each member streamed in bounded chunks, and memoized if the sequence
- advances past it. Opening as `Ar` scopes the source as `Tar` does, with `ar` as the handle.
- There are no flags, because a `*.deb` is plain `ar`, and only its members are compressed:
+ La lecture est le miroir de `Tarfile.read`. Les entrées s'analysent paresseusement depuis un
+ flux d'octets, en une seule passe et dans l'ordre, le corps de chaque membre étant diffusé en
+ morceaux bornés, et mémoïsé si la séquence le dépasse. Ouvrir en tant que `Ar` délimite la
+ portée de la source comme le fait `Tar`, avec `ar` comme poignée. Il n'y a pas d'indicateurs, car
+ un `*.deb` est du `ar` simple, et seuls ses membres sont compressés :

```scala fuel
run
##
Arfile.read(deb.source[Data]).each(entry => Out.println(entry.name))

val debPath = work / "hello.deb"
debPath.create[File](): handle ?=>
  handle.write(deb)

debPath.open[Ar]():
  ar.entries.each(entry => Out.println(entry.name))
```

- A GNU archive stores names longer than fifteen characters in a `//` name-table member, and
- refers to them by offset. The reader resolves those, and strips the SysV terminating `/`, into
- `name`. `rawName` keeps the literal field, so a parsed archive serializes again byte for byte,
- as `Arfile.from(stream).source[Data]`. The `role` of each entry says whether it is a `Regular`
- member, the `NameTable`, or a `SymbolTable`, which is `/` or `/SYM64/`. A malformed archive
- raises an `Ar.Error`: a missing magic, a corrupt header terminator, a non-numeric field, a
- truncated stream, or a long-name reference which does not resolve.
+ Une archive GNU stocke les noms de plus de quinze caractères dans un membre table des noms `//`,
+ et y fait référence par décalage. Le lecteur les résout, et retire le `/` terminal de SysV, dans
+ `name`. `rawName` conserve le champ littéral ; une archive analysée se sérialise donc de nouveau
+ octet pour octet, comme `Arfile.from(stream).source[Data]`. Le `role` de chaque entrée indique
+ s'il s'agit d'un membre `Regular`, de la `NameTable`, ou d'une `SymbolTable`, qui est `/` ou
+ `/SYM64/`. Une archive mal formée lève une `Ar.Error` : un nombre magique absent, un terminateur
+ d'en-tête corrompu, un champ non numérique, un flux tronqué, ou une référence de nom long qui ne
+ se résout pas.

- ### Related
+ ### Voir aussi

- - [Compression](compression.md) describes the gzip, zlib and deflate which a tarball travels
-   in.
- - [Filesystem](filesystem.md) describes the files, directories and handles which archives are
-   read from, written to and extracted onto.
- - [Paths](paths.md) describes the archive-relative paths of entries.
- - [Packaging](packaging.md) builds the JAR files and launchers which are ZIP archives.
- - [Containers](docker.md) builds image layers from tar archives.
- - [Streams](streams.md) describes the lazy streams which entries are read from.
- - [Versioning](versioning.md) describes the manifests which a JAR carries.
+ - [Compression](compression.md) décrit le gzip, le zlib et le deflate dans lesquels voyage une
+   archive tar.
+ - [Système de fichiers](filesystem.md) décrit les fichiers, répertoires et poignées depuis
+   lesquels les archives sont lues, dans lesquels elles sont écrites et sur lesquels elles sont
+   extraites.
+ - [Chemins](paths.md) décrit les chemins des entrées relatifs à l'archive.
+ - [Empaquetage](packaging.md) construit les fichiers JAR et les lanceurs qui sont des archives
+   ZIP.
+ - [Conteneurs](docker.md) construit des couches d'image à partir d'archives tar.
+ - [Flux](streams.md) décrit les flux paresseux depuis lesquels les entrées sont lues.
+ - [Versionnage](versioning.md) décrit les manifestes que porte un JAR.
