- ## Compression
+ ## Compression

- ### About
+ ### Présentation

- A handful of formats carry most of the world's compressed bytes:
- [DEFLATE](https://en.wikipedia.org/wiki/Deflate) and its wrappers `gzip` and `zlib`,
- [Brotli](https://en.wikipedia.org/wiki/Brotli), [XZ](https://en.wikipedia.org/wiki/XZ_Utils)
- and raw LZMA2, and [LZW](https://en.wikipedia.org/wiki/Lempel–Ziv–Welch). All of them compress
- and decompress data through one pair of methods. The format is a type, and `compress` and
- `decompress` serve every format, over a whole value or as a stage of a
- [stream](streams.md).
+ Une poignée de formats transportent la plupart des octets compressés du monde :
+ [DEFLATE](https://en.wikipedia.org/wiki/Deflate) et ses enveloppes `gzip` et `zlib`,
+ [Brotli](https://en.wikipedia.org/wiki/Brotli), [XZ](https://en.wikipedia.org/wiki/XZ_Utils)
+ et LZMA2 brut, et [LZW](https://en.wikipedia.org/wiki/Lempel–Ziv–Welch). Tous compressent et
+ décompressent les données au moyen d'une seule paire de méthodes. Le format est un type, et
+ `compress` et `decompress` servent tous les formats, sur une valeur entière ou comme étape d'un
+ [flux](streams.md).

- Every format is implemented in pure Scala, so it works on every platform. On the JVM, DEFLATE
- and its wrappers are also served by the platform's native zlib. That choice is made when the
- code is built, not at each use, so the JVM pays nothing for the portability.
+ Chaque format est implémenté en Scala pur : il fonctionne donc sur toutes les plateformes. Sur
+ la JVM, DEFLATE et ses enveloppes sont aussi servis par la zlib native de la plateforme. Ce
+ choix est fait lors du build du code, et non à chaque utilisation : la JVM ne paie donc rien
+ pour la portabilité.

- ### On compression
+ ### La compression

- A compression library usually presents a codec which must be constructed, fed and finished: an
- object with a buffer, a `setInput`, a `deflate` and a `finish`, and a loop around them which
- is easy to get wrong. The format is a string or an integer, the framing is a flag, and whether
- a given call has produced everything it will produce is a question the caller has to answer.
+ Une bibliothèque de compression présente d'ordinaire un codec qu'il faut construire, alimenter
+ et terminer : un objet avec un tampon, un `setInput`, un `deflate` et un `finish`, et une
+ boucle autour d'eux qu'il est facile d'écrire de travers. Le format est une chaîne ou un
+ entier, le tramage est un indicateur, et savoir si un appel donné a produit tout ce qu'il
+ produira est une question à laquelle l'appelant doit répondre.

- Here the format is a type, such as `Gzip`, `Brotli` or `Xz`, and the operation is a method on
- the data. The same instance describes both the whole-value form and the streaming form, so the
- two agree byte for byte, and a value compressed in one way decompresses in the other. A
- compression scheme is a stage of a pipeline, not a wrapper around a stream, which is
- [composability](../../philosophy/composability.md) in its most literal form.
+ Ici, le format est un type, comme `Gzip`, `Brotli` ou `Xz`, et l'opération est une méthode sur
+ les données. La même instance décrit la forme sur valeur entière et la forme en flux : les deux
+ concordent donc octet pour octet, et une valeur compressée d'une façon se décompresse de
+ l'autre. Un schéma de compression est une étape d'un pipeline, et non une enveloppe autour
+ d'un flux, ce qui est la [composabilité](../../philosophy/composability.md) sous sa forme la
+ plus littérale.

- Everything comes from the `soundness` package. The last import lets the samples print their
- results:
+ Tout provient du package `soundness`. Le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*

import charsets.utf8Charset
import textSanitizers.skipSanitizer
import codepages.utf8Codepage
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Compressing a value
+ ### Compresser une valeur

- A block of bytes is compressed, and decompressed, by naming the format:
+ Un bloc d'octets se compresse, et se décompresse, en nommant le format :

```scala
val text = t"the quick brown fox jumps over the lazy dog. "
val payload = (text + text + text + text).in[Data]

val compressed = payload.compress[Gzip]
```

```scala fuel
run
##
Out.println(payload.length.show)
Out.println(compressed.length.show)
Out.println(compressed.decompress[Gzip].utf8.keep(43))
```

- ### Compressing a stream
+ ### Compresser un flux

- The same names attach to a stream, where they become stages of a pipeline. The data is
- transformed as it flows, and nothing larger than the pipeline's buffers is held.
+ Les mêmes noms s'attachent à un flux, où ils deviennent des étapes d'un pipeline. Les données
+ sont transformées au fil de leur passage, et rien de plus grand que les tampons du pipeline
+ n'est conservé.

- Writing a compressed [file](filesystem.md), and reading it back, is a matter of placing the
- compressor and the decompressor on either side of the disk. `pump` delivers the compressed
- stream into the file's intake:
+ Écrire un [fichier](filesystem.md) compressé, et le relire, revient à placer le compresseur et
+ le décompresseur de part et d'autre du disque. `pump` délivre le flux compressé dans l'entrée
+ du fichier :

```scala fuel
typecheck
##
import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val archive = temporaryDirectory[Path on Local] / "compression" / Uuid().show / "fox.txt.gz"

archive.create[File](CreateFlag.Parents): handle ?=>
  payload.stream.compress[Gzip].pump(handle.intake())

val restored: Text = archive.open[File]()(file.stream.decompress[Gzip].read[Text])
```

- The whole-value and streaming forms interoperate freely. A value which was compressed as a
- whole decompresses through a stream, and the output of a stream decompresses as a value.
+ Les formes sur valeur entière et en flux interopèrent librement. Une valeur compressée d'un
+ bloc se décompresse par un flux, et la sortie d'un flux se décompresse comme une valeur.

- ### The formats
+ ### Les formats

- Seven formats are provided, in four families:
+ Sept formats sont fournis, en quatre familles :

```tabular
- column  Format
- column  Algorithm
- column  Framing
- column  Carried by
- row  `Deflate`  DEFLATE  none  ZIP entries
- row  `Zlib`  DEFLATE  a two-byte header and a checksum  PNG chunks, HTTP `deflate` encoding
- row  `Gzip`  DEFLATE  a header, timestamp and CRC  `*.gz` files, HTTP content encoding
- row  `Brotli`  Brotli  the Brotli stream format  HTTP content encoding
- row  `Xz`  LZMA2  the `*.xz` container, with a CRC-64 check  `*.xz` files
- row  `Lzma2`  LZMA2  none
- row  `Lzw`  LZW  none  TIFF and PDF streams
+ column  Format
+ column  Algorithme
+ column  Tramage
+ column  Utilisé par
+ row  `Deflate`  DEFLATE  aucun  entrées ZIP
+ row  `Zlib`  DEFLATE  un en-tête de deux octets et une somme de contrôle  morceaux PNG, encodage HTTP `deflate`
+ row  `Gzip`  DEFLATE  un en-tête, un horodatage et un CRC  fichiers `*.gz`, encodage de contenu HTTP
+ row  `Brotli`  Brotli  le format de flux Brotli  encodage de contenu HTTP
+ row  `Xz`  LZMA2  le conteneur `*.xz`, avec une vérification CRC-64  fichiers `*.xz`
+ row  `Lzma2`  LZMA2  aucun
+ row  `Lzw`  LZW  aucun  flux TIFF et PDF
```

- #### DEFLATE
+ #### DEFLATE

- `Deflate` is the raw algorithm. `Zlib` adds a two-byte header and a checksum. `Gzip` adds the
- header, timestamp and CRC which `*.gz` files carry. These are the formats of HTTP content
- encoding, of ZIP entries and of PNG chunks, and they are the fastest of the set.
+ `Deflate` est l'algorithme brut. `Zlib` ajoute un en-tête de deux octets et une somme de
+ contrôle. `Gzip` ajoute l'en-tête, l'horodatage et le CRC que portent les fichiers `*.gz`. Ce
+ sont les formats de l'encodage de contenu HTTP, des entrées ZIP et des morceaux PNG, et ce sont
+ les plus rapides de l'ensemble.

- #### Brotli
+ #### Brotli

- `Brotli` compresses to a smaller size than DEFLATE, at a comparable speed, and is the content
- encoding of the modern web. Both directions need the whole value before they produce any
- output. The decoder needs it because backward references may reach across the entire window,
- and the encoder because it chooses its framing from the total length. A Brotli stage therefore
- buffers, where a DEFLATE stage does not.
+ `Brotli` compresse à une taille plus petite que DEFLATE, à une vitesse comparable, et c'est
+ l'encodage de contenu du web moderne. Les deux sens ont besoin de la valeur entière avant de
+ produire la moindre sortie. Le décodeur en a besoin parce que les références arrière peuvent
+ s'étendre sur toute la fenêtre, et l'encodeur parce qu'il choisit son tramage d'après la
+ longueur totale. Une étape Brotli met donc en tampon, là où une étape DEFLATE ne le fait pas.

- Brotli can also encode a value *relative to* another which the receiver already holds. A
- `continuation` is the tail of a Brotli stream whose window has been loaded with a base
- beforehand. It carries only the new value's blocks, and no stream header, and its backward
- references reach into the base. A successor which differs from its predecessor by a few edits
- therefore costs a few bytes:
+ Brotli peut aussi encoder une valeur *relativement à* une autre que le destinataire détient
+ déjà. Une `continuation` est la fin d'un flux Brotli dont la fenêtre a été chargée au préalable
+ avec une base. Elle ne porte que les blocs de la nouvelle valeur, sans en-tête de flux, et ses
+ références arrière atteignent la base. Un successeur qui ne diffère de son prédécesseur que de
+ quelques modifications ne coûte donc que quelques octets :

```scala
val base: Data = t"the quick brown fox jumps over the lazy dog".in[Data]
val next: Data = t"the quick brown fox jumped over the lazy dog".in[Data]
val continuation = Brotli.continuation(base, next)
```

- The receiver rebuilds the window from its own copy of the base, with `prefix`, and decodes the
- prefix and the continuation as one stream. What comes out is the base, followed by the
- successor:
+ Le destinataire reconstruit la fenêtre à partir de sa propre copie de la base, avec `prefix`,
+ et décode le préfixe et la continuation comme un seul flux. Ce qui en sort est la base, suivie
+ du successeur :

```scala
val decoded = Chain(Brotli.prefix(base), continuation).decompress[Brotli].read[Data]
```

```scala fuel
run
##
Out.println(next.length.show)
Out.println(continuation.length.show)
Out.println((decoded.length == base.length + next.length).inspect)
```

- The bytes of a `prefix` are fixed by RFC 7932, together with the window and block parameters.
- Both methods take the window size as WBITS, from 10 to 24, and `prefix` takes the maximum
- length of an uncompressed block. Their defaults are `Brotli.Window` and `Brotli.Block`, the
- largest which the format allows. Both sides must use the same values.
+ Les octets d'un `prefix` sont fixés par la RFC 7932, avec les paramètres de fenêtre et de
+ bloc. Les deux méthodes prennent la taille de fenêtre en WBITS, de 10 à 24, et `prefix` prend
+ la longueur maximale d'un bloc non compressé. Leurs valeurs par défaut sont `Brotli.Window` et
+ `Brotli.Block`, les plus grandes que le format autorise. Les deux côtés doivent utiliser les
+ mêmes valeurs.

- #### XZ and LZMA2
+ #### XZ et LZMA2

- `Xz` is the codec with the highest ratio. It is LZMA2 inside the `*.xz` container, with a
- CRC-64 check, and matches what the `xz` command-line tool produces. `Lzma2` is the same codec
- without the container's framing, and stands to `Xz` as `Deflate` stands to `Gzip`.
+ `Xz` est le codec au meilleur taux. C'est du LZMA2 dans le conteneur `*.xz`, avec une
+ vérification CRC-64, et il correspond à ce que produit l'outil en ligne de commande `xz`.
+ `Lzma2` est le même codec sans le tramage du conteneur, et il est à `Xz` ce que `Deflate` est
+ à `Gzip`.

- Both default to preset 6. Presets 0 to 3 favor speed, and 4 to 9 favor ratio. An explicit
- preset is selected with `Xz.compress(stream, preset)` or `Xz.compressor(preset)`.
+ Les deux utilisent par défaut le préréglage 6. Les préréglages 0 à 3 privilégient la vitesse,
+ et 4 à 9 le taux. Un préréglage explicite se choisit avec `Xz.compress(stream, preset)` ou
+ `Xz.compressor(preset)`.

- #### LZW
+ #### LZW

- `Lzw` is the compression of TIFF and PDF streams. The JDK offers no implementation of it at
- all, so this one is written from the specification. Its `earlyChange` parameter makes both
- sides widen their codes one table entry sooner. It is on by default, which is what TIFF, PDF
- and every known encoder produce. The parameterized `Lzw.compressor` and `Lzw.decompressor`
- serve formats which state it explicitly.
+ `Lzw` est la compression des flux TIFF et PDF. Le JDK n'en propose aucune implémentation :
+ celle-ci est donc écrite à partir de la spécification. Son paramètre `earlyChange` fait élargir
+ leurs codes aux deux côtés une entrée de table plus tôt. Il est activé par défaut, ce que
+ produisent TIFF, PDF et tous les encodeurs connus. Les versions paramétrées `Lzw.compressor`
+ et `Lzw.decompressor` servent les formats qui l'indiquent explicitement.

- ### Where compression appears
+ ### Où apparaît la compression

- Compression is rarely used alone:
+ La compression est rarement utilisée seule :

- - [Archives](archives.md) store their entries compressed, and tar archives travel compressed.
- - [HTTP](http-client.md) bodies are encoded with it on the wire.
- - [Container](docker.md) image layers are compressed, and addressed by their
-   [digest](hashing.md#hashing-a-stream).
- - [PDF](pdf.md) content streams are stored compressed.
+ - Les [archives](archives.md) stockent leurs entrées compressées, et les archives tar voyagent
+   compressées.
+ - Les corps [HTTP](http-client.md) sont encodés avec elle en transmission.
+ - Les couches des images de [conteneur](docker.md) sont compressées, et désignées par leur
+   [condensé](hashing.md#hashing-a-stream).
+ - Les flux de contenu [PDF](pdf.md) sont stockés compressés.

- In each case the format is one of the types named here, so a layer, an entry or a body
- decompresses with the operation described above.
+ Dans chaque cas, le format est l'un des types nommés ici : une couche, une entrée ou un corps
+ se décompresse donc avec l'opération décrite plus haut.
