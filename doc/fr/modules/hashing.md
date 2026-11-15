- ## Hashing
+ ## Hachage

- ### About
+ ### Présentation

- A [hash](https://en.wikipedia.org/wiki/Hash_function) reduces data of any size to a short
- fingerprint of fixed length. The digest of any value can be computed, whether a case class, a
- collection or a stream of bytes, with the algorithm named as a type: `Sha2[256]`, `Sha1`, `Md5`
- or `Blake3`.
+ Un [hachage](https://en.wikipedia.org/wiki/Hash_function) réduit des données de taille
+ quelconque à une courte empreinte de longueur fixe. On peut calculer le condensé de n'importe
+ quelle valeur, qu'il s'agisse d'une case class, d'une collection ou d'un flux d'octets, avec
+ l'algorithme désigné par un type : `Sha2[256]`, `Sha1`, `Md5` ou `Blake3`.

- Anything which describes how to feed its bytes into a hash can be digested. A case class does
- so automatically, [derived][derivation] from its fields, so hashing a structured value needs no
- manual serialization. A digest is a value, and its bytes are written in hexadecimal or Base64
- through the same [base encoding](base-encoding.md) used everywhere else.
+ Tout ce qui décrit comment fournir ses octets à un hachage peut être haché. Une case class le
+ fait automatiquement, par [dérivation] à partir de ses champs : hacher une valeur structurée ne
+ nécessite donc aucune sérialisation manuelle. Un condensé est une valeur, et ses octets
+ s'écrivent en hexadécimal ou en Base64 au moyen du même [encodage en base](base-encoding.md)
+ utilisé partout ailleurs.

- The checksums `Crc32`, `Crc64` and `Adler32` are computed through exactly the same API. The
- algorithms which are known to be weak, MD5 and SHA-1, can be used only where their use is
- explicitly permitted. So can the checksums, which are not weak hashes but not hashes at all: a
- checksum detects accidental corruption, and nothing more.
+ Les sommes de contrôle `Crc32`, `Crc64` et `Adler32` se calculent avec exactement la même API.
+ Les algorithmes réputés faibles, MD5 et SHA-1, ne peuvent être utilisés que là où leur usage est
+ explicitement permis. Il en va de même des sommes de contrôle, qui ne sont pas des hachages
+ faibles mais pas des hachages du tout : une somme de contrôle détecte une corruption
+ accidentelle, et rien de plus.

- ### On hashing
+ ### Le hachage

- The usual hashing API takes a byte array, and an algorithm named by a string, and hands back
- another byte array:
+ L'API de hachage habituelle prend un tableau d'octets et un algorithme désigné par une chaîne,
+ et renvoie un autre tableau d'octets :

- - The algorithm is unchecked, so a typo is a failure at [runtime].
- - The input must be serialized to bytes by hand before it is hashed.
- - The output must be written as text by hand afterwards.
- - Nothing discourages reaching for a broken algorithm.
+ - L'algorithme n'est pas vérifié : une faute de frappe est donc un échec
+   [à l'exécution][exécution].
+ - L'entrée doit être sérialisée en octets à la main avant d'être hachée.
+ - La sortie doit ensuite être écrite sous forme de texte à la main.
+ - Rien ne dissuade de recourir à un algorithme cassé.

- Here each of these is fixed. The algorithm is a type, checked where it is used, so a digest of
- one algorithm cannot be confused with another's, which is
- [safety by construction](../../philosophy/safety-by-construction.md). Any value with a
- `Digestible` instance is hashed directly. The result is written through the base encodings. And
- a weak algorithm sits behind a permission which a program must opt into.
+ Ici, chacun de ces points est corrigé. L'algorithme est un type, vérifié là où il est utilisé :
+ le condensé d'un algorithme ne peut donc pas être confondu avec celui d'un autre, ce qui est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md). Toute valeur ayant une
+ instance de `Digestible` est hachée directement. Le résultat s'écrit au moyen des encodages en
+ base. Et un algorithme faible se trouve derrière une permission à laquelle un programme doit
+ explicitement souscrire.

- Everything comes from the `soundness` package. A provider of hash algorithms and an alphabet
- are in scope, and the last import lets the samples print their results:
+ Tout vient du package `soundness`. Un fournisseur d'algorithmes de hachage et un alphabet sont
+ dans la portée, et le dernier import permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*

import alphabets.hexLowerCase
import codepages.utf8Codepage
import providers.javaBaseProvider
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Hashing a value
+ ### Hacher une valeur

- `digest` computes the digest of a value, in a named algorithm, and `serialize` writes it out:
+ `digest` calcule le condensé d'une valeur, dans un algorithme désigné, et `serialize` l'écrit :

```scala fuel
run
##
Out.println(t"Hello world".digest[Sha2[256]].serialize[Hex])
```

- The provider in scope supplies the algorithms. These are the algorithms, the permit each
- needs, and the providers which offer each on the JVM:
+ Le fournisseur dans la portée fournit les algorithmes. Voici les algorithmes, le permis dont
+ chacun a besoin, et les fournisseurs qui les proposent sur la JVM :

```tabular
- column  Algorithm
- column  Kind
- column  Permit
- column  Providers
- column  Bits
- row  `Sha2[224]`, `Sha2[256]`, `Sha2[384]`, `Sha2[512]`  hash  none  `javaBaseProvider`, `soundnessProvider`  224–512
- row  `Blake3`  hash  none  `soundnessProvider`  256
- row  `Sha1`  weak hash  `permitDeprecatedCrypto`  `javaBaseProvider`, `soundnessProvider`  160
- row  `Md5`  weak hash  `permitDisallowedCrypto`  `javaBaseProvider`, `soundnessProvider`  128
- row  `Murmur3`  fast hash  `permitNonCryptographicHashes`  `soundnessProvider`  128
- row  `Crc32`  checksum  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
- row  `Crc64`  checksum  `permitNonCryptographicHashes`  `soundnessProvider`  64
- row  `Adler32`  checksum  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
+ column  Algorithme
+ column  Sorte
+ column  Permis
+ column  Fournisseurs
+ column  Bits
+ row  `Sha2[224]`, `Sha2[256]`, `Sha2[384]`, `Sha2[512]`  hachage  aucun  `javaBaseProvider`, `soundnessProvider`  224–512
+ row  `Blake3`  hachage  aucun  `soundnessProvider`  256
+ row  `Sha1`  hachage faible  `permitDeprecatedCrypto`  `javaBaseProvider`, `soundnessProvider`  160
+ row  `Md5`  hachage faible  `permitDisallowedCrypto`  `javaBaseProvider`, `soundnessProvider`  128
+ row  `Murmur3`  hachage rapide  `permitNonCryptographicHashes`  `soundnessProvider`  128
+ row  `Crc32`  somme de contrôle  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
+ row  `Crc64`  somme de contrôle  `permitNonCryptographicHashes`  `soundnessProvider`  64
+ row  `Adler32`  somme de contrôle  `permitNonCryptographicHashes`  `javaBaseProvider`, `soundnessProvider`  32
```

- The permits are imported from the package `cryptoPermits`.
+ Les permis s'importent depuis le package `cryptoPermits`.

- `javaBaseProvider` uses the JDK's `MessageDigest` where there is one. Off the JVM there is
- not, and the same import then selects pure-Scala implementations of MD5, SHA-1, SHA-2 and
- CRC-32, which are validated byte for byte against the JDK's. Code which hashes therefore reads
- the same, and produces the same digests, on the JVM, in a browser and inside a WebAssembly
- component.
+ `javaBaseProvider` utilise le `MessageDigest` du JDK quand il en existe un. Hors de la JVM, il
+ n'y en a pas, et le même import choisit alors des implémentations en pur Scala de MD5, SHA-1,
+ SHA-2 et CRC-32, validées octet par octet par rapport à celles du JDK. Le code qui hache se lit
+ donc de la même façon, et produit les mêmes condensés, sur la JVM, dans un navigateur et dans
+ un composant WebAssembly.

- ### The algorithm in the type
+ ### L'algorithme dans le type

- A digest has its algorithm in its type, as in `Digest in Sha2[256]`. Where the expected type
- already says which algorithm is wanted, `digest` need not name it. A field declared with the
- type fixes what `digest` computes:
+ Un condensé porte son algorithme dans son type, comme dans `Digest in Sha2[256]`. Quand le
+ type attendu indique déjà l'algorithme voulu, `digest` n'a pas besoin de le nommer. Un champ
+ déclaré avec ce type fixe ce que calcule `digest` :

```scala
case class Block(digest: Digest in Sha2[256], payload: Text)

val payload = t"""{"amount": 100}"""
val block = Block(payload.digest, payload)
```

```scala fuel
run
##
Out.println(block.digest.serialize[Hex])
```

- That is the pattern throughout. Digests, HMACs, keys and signatures are all bytes with a type
- parameter for the algorithm which produced them. A value from one algorithm cannot be passed
- where another is expected, and the algorithm rarely has to be written twice:
+ C'est le schéma que l'on retrouve partout. Condensés, HMAC, clés et signatures sont tous des
+ octets avec un paramètre de type pour l'algorithme qui les a produits. Une valeur issue d'un
+ algorithme ne peut pas être passée là où un autre est attendu, et l'algorithme a rarement besoin
+ d'être écrit deux fois :

```scala fuel
compile
errors show
##
Block(payload.digest[Sha2[512]], payload)
```

- [Cryptography](cryptography.md) covers HMACs, keys and signatures.
+ [Cryptographie](cryptography.md) traite des HMAC, des clés et des signatures.

- ### Hashing your own types
+ ### Hacher vos propres types

- Any value which can be reduced to bytes is digestible. A case class derives that from its
- fields, so a structured value is hashed as one:
+ Toute valeur réductible à des octets est hachable. Une case class le dérive de ses champs : une
+ valeur structurée se hache donc d'un seul tenant :

```scala
case class Person(name: Text, age: Int)
```

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).digest[Sha2[256]].serialize[Hex])
Out.println(Person(t"Hamlet", 31).digest[Sha2[256]].serialize[Hex])
```

- A digest is itself digestible, so a hash can be combined into a larger structure, and hashed
- again.
+ Un condensé est lui-même hachable : un hachage peut donc être combiné dans une structure plus
+ grande, et haché à nouveau.

- ### Hashing a stream
+ ### Hacher un flux

- Data which is too large to hold is hashed as it passes. The digest accumulates over successive
- chunks, and carries partial blocks across the boundaries between them. The result is therefore
- identical whatever sizes the chunks happen to arrive in, and is the digest of the whole text
- above:
+ Les données trop volumineuses pour être conservées sont hachées au passage. Le condensé s'accumule
+ sur des morceaux successifs, et reporte les blocs partiels au-delà des frontières entre eux. Le
+ résultat est donc identique quelles que soient les tailles des morceaux reçus, et c'est le
+ condensé du texte entier ci-dessus :

```scala
val chunks = Chain(t"Hello ".in[Data], t"wor".in[Data], t"ld".in[Data])
```

```scala fuel
run
##
Out.println(chunks.checksum[Sha2[256]].serialize[Hex])
```

- This is what makes a digest computable over a [stream](streams.md) with bounded memory. It is
- why hashing composes with [compression](compression.md) and
- [encryption](cryptography.md) in one pipeline, without the data being held between them.
+ C'est ce qui permet de calculer un condensé sur un [flux](streams.md) avec une mémoire bornée.
+ C'est pourquoi le hachage se compose avec la [compression](compression.md) et le
+ [chiffrement](cryptography.md) dans un même pipeline, sans que les données soient conservées
+ entre eux.

- `checksum` digests what is *behind* a value: a stream's bytes and, for a
- [filesystem](filesystem.md) path, the content of a file or of a directory. A directory's
- checksum covers its regular files, in the order of their paths relative to it, and each
- contributes its relative path and its content. So:
+ `checksum` hache ce qui se trouve *derrière* une valeur : les octets d'un flux et, pour un chemin
+ du [système de fichiers](filesystem.md), le contenu d'un fichier ou d'un répertoire. La somme de
+ contrôle d'un répertoire couvre ses fichiers ordinaires, dans l'ordre de leurs chemins relatifs à
+ celui-ci, et chacun y contribue par son chemin relatif et son contenu. Ainsi :

- - a rename, a move or an edit changes the checksum;
- - two directories which hold the same files have the same checksum, wherever they are;
- - an empty directory has a checksum too.
+ - un renommage, un déplacement ou une modification change la somme de contrôle ;
+ - deux répertoires contenant les mêmes fichiers ont la même somme de contrôle, où qu'ils se
+   trouvent ;
+ - un répertoire vide a lui aussi une somme de contrôle.

- That is the identity a build keys its caches by:
+ C'est l'identité par laquelle un build indexe ses caches :

```scala fuel
typecheck
##
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val directory = temporaryDirectory[Path on Linux] / "hashing"
directory.create[Directory](CreateFlag.Replace)

val identity: Digest in Sha2[256] = directory.checksum[Sha2[256]]
```

- ### Rendering a digest
+ ### Rendre un condensé

- A digest's bytes are written in any base encoding: hexadecimal for display, or Base64 where
- they must be compact:
+ Les octets d'un condensé s'écrivent dans n'importe quel encodage en base : l'hexadécimal pour
+ l'affichage, ou le Base64 là où ils doivent être compacts :

```scala fuel
run
##
locally:
  import alphabets.base64Standard
  Out.println(t"Hello world".digest[Sha2[256]].serialize[Base64])
```

- ### Self-describing digests
+ ### Condensés auto-descriptifs

- A digest's bytes say nothing about the algorithm which produced them. A digest which is stored
- or sent alone can be checked only by something which already knows how it was made. A
- [Multihash](https://multiformats.io/multihash/) prefixes the digest with a code for its
- algorithm and its length, so it describes itself. Content-addressed storage, and the
- distributed protocols built on it, exchange multihashes:
+ Les octets d'un condensé ne disent rien de l'algorithme qui les a produits. Un condensé stocké ou
+ envoyé seul ne peut être vérifié que par quelque chose qui sait déjà comment il a été fait. Un
+ [Multihash](https://multiformats.io/multihash/) préfixe le condensé d'un code pour son algorithme
+ et sa longueur : il se décrit ainsi lui-même. Le stockage adressé par le contenu, et les
+ protocoles distribués construits dessus, échangent des multihashes :

```scala fuel
run
##
Out.println(Multihash(t"Hello world".digest[Sha2[256]]).serialize.serialize[Hex])
```

- The two bytes `12` and `20` say that a SHA-256 digest of thirty-two bytes follows. The
- algorithm's code comes from the digest's type, so a multihash cannot claim an algorithm other
- than the one which computed it.
+ Les deux octets `12` et `20` indiquent qu'un condensé SHA-256 de trente-deux octets suit. Le
+ code de l'algorithme provient du type du condensé : un multihash ne peut donc pas prétendre à
+ un autre algorithme que celui qui l'a calculé.

- ### Weak algorithms
+ ### Algorithmes faibles

- MD5 and SHA-1 are broken for the purposes of security, and remain only for compatibility.
- Without a permission in scope, neither compiles:
+ MD5 et SHA-1 sont cassés du point de vue de la sécurité, et ne subsistent que pour la
+ compatibilité. Sans permission dans la portée, aucun des deux ne compile :

```scala fuel
compile
errors show
##
t"Hello world".digest[Md5]
```

- Importing the permission makes the choice visible and deliberate:
+ Importer la permission rend le choix visible et délibéré :

```scala fuel
run
##
locally:
  import cryptoPermits.permitDisallowedCrypto
  Out.println(t"Hello world".digest[Md5].serialize[Hex])
```

- The strong algorithms need no permission.
+ Les algorithmes robustes ne nécessitent aucune permission.

- ### Checksums
+ ### Sommes de contrôle

- `Crc32`, `Crc64` and `Adler32` are not hashes but checksums. They detect accidental
- corruption, and defend against nothing. They sit behind a permit of their own, which concedes
- only that no adversary is being defended against:
+ `Crc32`, `Crc64` et `Adler32` ne sont pas des hachages mais des sommes de contrôle. Elles
+ détectent une corruption accidentelle, et ne protègent contre rien. Elles se trouvent derrière
+ un permis qui leur est propre, et qui concède seulement qu'on ne se défend contre aucun
+ adversaire :

```scala
import cryptoPermits.permitNonCryptographicHashes
```

```scala fuel
run
##
Out.println(t"123456789".digest[Crc32].serialize[Hex])
```

- A source of bytes, such as a file or a download, is checksummed without being held in memory,
- by digesting its stream as it flows, exactly as `checksum` did above.
+ Une source d'octets, telle qu'un fichier ou un téléchargement, reçoit sa somme de contrôle sans
+ être conservée en mémoire, en hachant son flux au fil de l'eau, exactement comme l'a fait
+ `checksum` ci-dessus.

- ### Fast hashing
+ ### Hachage rapide

- `Murmur3` is MurmurHash3 in its 128-bit form. It is not a checksum, but it is not
- cryptographic either. It is a hash made for hash tables, [Bloom filters](bloom-filters.md) and
- sharding, where the input is not adversarial, and the rounds of a cryptographic digest would
- be several times the work for no benefit. It sits behind the same permit as the checksums, and
- its output is byte for byte what Guava's `murmur3_128()` produces:
+ `Murmur3` est MurmurHash3 dans sa version 128 bits. Ce n'est pas une somme de contrôle, mais ce
+ n'est pas non plus un hachage cryptographique. C'est un hachage conçu pour les tables de
+ hachage, les [filtres de Bloom](bloom-filters.md) et le sharding, où l'entrée n'est pas hostile,
+ et où les tours d'un condensé cryptographique représenteraient plusieurs fois le travail sans
+ aucun bénéfice. Il se trouve derrière le même permis que les sommes de contrôle, et sa sortie
+ est, octet pour octet, celle que produit le `murmur3_128()` de Guava :

```scala fuel
run
##
locally:
  import providers.soundnessProvider
  Out.println(t"hello".digest[Murmur3].serialize[Hex])
```

- ### BLAKE3
+ ### BLAKE3

- `Blake3` is implemented in pure Scala, by `soundnessProvider`, and offers the algorithm's three
- modes:
+ `Blake3` est implémenté en pur Scala, par `soundnessProvider`, et propose les trois modes de
+ l'algorithme :

```tabular
- column  Mode
- column  Input
- column  Use
- row  hash  the data  an ordinary digest
- row  keyed hash  a key and the data  message authentication
- row  key derivation  a context string and key material  deriving keys
+ column  Mode
+ column  Entrée
+ column  Usage
+ row  hachage  les données  un condensé ordinaire
+ row  hachage avec clé  une clé et les données  authentification de messages
+ row  dérivation de clé  une chaîne de contexte et du matériel de clé  dériver des clés
```

- All three are checked against the official test data, at every input length which that data
- covers, so the implementation is verified, and not merely tested for consistency with itself.
+ Les trois sont vérifiés par rapport aux données de test officielles, pour chaque longueur
+ d'entrée que couvrent ces données : l'implémentation est donc vérifiée, et pas seulement testée
+ pour sa cohérence avec elle-même.

- Its output is extendable. A BLAKE3 digest may be taken at any length, not only at 256 bits,
- which is what makes it usable for deriving keys as well as for hashing.
+ Sa sortie est extensible. Un condensé BLAKE3 peut être pris à n'importe quelle longueur, et
+ pas seulement à 256 bits, ce qui la rend utilisable pour dériver des clés autant que pour
+ hacher.

- ### Related
+ ### Voir aussi

- - [UUIDs](uuid.md), [packaging](packaging.md) and [containers](docker.md) identify values by their digests.
+ - Les [UUID](uuid.md), le [packaging](packaging.md) et les [conteneurs](docker.md) identifient
+   des valeurs par leurs condensés.
