- ## Base Encoding
+ ## Encodage en base

- ### About
+ ### Présentation

- Binary data often has to travel through a channel which carries only text: a URL, a JSON
- string, an email header. A
- [base encoding](https://en.wikipedia.org/wiki/Binary-to-text_encoding) maps bytes onto a safe
- set of characters, so that they survive the trip.
+ Les données binaires doivent souvent passer par un canal qui ne transporte que du texte : une
+ URL, une chaîne JSON, un en-tête d'email. Un
+ [encodage en base](https://en.wikipedia.org/wiki/Binary-to-text_encoding) fait correspondre les
+ octets à un ensemble sûr de caractères, afin qu'ils survivent au voyage.

- Bytes are serialized to text, and back, in Base64, Base32, hexadecimal and the smaller bases.
- The encoding is named as a type, and its set of characters, called its *alphabet*, is chosen
- in scope.
+ Les octets se sérialisent en texte, et inversement, en Base64, en Base32, en hexadécimal et dans
+ les bases plus petites. L'encodage est désigné par un type, et son ensemble de caractères,
+ appelé son *alphabet*, est choisi dans la portée.

- ### On base encoding
+ ### L'encodage en base

- "Base64" is not one thing. The standard alphabet, the URL-safe alphabet, and the variants used
- by MIME, by bcrypt and by IMAP all encode the same bytes into different characters, and mixing
- them corrupts the result. Decoding raises a further question: what to do with a character
- which is not in the alphabet at all. Most APIs bury these choices in flags, or fix them
- silently.
+ « Base64 » n'est pas une chose unique. L'alphabet standard, l'alphabet sûr pour les URL, et les
+ variantes utilisées par MIME, par bcrypt et par IMAP encodent tous les mêmes octets en
+ caractères différents, et les mélanger corrompt le résultat. Le décodage soulève une autre
+ question : que faire d'un caractère qui ne fait pas du tout partie de l'alphabet. La plupart des
+ API enfouissent ces choix dans des indicateurs, ou les figent sans rien dire.

- Here the encoding is a type, and the alphabet is an explicit value:
+ Ici, l'encodage est un type, et l'alphabet est une valeur explicite :

- - The base, which is how many bits each character carries, is a type parameter.
- - The alphabet is a [given], brought into scope by import, so the standard and the URL-safe
-   forms of Base64 differ by one line.
- - Decoding validates each character, and reports one which is outside the alphabet as a typed
-   error.
+ - La base, c'est-à-dire le nombre de bits que porte chaque caractère, est un paramètre de type.
+ - L'alphabet est un [given], amené dans la portée par un import : les formes standard et sûre
+   pour les URL de Base64 diffèrent donc d'une ligne.
+ - Le décodage valide chaque caractère, et signale un caractère hors de l'alphabet par une
+   erreur typée.

- Naming the alphabet by import, instead of passing a flag, is the
- [declarative context](../../philosophy/declarative-context.md) style used throughout.
+ Nommer l'alphabet par un import, au lieu de passer un indicateur, c'est le style du
+ [contexte déclaratif](../../philosophy/declarative-context.md) employé partout.

- The word "encoding" is used for two different jobs, and it is worth being precise about which
- this is. What happens here is *serialization*. Bytes become text, and nothing about the
- meaning of the bytes is consulted or preserved: the same bytes serialize identically, whether
- they were an image or a signature. Turning a *value* into JSON or XML is the other job. It is
- [encoding](json.md), and it needs to know what the value is. The operations here are named
- accordingly: `serialize` and `deserialize`.
+ Le mot « encodage » sert à deux tâches différentes, et il vaut la peine de préciser de laquelle
+ il s'agit ici. Ce qui se passe ici est une *sérialisation*. Des octets deviennent du texte, et
+ rien du sens de ces octets n'est consulté ni préservé : les mêmes octets se sérialisent à
+ l'identique, qu'ils aient été une image ou une signature. Transformer une *valeur* en JSON ou en
+ XML est l'autre tâche. C'est l'[encodage](json.md), et il a besoin de savoir ce qu'est la
+ valeur. Les opérations sont nommées en conséquence : `serialize` et `deserialize`.

- Everything comes from the `soundness` package. The last import lets the samples print their
- results:
+ Tout provient du package `soundness`. Le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Serializing bytes
+ ### Sérialiser des octets

- `serialize` writes bytes as text in a named encoding. The characters come from the alphabet in
- scope:
+ `serialize` écrit des octets sous forme de texte dans un encodage nommé. Les caractères
+ proviennent de l'alphabet dans la portée :

```scala
import alphabets.base64Standard

val bytes = t"Hello".in[Data]
```

```scala fuel
run
##
Out.println(bytes.serialize[Base64])
```

- `serialize` works on any value which encodes to bytes, not only on raw bytes. A value with an
- `Encodable in Data` instance, such as a [digest](hashing.md), is serialized directly.
+ `serialize` fonctionne sur toute valeur qui s'encode en octets, pas seulement sur des octets
+ bruts. Une valeur dotée d'une instance `Encodable in Data`, comme un [condensé](hashing.md), se
+ sérialise directement.

- With no alphabet for the base in scope, there is nothing to serialize with:
+ Sans alphabet pour la base dans la portée, il n'y a rien avec quoi sérialiser :

```scala fuel
compile
errors show
##
bytes.serialize[Base32]
```

- ### Deserializing
+ ### Désérialiser

- `deserialize` reads text back to bytes, in the same encoding. A character outside the alphabet
- raises a `Serialization.Error`, so deserializing needs a [strategy] in scope:
+ `deserialize` relit du texte en octets, dans le même encodage. Un caractère hors de l'alphabet
+ lève une `Serialization.Error` : désérialiser exige donc une [stratégie] dans la portée :

```scala
import strategies.throwUnsafely
```

```scala fuel
run
##
Out.println(t"SGVsbG8=".deserialize[Base64].utf8)
```

```scala fuel
run
errors show
##
t"SGVsbG8!".deserialize[Base64]
```

- ### Choosing an alphabet
+ ### Choisir un alphabet

- The alphabet in scope decides the exact characters. `base64Standard` and `base64Url` are the
- common choices. The URL-safe form is unpadded, and avoids `+` and `/`, which have a meaning in
- a URL:
+ L'alphabet dans la portée décide des caractères exacts. `base64Standard` et `base64Url` sont les
+ choix courants. La forme sûre pour les URL est sans remplissage, et évite `+` et `/`, qui ont
+ un sens dans une URL :

```scala fuel
run
##
locally:
  import alphabets.base64Url
  Out.println(bytes.serialize[Base64])

locally:
  import alphabets.hexLowerCase
  Out.println(bytes.serialize[Hex])
```

- A *strict* alphabet accepts only its own characters when decoding. A tolerant one also accepts
- recognized equivalents, such as hexadecimal in either case. The strictness of a decode is
- therefore a choice between imports: `hexLowerCase` reads either case, and `hexStrictLowerCase`
- only its own.
+ Un alphabet *strict* n'accepte au décodage que ses propres caractères. Un alphabet tolérant
+ accepte aussi des équivalents reconnus, comme l'hexadécimal dans l'une ou l'autre casse. La
+ rigueur d'un décodage est donc un choix entre imports : `hexLowerCase` lit les deux casses, et
+ `hexStrictLowerCase` seulement la sienne.

- The full set is in the package `alphabets`:
+ L'ensemble complet se trouve dans le package `alphabets` :

```tabular
- column  Base
- column  Given
- column  Variant
- column  Padded
- column  Also accepts
- row  `Base64`  `base64Standard`  standard  yes
- row  `Base64`  `base64Unpadded`  standard, unpadded  no
- row  `Base64`  `base64Url`  URL-safe, with `-` and `_`  no
- row  `Base64`  `base64Xml`  XML, with `+` and `-`, padded with `.`  yes
- row  `Base64`  `base64Imap`  IMAP, with `+` and `,`  no
- row  `Base64`  `base64Yui`  YUI, with `.` and `_`  no
- row  `Base64`  `base64Radix64`  Radix-64  yes
- row  `Base64`  `base64Bcrypt`  bcrypt, with `.` and `/` first  no
- row  `Base64`  `base64Sasl`  SASL, with `+` and `,`  no
- row  `Base64`  `base64Uuencoding`  uuencoding  no
- row  `Base32`  `base32StrictUpperCase`  standard, upper-case  yes
- row  `Base32`  `base32StrictLowerCase`  standard, lower-case  yes
- row  `Base32`  `base32UpperCase`  standard, upper-case  yes  lower case
- row  `Base32`  `base32LowerCase`  standard, lower-case  yes  upper case
- row  `Base32`  `base32ExtendedHexUpperCase`  extended hex, upper-case  yes  lower case
- row  `Base32`  `base32ExtendedHexLowerCase`  extended hex, lower-case  yes  upper case
- row  `Base32`  `base32ZBase32`  z-base-32  yes
- row  `Base32`  `base32ZBase32Unpadded`  z-base-32, unpadded  no
- row  `Base32`  `base32Geohash`  Geohash  no
- row  `Base32`  `base32WordSafe`  word-safe  no
- row  `Base32`  `base32Crockford`  Crockford  no  lower case; `o` and `O` as `0`; `i`, `I`, `l` and `L` as `1`
- row  `Hex`  `hexStrictUpperCase`  upper-case  no
- row  `Hex`  `hexStrictLowerCase`  lower-case  no
- row  `Hex`  `hexUpperCase`  upper-case  no  lower case
- row  `Hex`  `hexLowerCase`  lower-case  no  upper case
- row  `Hex`  `hexBioctal`  bioctal  no
- row  `Octal`  `octalStandard`  standard  no
- row  `Quaternary`  `quaternaryStandard`  `0123`  no
- row  `Quaternary`  `quaternaryDnaNucleotide`  DNA nucleotides, `ATCG`  no
- row  `Binary`  `binaryStandard`  `01`  no
+ column  Base
+ column  Given
+ column  Variante
+ column  Remplissage
+ column  Accepte aussi
+ row  `Base64`  `base64Standard`  standard  oui
+ row  `Base64`  `base64Unpadded`  standard, sans remplissage  non
+ row  `Base64`  `base64Url`  sûr pour les URL, avec `-` et `_`  non
+ row  `Base64`  `base64Xml`  XML, avec `+` et `-`, rempli avec `.`  oui
+ row  `Base64`  `base64Imap`  IMAP, avec `+` et `,`  non
+ row  `Base64`  `base64Yui`  YUI, avec `.` et `_`  non
+ row  `Base64`  `base64Radix64`  Radix-64  oui
+ row  `Base64`  `base64Bcrypt`  bcrypt, avec `.` et `/` en premier  non
+ row  `Base64`  `base64Sasl`  SASL, avec `+` et `,`  non
+ row  `Base64`  `base64Uuencoding`  uuencoding  non
+ row  `Base32`  `base32StrictUpperCase`  standard, majuscules  oui
+ row  `Base32`  `base32StrictLowerCase`  standard, minuscules  oui
+ row  `Base32`  `base32UpperCase`  standard, majuscules  oui  minuscules
+ row  `Base32`  `base32LowerCase`  standard, minuscules  oui  majuscules
+ row  `Base32`  `base32ExtendedHexUpperCase`  hexadécimal étendu, majuscules  oui  minuscules
+ row  `Base32`  `base32ExtendedHexLowerCase`  hexadécimal étendu, minuscules  oui  majuscules
+ row  `Base32`  `base32ZBase32`  z-base-32  oui
+ row  `Base32`  `base32ZBase32Unpadded`  z-base-32, sans remplissage  non
+ row  `Base32`  `base32Geohash`  Geohash  non
+ row  `Base32`  `base32WordSafe`  sans mots  non
+ row  `Base32`  `base32Crockford`  Crockford  non  minuscules ; `o` et `O` comme `0` ; `i`, `I`, `l` et `L` comme `1`
+ row  `Hex`  `hexStrictUpperCase`  majuscules  non
+ row  `Hex`  `hexStrictLowerCase`  minuscules  non
+ row  `Hex`  `hexUpperCase`  majuscules  non  minuscules
+ row  `Hex`  `hexLowerCase`  minuscules  non  majuscules
+ row  `Hex`  `hexBioctal`  bioctal  non
+ row  `Octal`  `octalStandard`  standard  non
+ row  `Quaternary`  `quaternaryStandard`  `0123`  non
+ row  `Quaternary`  `quaternaryDnaNucleotide`  nucléotides de l'ADN, `ATCG`  non
+ row  `Binary`  `binaryStandard`  `01`  non
```

- ### Other bases
+ ### Autres bases

- The same `serialize` and `deserialize` handle every base, each with alphabets of its own. The
- base is the number of bits which each character carries:
+ Les mêmes `serialize` et `deserialize` gèrent toutes les bases, chacune avec ses propres
+ alphabets. La base est le nombre de bits que porte chaque caractère :

```tabular
- column  Encoding
- column  Bits per character
- row  `Base64`  6
- row  `Base32`  5
- row  `Hex`  4
- row  `Octal`  3
- row  `Quaternary`  2
- row  `Binary`  1
+ column  Encodage
+ column  Bits par caractère
+ row  `Base64`  6
+ row  `Base32`  5
+ row  `Hex`  4
+ row  `Octal`  3
+ row  `Quaternary`  2
+ row  `Binary`  1
```

```scala
import alphabets.{hexLowerCase, binaryStandard, quaternaryDnaNucleotide}
```

```scala fuel
run
##
Out.println(bytes.serialize[Hex])
Out.println(bytes.serialize[Binary])
Out.println(bytes.serialize[Quaternary])
```

- A hexadecimal literal for bytes is written with the `hex"…"` interpolator, as
- [binary data](binary-data.md#reading) shows.
+ Un littéral hexadécimal d'octets s'écrit avec l'interpolateur `hex"…"`, comme le montre
+ [données binaires](binary-data.md#reading).

- ### Encoding a stream
+ ### Encoder un flux

- An alphabet is also a stage of a [stream](streams.md#composing-a-pipeline). Bytes are encoded
- as they flow, without being gathered first, which is what a base-encoded body written to a
- socket requires, or a large file prepared for transport as text:
+ Un alphabet est aussi une étape d'un [flux](streams.md#composing-a-pipeline). Les octets sont
+ encodés au fil de leur passage, sans être d'abord rassemblés, ce qu'exige un corps encodé en
+ base écrit sur un socket, ou un gros fichier préparé pour être transporté sous forme de texte :

```scala
val encoded = bytes.stream.via(summon[Alphabet[Hex]])
```

- The streaming form and the whole-value form agree byte for byte. That includes the boundaries
- where a Base64 group of three bytes, or a Base32 group of five, straddles two chunks. Decoding
- runs in the same way, so a stream may be encoded on one side and decoded on the other, with
- neither side holding the whole of it.
+ La forme en flux et la forme sur valeur entière concordent octet pour octet. Cela vaut aussi aux
+ frontières où un groupe Base64 de trois octets, ou un groupe Base32 de cinq, chevauche deux
+ morceaux. Le décodage fonctionne de la même façon : un flux peut donc être encodé d'un côté et
+ décodé de l'autre, sans qu'aucun des deux côtés n'en détienne la totalité.

- ### Related
+ ### Voir aussi

- - [CBOR](cbor.md), [cryptography](cryptography.md), [SSH](ssh.md), [geography](geography.md) and [text](text.md) encode bytes and coordinates in these alphabets.
+ - [CBOR](cbor.md), la [cryptographie](cryptography.md), [SSH](ssh.md), la
+   [géographie](geography.md) et le [texte](text.md) encodent des octets et des coordonnées dans
+   ces alphabets.
