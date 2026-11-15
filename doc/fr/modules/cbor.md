- ## CBOR
+ ## CBOR

- ### About
+ ### Présentation

- [CBOR](https://en.wikipedia.org/wiki/CBOR) is a binary cousin of JSON. It has the same data
- model, encoded compactly for machines instead of legibly for people, and is standardized as
- [RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949).
+ [CBOR](https://en.wikipedia.org/wiki/CBOR) est un cousin binaire de JSON. Il a le même modèle de
+ données, encodé de façon compacte pour les machines plutôt que lisible pour les humains, et est
+ normalisé par la [RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949).

- It is read and written with the same interface as the textual formats. Bytes parse into a
- `Cbor` value. Case classes convert to and from it with [derived][derivation] codecs.
- Navigation, updates and lenses work as they do for [JSON](json.md) and [YAML](yaml.md).
+ Il se lit et s'écrit avec la même interface que les formats textuels. Les octets s'analysent en
+ une valeur `Cbor`. Les case classes s'y convertissent et s'en reconstruisent avec des codecs
+ [dérivés][dérivation]. La navigation, les mises à jour et les lentilles fonctionnent comme pour
+ [JSON](json.md) et [YAML](yaml.md).

- ### On binary interchange
+ ### L'échange binaire

- JSON pays for its readability in bytes and in parsing time. Where no person reads the data, as
- in a device protocol, a cache entry or a message queue, that price buys nothing. CBOR keeps
- JSON's shape of maps, arrays, numbers, text and byte strings, in a compact binary form.
+ JSON paie sa lisibilité en octets et en temps d'analyse. Là où personne ne lit les données, comme
+ dans un protocole d'appareil, une entrée de cache ou une file de messages, ce prix n'achète rien.
+ CBOR conserve la forme de JSON — maps, tableaux, nombres, texte et chaînes d'octets — sous une
+ forme binaire compacte.

- Binary formats are usually consumed through byte-fiddling APIs, far clumsier than those of
- their textual equivalents. Here CBOR gets the full treatment of a textual format: derived
- codecs, typed errors with reasons, dynamic access, and optics. Choosing the compact format
- costs the code no expressiveness.
+ Les formats binaires se consomment généralement au moyen d'API de manipulation d'octets, bien
+ plus maladroites que celles de leurs équivalents textuels. Ici, CBOR bénéficie de tout le
+ traitement d'un format textuel : codecs dérivés, erreurs typées avec leurs raisons, accès
+ dynamique et optiques. Choisir le format compact ne coûte au code aucune expressivité.

- The codec is derived from the type, so the type is the schema. The
- [correctness](../../philosophy/correctness.md) of the encoding follows from the definition, not
- from tests.
+ Le codec est dérivé du type : le type est donc le schéma. La
+ [correction](../../philosophy/correctness.md) de l'encodage découle de la définition, et non des
+ tests.

- Everything comes from the `soundness` package. The samples throw any error as an exception, and
- the last import lets them print their results:
+ Tout vient du package `soundness`. Les exemples lèvent toute erreur sous forme d'exception, et
+ le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Encoding and decoding
+ ### Encoder et décoder

- `in[Cbor]` encodes a value as a `Cbor` value, and `as` decodes one to a type:
+ `in[Cbor]` encode une valeur en une valeur `Cbor`, et `as` en décode une vers un type :

```scala
case class Person(name: Text, age: Int)

val encoded = Person(t"Hamlet", 30).in[Cbor]
```

```scala fuel
run
##
Out.println(encoded.as[Person].inspect)
```

- A `Cbor` value is a tree. `Cbor.unseal` exposes it as a `Cbor.Ast`, which serializes to its
- bytes:
+ Une valeur `Cbor` est un arbre. `Cbor.unseal` l'expose sous forme de `Cbor.Ast`, qui se
+ sérialise en ses octets :

```scala
val bytes: Data = Cbor.Ast.encodable.encoded(Cbor.unseal(encoded))
```

- Bytes are read straight back to a type in one step, with `read[Person in Cbor]`, whether they
- arrive whole or as a [stream](streams.md) of chunks:
+ Les octets se relisent directement vers un type en une étape, avec `read[Person in Cbor]`,
+ qu'ils arrivent en entier ou sous forme de [flux](streams.md) de morceaux :

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println(Chain(bytes).read[Person in Cbor].inspect)
```

- The two steps of writing combine in the same way. `in[Data over Cbor]` encodes a value straight
- to its bytes, and is the mirror of `read[Person in Cbor]`:
+ Les deux étapes de l'écriture se combinent de la même façon. `in[Data over Cbor]` encode une
+ valeur directement en ses octets, et est le pendant de `read[Person in Cbor]` :

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).in[Data over Cbor].read[Person in Cbor].inspect)
```

- The same `Person`, written as compact [JSON](json.md), is 26 bytes.
+ Le même `Person`, écrit en [JSON](json.md) compact, fait 26 octets.

- The `@name[Cbor]` [annotation](annotations.md) renames a field in the encoded map. A sealed
- hierarchy encodes with a discriminator, declared once:
+ L'[annotation](annotations.md) `@name[Cbor]` renomme un champ dans la map encodée. Une
+ hiérarchie scellée s'encode avec un discriminant, déclaré une seule fois :

```scala
enum Status:
  case Active(since: Int)
  case Retired

given statusDiscriminable: (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")

val active: Status = Status.Active(2020)
```

```scala fuel
run
##
Out.println(active.in[Cbor].as[Status].inspect)
```

- ### Optional fields
+ ### Champs optionnels

- An `Optional` field is left out of the map when it is unset, and a field with a default takes
- that default when the map lacks it. A message may therefore gain fields without breaking
- readers which are older than they are.
+ Un champ `Optional` est omis de la map quand il n'est pas défini, et un champ ayant une valeur
+ par défaut prend cette valeur quand la map ne le contient pas. Un message peut donc gagner des
+ champs sans casser les lecteurs plus anciens que ces champs.

- Reading an `Optional` field follows the policies which
- [Decoding](decoding.md#policies-for-optional-fields) describes. Each is varied separately, by a
- given in the package `optionalityOptions`:
+ La lecture d'un champ `Optional` suit les politiques que décrit
+ [Décodage](decoding.md#policies-for-optional-fields). Chacune se modifie séparément, par un given
+ du package `optionalityOptions` :

```tabular
- column  A field which is
- column  Reads as
- column  Unless
- row  omitted from the map, or `undefined`  `Unset`  `strictCborAbsence` makes it an error
- row  a CBOR `null`  `Unset`  `strictCborNulls` hands it to the inner decoder
- row  present but rejected by the inner decoder  an error  `lenientCborFaults` reads it as `Unset`
+ column  Un champ qui est
+ column  Se lit comme
+ column  Sauf si
+ row  omis de la map, ou `undefined`  `Unset`  `strictCborAbsence` en fait une erreur
+ row  un `null` CBOR  `Unset`  `strictCborNulls` le confie au décodeur interne
+ row  présent mais rejeté par le décodeur interne  une erreur  `lenientCborFaults` le lit comme `Unset`
```

- Here a text is put where a number belongs, and the lenient policy reads it as `Unset`:
+ Ici, un texte est placé là où un nombre est attendu, et la politique tolérante le lit comme
+ `Unset` :

```scala
case class Reading(sensor: Text, value: Optional[Int])

val tolerant =
  import dynamicAccess.dynamicCbor
  import optionalityOptions.lenientCborFaults
  (Reading(t"a", 1).in[Cbor].value = t"x").as[Reading]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

- ### Navigating and updating
+ ### Naviguer et mettre à jour

- With dynamic access enabled, a map's fields are read as members and an array is indexed.
- Assigning to a field gives a new value, and assigning `Unset` removes the field:
+ Avec l'accès dynamique activé, les champs d'une map se lisent comme des membres et un tableau
+ s'indexe. Affecter un champ donne une nouvelle valeur, et affecter `Unset` supprime le champ :

```scala
import dynamicAccess.dynamicCbor

val person = Person(t"Hamlet", 30).in[Cbor]
```

```scala fuel
run
##
Out.println(person.name.as[Text])
Out.println((person.age = 40).as[Person].inspect)
```

- The import grants the permission `Cbor is Dynamical` to the rest of its scope.
- `dynamically[Cbor]:` grants it to a single block instead, as
- [JSON](json.md#fields-as-members) describes.
+ L'import accorde la permission `Cbor is Dynamical` au reste de sa portée. `dynamically[Cbor]:`
+ l'accorde plutôt à un seul bloc, comme le décrit [JSON](json.md#fields-as-members).

- A deeper update uses a [lens](optics.md), with the optics `Each` and `Filter` to change many
- elements at once, exactly as for the textual formats.
+ Une mise à jour plus profonde utilise une [lentille](optics.md), avec les optiques `Each` et
+ `Filter` pour modifier de nombreux éléments à la fois, exactement comme pour les formats
+ textuels.

- ### Building directly
+ ### Construire directement

- Where no case class fits, a CBOR map is assembled from named arguments:
+ Quand aucune case class ne convient, une map CBOR s'assemble à partir d'arguments nommés :

```scala fuel
run
##
Out.println(Cbor.make(name = t"Juliet".in[Cbor], age = 13.in[Cbor]).as[Person].inspect)
```

- ### Diagnostic notation
+ ### Notation de diagnostic

- RFC 8949 defines a human-readable rendering of CBOR. It is the format's counterpart of a hex
- dump, but one which can actually be read, and `show` produces it. A byte string is written as
- `h'…'`, and arrays and maps as their JSON-like equivalents:
+ La RFC 8949 définit un rendu de CBOR lisible par un humain. C'est l'équivalent, pour ce format,
+ d'un dump hexadécimal, mais qui peut réellement être lu, et `show` le produit. Une chaîne
+ d'octets s'écrit `h'…'`, et les tableaux et les maps comme leurs équivalents de style JSON :

```scala fuel
run
##
Out.println(Cbor.Ast.parse(hex"83010203").show)
Out.println(Cbor.Ast.parse(hex"4401020304").show)
```

- This is what to reach for when a message is not what it should be. The notation says what the
- bytes mean, in the format's own vocabulary. [Base encoding](base-encoding.md) describes the
- `hex"…"` literals.
+ C'est l'outil à utiliser quand un message n'est pas ce qu'il devrait être. La notation dit ce que
+ signifient les octets, dans le vocabulaire propre au format. [Encodage en base](base-encoding.md)
+ décrit les littéraux `hex"…"`.

- ### Tags
+ ### Tags

- CBOR's tags annotate a value with its meaning. Tag 1 is an epoch time, tag 2 is a big integer,
- and the registry runs to hundreds. A tagged value keeps both its tag and the value inside it,
- so a document which carries tags is read and written without losing them. These bytes are
- tag 1 around the integer 1363896240:
+ Les tags de CBOR annotent une valeur avec sa signification. Le tag 1 est un temps epoch, le
+ tag 2 un grand entier, et le registre en compte des centaines. Une valeur taguée conserve à
+ la fois son tag et la valeur qu'il enveloppe : un document qui porte des tags se lit et s'écrit
+ donc sans les perdre. Ces octets sont le tag 1 autour de l'entier 1363896240 :

```scala
val document = Cbor.ast(Cbor.Ast.parse(hex"c11a514b67b0"))
val ast = Cbor.unseal(document)
```

```scala fuel
run
##
Out.println(ast.isTag.inspect)
Out.println(ast.tag.tag.show)
```

- ### Errors
+ ### Erreurs

- A malformed document, or a conversion which cannot be made, raises a `Cbor.Error` with a
- specific reason. A protocol failure is diagnosed from the error, not from a hex dump:
+ Un document mal formé, ou une conversion impossible, lève une `Cbor.Error` avec une raison
+ précise. Un échec de protocole se diagnostique à partir de l'erreur, et non d'un dump
+ hexadécimal :

```tabular
- column  Reason
- column  Raised when
- row  `Truncated(offset)`  the input stops mid-value
- row  `Reserved(offset, byte)`  a head byte is one CBOR reserves
- row  `BadSimpleValue(offset, value)`  a simple value is invalid
- row  `InvalidUtf8(offset)`  a text string is not valid UTF-8
- row  `Overflow(offset)`  an integer is too large for a `Long`
- row  `UnexpectedBreak(offset)`  a break stop code is unexpected
- row  `Trailing(offset)`  bytes follow the end of the value
- row  `OutOfRange`  an array index is out of range
- row  `NotType(found, expected)`  a value has the wrong type
- row  `NonStringKey`  a map key is not a string
- row  `Absent`  a value is not present
+ column  Raison
+ column  Levée quand
+ row  `Truncated(offset)`  l'entrée s'arrête au milieu d'une valeur
+ row  `Reserved(offset, byte)`  un octet de tête est l'un de ceux que CBOR réserve
+ row  `BadSimpleValue(offset, value)`  une valeur simple est invalide
+ row  `InvalidUtf8(offset)`  une chaîne de texte n'est pas de l'UTF-8 valide
+ row  `Overflow(offset)`  un entier est trop grand pour un `Long`
+ row  `UnexpectedBreak(offset)`  un code d'arrêt break est inattendu
+ row  `Trailing(offset)`  des octets suivent la fin de la valeur
+ row  `OutOfRange`  un indice de tableau est hors limites
+ row  `NotType(found, expected)`  une valeur n'a pas le bon type
+ row  `NonStringKey`  une clé de map n'est pas une chaîne
+ row  `Absent`  une valeur est absente
```

- A parsing failure carries the offset of the byte at which it occurred, which for a binary
- format is the whole of the diagnosis. Input which stops mid-value names the offset of the byte
- which should have followed. Trailing bytes name where the surplus begins. A reserved head byte
- names both its offset and its value.
+ Un échec d'analyse porte le décalage de l'octet où il s'est produit, ce qui, pour un format
+ binaire, constitue tout le diagnostic. Une entrée qui s'arrête au milieu d'une valeur indique le
+ décalage de l'octet qui aurait dû suivre. Des octets en trop indiquent où commence l'excédent.
+ Un octet de tête réservé indique à la fois son décalage et sa valeur.

- These bytes promise an array of three elements, and deliver two:
+ Ces octets promettent un tableau de trois éléments, et en livrent deux :

```scala fuel
run
errors show
##
Cbor.Ast.parse(hex"830102")
```

```scala fuel
run
##
Out.println(capture[Cbor.Error](Cbor.Ast.parse(hex"830102")).reason.inspect)
```

- A conversion to the wrong type is reported as clearly:
+ Une conversion vers le mauvais type est signalée aussi clairement :

```scala fuel
run
errors show
##
encoded.as[List[Int]]
```

- ### Parsing directly
+ ### Analyser directement

- A type with a `Cbor.Parsable` instance is read straight from the bytes, with no intermediate
- tree. The instance is derived at [compiletime], and composes a parser for that exact shape:
+ Un type ayant une instance de `Cbor.Parsable` se lit directement depuis les octets, sans arbre
+ intermédiaire. L'instance est dérivée [à la compilation][compilation], et compose un analyseur
+ pour cette forme exacte :

```scala fuel
compile
##
import breviloquence.Inlinable

given personParsable: (Person is Cbor.Parsable) = Inlinable.parsable[Person]

val direct = Chain(bytes).read[Person in Cbor]
```

- The derivation is imported by name. A sealed hierarchy, and a field whose type has a
- hand-written `Inlinable`, also need the `breviloquence.staged` module, which evaluates the
- discriminator and those instances as the code compiles.
+ La dérivation s'importe par son nom. Une hiérarchie scellée, ainsi qu'un champ dont le type a un
+ `Inlinable` écrit à la main, nécessitent aussi le module `breviloquence.staged`, qui évalue le
+ discriminant et ces instances pendant la compilation du code.

- Reading from a stream works in the same way, and the input is never assembled first. The parser
- reads each chunk as it arrives, so a value which is split across two chunks reads exactly as
- one which arrives whole. The memory held is bounded by the one item being read, not by the
- document. `read[Cbor]` likewise reads the chunks as they arrive, but builds the whole tree.
+ La lecture depuis un flux fonctionne de la même manière, et l'entrée n'est jamais assemblée au
+ préalable. L'analyseur lit chaque morceau à son arrivée : une valeur répartie sur deux morceaux
+ se lit donc exactement comme une valeur arrivée en entier. La mémoire occupée est bornée par
+ l'élément en cours de lecture, et non par le document. `read[Cbor]` lit de même les morceaux à
+ leur arrivée, mais construit l'arbre entier.

- ### Printing directly
+ ### Écrire directement

- The same derivation generates the writing side. A `Cbor.Printable` writes a value's bytes
- straight out, with no tree, and `in[Data over Cbor]` uses one wherever it exists:
+ La même dérivation génère le côté écriture. Un `Cbor.Printable` écrit directement les octets
+ d'une valeur, sans arbre, et `in[Data over Cbor]` en utilise un partout où il existe :

```scala fuel
compile
##
given personPrintable: (Person is Cbor.Printable) = Inlinable.printable[Person]

val written = Person(t"Hamlet", 30).in[Data over Cbor]
```

- The bytes are exactly those the derived encoding writes. `Cbor.Printable.emit` writes them a
- block at a time, handing each block to a function as it fills, so a value can be sent without
- its whole encoding ever being held.
+ Les octets sont exactement ceux qu'écrit l'encodage dérivé. `Cbor.Printable.emit` les écrit un
+ bloc à la fois, en confiant chaque bloc à une fonction dès qu'il est plein : une valeur peut
+ donc être envoyée sans que son encodage entier soit jamais conservé.

- ### Choosing the route for every type
+ ### Choisir la voie pour chaque type

- A record can generate both instances at once, into its companion, with `derives Cbor.Direct`.
- A single import then selects the route for every record in scope. A type which is not a record
- is a generic case class or a sealed hierarchy:
+ Un enregistrement peut générer les deux instances à la fois, dans son objet compagnon, avec
+ `derives Cbor.Direct`. Un seul import choisit alors la voie pour chaque enregistrement dans la
+ portée. Un type qui n'est pas un enregistrement est une case class générique ou une hiérarchie
+ scellée :

```tabular
- column  Import
- column  For a record
- column  For any other type
- row  `codecs.direct`  a generated parser and printer  the derived codecs, unless it has a direct instance of its own
- row  `codecs.derived`  the derived codecs, even over a direct instance  the derived codecs
- row  `codecs.strict`  a generated parser and printer  a compile error naming the type, unless it has a direct instance
+ column  Import
+ column  Pour un enregistrement
+ column  Pour tout autre type
+ row  `codecs.direct`  un analyseur et un afficheur générés  les codecs dérivés, sauf s'il a sa propre instance directe
+ row  `codecs.derived`  les codecs dérivés, même s'il existe une instance directe  les codecs dérivés
+ row  `codecs.strict`  un analyseur et un afficheur générés  une erreur de compilation nommant le type, sauf s'il a une instance directe
```

- Under `codecs.strict`, `read[Play in Cbor]` and `in[Data over Cbor]` never fall back to the
- tree. The modes are alternatives, so import one at a time.
+ Avec `codecs.strict`, `read[Play in Cbor]` et `in[Data over Cbor]` ne se rabattent jamais sur
+ l'arbre. Les modes sont des alternatives : importez-en un seul à la fois.

- ### CBOR over HTTP
+ ### CBOR sur HTTP

- A `Cbor` value is a request body or a response body, with the media type `application/cbor`,
- and a body is parsed back on arrival. A binary API is therefore consumed and offered with the
- same code as a [JSON one](json.md#json-over-http). Only the format's type differs.
+ Une valeur `Cbor` est un corps de requête ou de réponse, avec le type de média
+ `application/cbor`, et un corps est analysé en retour à son arrivée. Une API binaire se consomme
+ et se propose donc avec le même code qu'une [API JSON](json.md#json-over-http). Seul le type du
+ format diffère.

- ### Related
+ ### Voir aussi

- - [Binary data](binary-data.md) describes the bytes which CBOR is written in.
+ - [Données binaires](binary-data.md) décrit les octets dans lesquels CBOR est écrit.
