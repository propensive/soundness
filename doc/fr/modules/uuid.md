- ## UUIDs
+ ## UUID

- ### About
+ ### Présentation

- A [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) is a 128-bit identifier,
- unique enough to assign without coordination. It is represented as a `Uuid` value. A fresh one
- is generated, a known one is written as a literal which the compiler checks, and one is parsed
- from text with a typed error on failure. Its bytes and bits are available when they are needed.
+ Un [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) est un identifiant de
+ 128 bits, assez unique pour être attribué sans coordination. Il est représenté par une valeur
+ `Uuid`. On en génère un nouveau, on écrit un UUID connu comme un littéral que le compilateur
+ vérifie, et on en analyse un à partir d'un texte, avec une erreur typée en cas d'échec. Ses
+ octets et ses bits sont disponibles quand on en a besoin.

- ### On UUIDs
+ ### À propos des UUID

- A UUID is almost always carried as a string, and a string is an unreliable home for it. A
- literal with a wrong digit, or a value which is not a UUID at all, cannot be told from a valid
- one until something parses it and fails. Nothing in the type marks a particular string as a
- well-formed identifier.
+ Un UUID est presque toujours transporté sous forme de chaîne, et une chaîne est un mauvais
+ logement pour lui. Un littéral avec un chiffre erroné, ou une valeur qui n'est pas du tout un
+ UUID, ne se distingue d'un UUID valide que lorsque quelque chose l'analyse et échoue. Rien dans
+ le type ne signale qu'une chaîne donnée est un identifiant bien formé.

- A `Uuid` is a value which is known to be well-formed. A literal is validated at [compiletime].
- Parsing at [runtime] reports a typed `Uuid.Error`, and never returns a broken value. A UUID
- which cannot be constructed malformed is
- [safety by construction](../../philosophy/safety-by-construction.md) in its simplest form.
+ Un `Uuid` est une valeur dont on sait qu'elle est bien formée. Un littéral est validé
+ [à la compilation][compilation]. L'analyse [à l'exécution][exécution] signale une
+ `Uuid.Error` typée, et ne renvoie jamais une valeur invalide. Un UUID qui ne peut pas être
+ construit mal formé est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) sous sa forme la plus
+ simple.

- Everything comes from the `soundness` package. The second import lets the samples print their
- results:
+ Tout vient du package `soundness`. Le second import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- The operations, each covered below:
+ Les opérations, chacune décrite ci-dessous :

```tabular
- column  Operation
- column  Gives
- row  `Uuid()`  a fresh random identifier
- row  `uuid"…"`  a literal identifier, checked as the code compiles
- row  `Uuid.parse(text)`  the identifier, or a raised `Uuid.Error`
- row  `Uuid.extract(text)`  an `Optional` identifier; `Uuid(id)` is the same as a pattern
- row  `id.text`  the canonical text
- row  `id.in[Data]`  the sixteen bytes, the most significant half first
- row  `id.msb`, `id.lsb`  the most and least significant 64 bits
- row  `a ^ b`, `~a`  the exclusive-or of two identifiers, and the inverse of one
+ column  Opération
+ column  Donne
+ row  `Uuid()`  un nouvel identifiant aléatoire
+ row  `uuid"…"`  un identifiant littéral, vérifié à la compilation du code
+ row  `Uuid.parse(text)`  l'identifiant, ou une `Uuid.Error` levée
+ row  `Uuid.extract(text)`  un identifiant `Optional` ; `Uuid(id)` fait de même dans un motif
+ row  `id.text`  le texte canonique
+ row  `id.in[Data]`  les seize octets, la moitié de poids fort en premier
+ row  `id.msb`, `id.lsb`  les 64 bits de poids fort et de poids faible
+ row  `a ^ b`, `~a`  le ou exclusif de deux identifiants, et l'inverse d'un identifiant
```

- ### Generating
+ ### Génération

- `Uuid()` produces a fresh random identifier, different each time:
+ `Uuid()` produit un nouvel identifiant aléatoire, différent à chaque fois :

```scala
val fresh = Uuid()
```

- ### Literals
+ ### Littéraux

- The `uuid"…"` [interpolator] writes a known identifier, and checks it at compiletime:
+ L'[interpolateur] `uuid"…"` écrit un identifiant connu, et le vérifie à la compilation :

```scala
val id = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
```

- A malformed literal does not compile:
+ Un littéral mal formé ne compile pas :

```scala fuel
compile
errors show
##
uuid"a0cb16f0-d41e-4c28-862f-bd6164bbccXY"
```

- ### Parsing
+ ### Analyse

- Text which arrives at runtime may not be a UUID, so `Uuid.parse` raises a `Uuid.Error` on
- failure, which the [strategy] in scope handles:
+ Un texte reçu à l'exécution n'est peut-être pas un UUID ; `Uuid.parse` lève donc une
+ `Uuid.Error` en cas d'échec, que la [stratégie] dans la portée traite :

```scala
import strategies.throwUnsafely
```

```scala fuel
run
##
Out.println(Uuid.parse(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c").text)
```

```scala fuel
run
errors show
##
Uuid.parse(t"not-a-uuid")
```

- `Uuid.extract` gives an [optional](optional-values.md) value instead, and the same extractor
- serves in a pattern:
+ `Uuid.extract` donne plutôt une valeur [optionnelle](optional-values.md), et le même extracteur
+ sert dans un motif :

```scala
def identify(text: Text): Uuid = text match
  case Uuid(id) => id
  case _        => uuid"00000000-0000-0000-0000-000000000000"
```

```scala fuel
run
##
Out.println(Uuid.extract(t"not-a-uuid").inspect)
Out.println(identify(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c").text)
Out.println(identify(t"not-a-uuid").text)
```

- `as[Uuid]` [decodes](decoding.md#decoding-a-value) text in the same way, so a `Uuid` field in
- a case class is read from [JSON](json.md) or a
- [command-line parameter](cli.md) with no further code.
+ `as[Uuid]` [décode](decoding.md#decoding-a-value) un texte de la même manière, si bien qu'un
+ champ `Uuid` d'une case class se lit depuis du [JSON](json.md) ou un
+ [paramètre de ligne de commande](cli.md) sans code supplémentaire.

- ### Rendering and bytes
+ ### Rendu et octets

- `text` gives a UUID's canonical text. Its sixteen bytes and its two 64-bit halves are also
- available:
+ `text` donne le texte canonique d'un UUID. Ses seize octets et ses deux moitiés de 64 bits sont
+ aussi disponibles :

```scala fuel
run
##
Out.println(id.text)
Out.println(id.in[Data].length.show)
Out.println(id.msb.show)
Out.println(id.lsb.show)
```

- ### Combining
+ ### Combinaison

- Two identifiers combine with exclusive-or, and one is inverted with `~`. These are for the
- occasions when a UUID is treated as the bit pattern it is, such as deriving a stable identifier
- from two others:
+ Deux identifiants se combinent par ou exclusif, et un identifiant s'inverse avec `~`. Cela sert
+ dans les cas où un UUID est traité comme le motif de bits qu'il est, par exemple pour dériver
+ un identifiant stable de deux autres :

```scala
val other = uuid"11111111-2222-3333-4444-555555555555"
```

```scala fuel
run
##
Out.println((id ^ other).text)
Out.println((~id).text)
```

- [Hashing](hashing.md) derives an identifier from content, and [Randomness](randomness.md)
- controls the source which `Uuid()` draws from.
+ Le [hachage](hashing.md) dérive un identifiant d'un contenu, et [Randomness](randomness.md)
+ contrôle la source à laquelle `Uuid()` puise.
