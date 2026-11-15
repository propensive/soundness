- ## JSON
+ ## JSON

- ### About
+ ### Présentation

- [JSON](https://en.wikipedia.org/wiki/JSON) text parses into a `Json` value, and a `Json` value
- converts to and from ordinary Scala types. A case class needs no annotation and no hand-written
- codec: its encoder and decoder are [derived][derivation] from its fields.
+ Un texte [JSON](https://en.wikipedia.org/wiki/JSON) s'analyse en une valeur `Json`, et une valeur
+ `Json` se convertit vers et depuis les types Scala ordinaires. Une case class n'a besoin
+ d'aucune annotation ni d'aucun codec écrit à la main : son encodeur et son décodeur sont
+ [dérivés][dérivation] de ses champs.

- Every conversion is checked. Asking a number for a string, or a document for a field it lacks,
- raises a typed error which says what was wrong and where. It never yields a `null`, a default,
- or a wrong answer.
+ Chaque conversion est vérifiée. Demander une chaîne à un nombre, ou à un document un champ qu'il
+ n'a pas, lève une erreur typée qui dit ce qui n'allait pas et où. On n'obtient jamais un `null`,
+ une valeur par défaut ou une réponse fausse.

- A document can also be validated against a schema derived from a Scala type. Once it is
- validated, its fields can be navigated with the compiler checking each step.
+ Un document peut aussi être validé par rapport à un schéma dérivé d'un type Scala. Une fois
+ validé, on peut parcourir ses champs, le compilateur vérifiant chaque étape.

- ### On JSON
+ ### À propos de JSON

- Most libraries treat JSON loosely. A parsed document is an untyped tree, and fields are fished
- out of it by string keys. When the data does not hold what the code expects, the mismatch
- surfaces as a `null`, a failed cast or a silent default, often far from where it began.
+ La plupart des bibliothèques traitent JSON de manière lâche. Un document analysé est un arbre
+ non typé, dont on extrait les champs par des clés sous forme de chaînes. Lorsque les données ne
+ contiennent pas ce que le code attend, l'incohérence se manifeste par un `null`, un cast raté
+ ou une valeur par défaut silencieuse, souvent loin de son origine.

- Here the structure and the types stay together:
+ Ici, la structure et les types restent ensemble :

- - Parsing gives a `Json` value, which is [immutable]: every operation on it returns a new value,
-   in keeping with [immutability](../../philosophy/immutability.md).
- - Converting between `Json` and a Scala type is done by an encoder or decoder which the compiler
-   derives from the type's shape. A case class becomes an object, and an enumeration becomes a
-   tagged union, so there is nothing to write and nothing to keep in step by hand.
- - A conversion that cannot be made raises a `Json.Error`. The error names its reason and, where
-   it can, the position in the document.
- - The same derivation produces a schema. A document checked against the schema can then be read
-   with the compiler rejecting any path the schema does not allow.
+ - L'analyse donne une valeur `Json`, qui est [immuable] : chaque opération sur elle renvoie une
+   nouvelle valeur, conformément à l'[immuabilité](../../philosophy/immutability.md).
+ - La conversion entre `Json` et un type Scala est réalisée par un encodeur ou un décodeur que le
+   compilateur dérive de la forme du type. Une case class devient un objet, et une énumération
+   devient une union étiquetée ; il n'y a donc rien à écrire ni rien à maintenir en phase à la
+   main.
+ - Une conversion impossible lève une `Json.Error`. L'erreur nomme sa raison et, lorsque c'est
+   possible, la position dans le document.
+ - La même dérivation produit un schéma. Un document vérifié par rapport au schéma peut ensuite
+   être lu, le compilateur rejetant tout chemin que le schéma n'autorise pas.

- The sections below begin with the everyday work of parsing, decoding and encoding. Schemas,
- validation, streaming and the faster direct codecs come later, and each can be read on its own.
+ Les sections suivantes commencent par le travail quotidien d'analyse, de décodage et
+ d'encodage. Les schémas, la validation, le streaming et les codecs directs plus rapides
+ viennent ensuite, et chacun peut se lire séparément.

- Everything comes from the `soundness` package. Working with JSON also depends on a few choices,
- each made by importing a [given] from a [choice package]:
+ Tout provient du package `soundness`. Travailler avec JSON dépend aussi de quelques choix,
+ chacun fait en important un [given] depuis un [package de choix] :

```scala
import soundness.*

import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

```tabular
- column  Import
- column  Decides
- row  `codepages.utf8Codepage`  how JSON text is written as bytes
- row  `errorDiagnostics.stackTracesDiagnostics`  that an error records a stack trace
- row  `formatting.compactJsonFormatting`  that JSON is written without whitespace; [Formatting](#formatting) has the alternative
- row  `strategies.throwUnsafely`  that an error is thrown as an exception; [errors](errors.md#choosing-a-strategy) has the other strategies
- row  `stdios.javaLangSystemStdio`  where the samples print their results
+ column  Import
+ column  Décide
+ row  `codepages.utf8Codepage`  comment le texte JSON s'écrit en octets
+ row  `errorDiagnostics.stackTracesDiagnostics`  qu'une erreur enregistre une trace de pile
+ row  `formatting.compactJsonFormatting`  que JSON s'écrit sans espaces ; [Formatage](#formatting) présente l'alternative
+ row  `strategies.throwUnsafely`  qu'une erreur est levée comme exception ; [erreurs](errors.md#choosing-a-strategy) présente les autres stratégies
+ row  `stdios.javaLangSystemStdio`  où les exemples affichent leurs résultats
```

- ### Parsing
+ ### Analyse

- `read[Json]` parses any source of text into a `Json` value:
+ `read[Json]` analyse toute source de texte en une valeur `Json` :

```scala
val document = t"""{"name": "Hamlet", "age": 30}""".read[Json]
```

- `show` writes a `Json` value back out as text, in the formatting in scope:
+ `show` réécrit une valeur `Json` sous forme de texte, dans le formatage présent dans la portée :

```scala fuel
run
##
Out.println(document.show)
```

- Parsing fails on malformed input, and the failure goes to the [strategy] in scope. Under
- `throwUnsafely`, that means an exception which says what the parser found and where:
+ L'analyse échoue sur une entrée mal formée, et l'échec est confié à la [stratégie] dans la
+ portée. Sous `throwUnsafely`, cela signifie une exception qui dit ce que l'analyseur a trouvé et
+ où :

```scala fuel
run
errors show
##
t"""{"name": "Hamlet", "age": }""".read[Json]
```

- ### Reading values
+ ### Lecture des valeurs

- `as` converts a `Json` value to a Scala type. Primitive types, collections and maps are all read
- directly:
+ `as` convertit une valeur `Json` en un type Scala. Les types primitifs, les collections et les
+ maps se lisent tous directement :

```scala fuel
run
##
Out.println(t"42".read[Json].as[Int].show)
Out.println(t"3.14".read[Json].as[Double].inspect)
Out.println(t""""hello"""".read[Json].as[Text])
Out.println(t"true".read[Json].as[Boolean].inspect)
Out.println(t"[1, 2, 3]".read[Json].as[List[Int]].inspect)
Out.println(t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]].inspect)
```

- JSON's `null` reads as `Unit`, and as the absent value of an [optional] type.
+ Le `null` de JSON se lit comme `Unit`, et comme la valeur absente d'un type [optionnel].

- Asking for the wrong type is an error, not a conversion. A string is never quietly turned into a
- number:
+ Demander le mauvais type est une erreur, pas une conversion. Une chaîne n'est jamais
+ discrètement transformée en nombre :

```scala fuel
run
errors show
##
t""""hello"""".read[Json].as[Int]
```

- ### Case classes and enumerations
+ ### Case classes et énumérations

- A case class takes part in JSON as it stands. Its decoder is derived from its fields, so an
- object reads into a value:
+ Une case class participe à JSON telle quelle. Son décodeur est dérivé de ses champs, de sorte
+ qu'un objet se lit en une valeur :

```scala
case class Person(name: Text, age: Int)

val horatio = t"""{"name": "Horatio", "age": 32}""".read[Json].as[Person]
```

```scala fuel
run
##
Out.println(horatio.inspect)
```

- Its encoder is derived in the same way. `in[Json]` encodes a value as `Json`:
+ Son encodeur est dérivé de la même manière. `in[Json]` encode une valeur en `Json` :

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).in[Json].show)
```

- Nested case classes, and collections of them, derive in turn, so a whole structure converts in
- one step:
+ Les case classes imbriquées, et leurs collections, se dérivent à leur tour, de sorte qu'une
+ structure entière se convertit en une seule étape :

```scala
case class Troupe(players: List[Person], director: Person, prompter: Option[Person])

val troupe =
  t"""{"players":[{"name":"Bottom","age":35}],"director":{"name":"Quince","age":40}}"""
  . read[Troupe in Json]
```

```scala fuel
run
##
Out.println(troupe.inspect)
```

- `read[Troupe in Json]` parses and decodes together. `in`, `as` and `read` are the same vocabulary
- for every format, and between them they give each route between text, `Json` and a Scala type:
+ `read[Troupe in Json]` analyse et décode ensemble. `in`, `as` et `read` forment le même
+ vocabulaire pour chaque format, et à eux trois ils couvrent chaque chemin entre texte, `Json`
+ et type Scala :

```tabular
- column  Expression
- column  Does
- column  Yields
- row  `value.in[Json]`  encodes a value  `Json`
- row  `json.as[Type]`  decodes a `Json` value  `Type`
- row  `text.read[Json]`  parses text  `Json`
- row  `text.read[Type in Json]`  parses and decodes in one step  `Type`
- row  `value.in[Text over Json]`  encodes and renders  `Text`
- row  `value.in[Data over Json]`  encodes and renders as bytes  `Data`
+ column  Expression
+ column  Fait
+ column  Produit
+ row  `value.in[Json]`  encode une valeur  `Json`
+ row  `json.as[Type]`  décode une valeur `Json`  `Type`
+ row  `text.read[Json]`  analyse du texte  `Json`
+ row  `text.read[Type in Json]`  analyse et décode en une étape  `Type`
+ row  `value.in[Text over Json]`  encode et rend  `Text`
+ row  `value.in[Data over Json]`  encode et rend en octets  `Data`
```

- `in[Text over Json]` writes in the `Json.Formatting` in scope, exactly as `show` does, and
- `in[Data over Json]` writes the same text as bytes in the `Codepage` in scope:
+ `in[Text over Json]` écrit dans le `Json.Formatting` présent dans la portée, exactement comme
+ `show`, et `in[Data over Json]` écrit le même texte en octets dans le `Codepage` présent dans la
+ portée :

```scala
val written: Text = Person(t"Hamlet", 30).in[Text over Json]
```

```scala fuel
run
##
Out.println(written)
```

- [Derivation](derivation.md#when-derivation-fails) explains what happens when a field's type has
- no decoder, and
- [Decoding](decoding.md) describes the decoders that turn a single text value, such as a JSON
- string, into a type of one's own.
+ [Dérivation](derivation.md#when-derivation-fails) explique ce qui se passe lorsque le type d'un
+ champ n'a pas de décodeur, et
+ [Décodage](decoding.md) décrit les décodeurs qui transforment une valeur textuelle unique, comme
+ une chaîne JSON, en un type à soi.

- ### Optional fields and defaults
+ ### Champs optionnels et valeurs par défaut

- A field typed as `Optional` is left out of the output when it is absent, and reads as `Unset`
- when the document omits it. `Option` behaves the same way, with `None`:
+ Un champ typé `Optional` est omis de la sortie lorsqu'il est absent, et se lit comme `Unset`
+ lorsque le document l'omet. `Option` se comporte de la même manière, avec `None` :

```scala
case class Profile(name: Text, age: Optional[Int])
```

```scala fuel
run
##
Out.println(Profile(t"Fool", Unset).in[Json].show)
Out.println(t"""{"name": "Fool"}""".read[Json].as[Profile].inspect)
```

- A field with a default value takes that default when the document omits it:
+ Un champ doté d'une valeur par défaut prend cette valeur lorsque le document l'omet :

```scala
case class Account(name: Text, age: Int = 18)
```

```scala fuel
run
##
Out.println(t"""{"name": "Fool"}""".read[Json].as[Account].inspect)
```

- A field which is neither optional nor defaulted is required. A document without it does not
- decode:
+ Un champ ni optionnel ni doté d'une valeur par défaut est obligatoire. Un document qui ne le
+ contient pas ne se décode pas :

```scala fuel
run
errors show
##
t"""{"age": 40}""".read[Json].as[Person]
```

- [Optional values](optional-values.md) describes `Optional` and `Unset` in full.
+ [Valeurs optionnelles](optional-values.md) décrit `Optional` et `Unset` en détail.

- #### Controlling optionality
+ #### Contrôler l'optionalité

- An `Optional` field answers three questions. Each answer has a default, which a given from the
- `optionalityOptions` package overrides:
+ Un champ `Optional` répond à trois questions. Chaque réponse a une valeur par défaut, qu'un given
+ du package `optionalityOptions` remplace :

```tabular
- column  A key which is
- column  Reads as
- column  Unless
- row  omitted from the document  `Unset`  `strictJsonAbsence` makes it an error
- row  an explicit null  `Unset`  `strictJsonNulls` hands it to the inner decoder
- row  present but rejected by the inner decoder  an error  `lenientJsonFaults` reads it as `Unset`
+ column  Une clé qui est
+ column  Se lit comme
+ column  Sauf si
+ row  omise du document  `Unset`  `strictJsonAbsence` en fait une erreur
+ row  un null explicite  `Unset`  `strictJsonNulls` la confie au décodeur interne
+ row  présente mais rejetée par le décodeur interne  une erreur  `lenientJsonFaults` la lit comme `Unset`
```

- A strict absence raises the reason `Absent`. A strict null is rejected by the inner decoder, as
- it would be for any other field. A lenient fault is read as `Unset` and reports nothing. Each is
- a separate given, so one can be varied alone:
+ Une absence stricte lève la raison `Absent`. Un null strict est rejeté par le décodeur interne,
+ comme il le serait pour tout autre champ. Un défaut toléré se lit comme `Unset` et ne signale
+ rien. Chacun est un given distinct, de sorte qu'on peut en faire varier un seul :

```scala
val strict =
  import optionalityOptions.strictJsonAbsence
  capture[Json.Error](t"""{"name": "Fool"}""".read[Json].as[Profile]).reason

val lenient =
  import optionalityOptions.lenientJsonFaults
  t"""{"name": "Fool", "age": "old"}""".read[Json].as[Profile].age
```

```scala fuel
run
##
Out.println(strict.inspect)
Out.println(lenient.inspect)
```

- `Option` fields follow the same three policies, and give `None`. A declared default of `Unset`
- cannot be told apart from no default, so it does not satisfy a strict absence.
+ Les champs `Option` suivent les trois mêmes politiques, et donnent `None`. Une valeur par
+ défaut déclarée de `Unset` ne se distingue pas d'une absence de valeur par défaut, elle ne
+ satisfait donc pas une absence stricte.

- The [direct parsers](#skipping-the-tree) honor the policies too, with one limit. They tolerate
- a lenient fault only when the value in the document is of the wrong *kind*, such as a string
- where a number was expected, because the parser sees the kind before reading the value and can
- skip it whole. A value of the right kind which fails part-way, such as a number too large for an
- `Int`, has already been partly consumed, so it still raises.
+ Les [analyseurs directs](#skipping-the-tree) respectent aussi ces politiques, avec une limite.
+ Ils ne tolèrent un défaut que lorsque la valeur dans le document est de la mauvaise *nature*,
+ par exemple une chaîne là où un nombre était attendu, car l'analyseur voit la nature avant de
+ lire la valeur et peut la sauter en entier. Une valeur de la bonne nature qui échoue en cours
+ de route, comme un nombre trop grand pour un `Int`, a déjà été en partie consommée, et lève donc
+ quand même une erreur.

- ### Renaming fields
+ ### Renommer des champs

- A field's name in Scala need not match its key in JSON. The `@name` annotation gives the key to
- use. `@name[Json]` confines the rename to JSON, where another format may want a different key:
+ Le nom d'un champ en Scala n'a pas à correspondre à sa clé en JSON. L'annotation `@name` donne
+ la clé à utiliser. `@name[Json]` limite le renommage à JSON, là où un autre format pourrait
+ vouloir une clé différente :

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
```

```scala fuel
run
##
Out.println(Record(t"Juliet", 1582).in[Json].show)
```

- ### Tagged unions
+ ### Unions étiquetées

- An enumeration or sealed hierarchy encodes as an object with an extra *discriminator* field,
- which names the case. The key that holds the discriminator is chosen by import, and is `kind`
- here:
+ Une énumération ou une hiérarchie scellée s'encode en un objet doté d'un champ supplémentaire,
+ le *discriminant*, qui nomme le cas. La clé qui contient le discriminant se choisit par import,
+ et c'est `kind` ici :

```scala
import discriminables.jsonByKindDiscriminable

enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

val circle: Shape = Shape.Circle(1.0)
```

```scala fuel
run
##
Out.println(circle.in[Json].show)
```

- Decoding reads the discriminator and builds the right case:
+ Le décodage lit le discriminant et construit le bon cas :

```scala fuel
run
##
Out.println(t"""{"side":2.0,"kind":"Square"}""".read[Json].as[Shape].inspect)
```

- `@name` renames a case's discriminator value just as it renames a field, so the name in the
- document and the name in Scala can differ.
+ `@name` renomme la valeur du discriminant d'un cas exactement comme il renomme un champ, de
+ sorte que le nom dans le document et le nom en Scala peuvent différer.

- A discriminator beside the case's own fields is only one convention in use. The others are also
- chosen by a `Discriminable` given, not by writing a codec. A *wrapper* puts the whole variant
- inside an object with a single key, named for the case. An *envelope* names one field for the
- tag and another for the payload:
+ Un discriminant à côté des champs propres du cas n'est qu'une des conventions en usage. Les
+ autres se choisissent aussi par un given `Discriminable`, et non en écrivant un codec. Une
+ *enveloppe simple* place toute la variante dans un objet à clé unique, nommée d'après le cas.
+ Une *enveloppe* nomme un champ pour le tag et un autre pour la charge utile :

```scala fuel
alternative Discriminator field
  code
      circle.in[Json].show
alternative Wrapper
  code
      given Shape is Discriminable in Json = Json.DiscriminantWrapper()

      circle.in[Json].show
alternative Envelope
  code
      given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")

      circle.in[Json].show
##
```

```tabular
- column  Convention
- column  `Shape.Circle(1.0)` is written as
- row  Discriminator field  `{"radius":1.0,"kind":"Circle"}`
- row  Wrapper  `{"Circle":{"radius":1.0}}`
- row  Envelope  `{"type":"Circle","value":{"radius":1.0}}`
+ column  Convention
+ column  `Shape.Circle(1.0)` s'écrit
+ row  Champ discriminant  `{"radius":1.0,"kind":"Circle"}`
+ row  Enveloppe simple  `{"Circle":{"radius":1.0}}`
+ row  Enveloppe  `{"type":"Circle","value":{"radius":1.0}}`
```

- Each convention works through the tree and through the [direct parsers](#skipping-the-tree). A
- direct parser does not need the tag to come first: a document whose `type` field follows its
- `value` reads just as well. A wrapper object with more than one key is not a valid variant, and
- raises `Json.Error.Reason.Absent` instead of guessing which key was meant.
+ Chaque convention fonctionne avec l'arbre et avec les [analyseurs directs](#skipping-the-tree).
+ Un analyseur direct n'a pas besoin que le tag vienne en premier : un document dont le champ
+ `type` suit son `value` se lit tout aussi bien. Un objet d'enveloppe simple ayant plus d'une clé
+ n'est pas une variante valide, et lève `Json.Error.Reason.Absent` plutôt que de deviner quelle
+ clé était voulue.

- Anything more unusual is a `Discriminable` written by hand. It has three methods, which say how
- to attach a tag, how to read one, and how to reach the variant. Codecs derived from it work
- unchanged.
+ Tout ce qui est plus inhabituel est un `Discriminable` écrit à la main. Il a trois méthodes, qui
+ disent comment attacher un tag, comment en lire un, et comment atteindre la variante.
+ Les codecs qui en sont dérivés fonctionnent sans changement.

- ### Building JSON
+ ### Construire du JSON

- A `Json` value can also be assembled directly, without a case class. `Json.make` builds an
- object from named arguments, each of them a `Json`:
+ Une valeur `Json` peut aussi être assemblée directement, sans case class. `Json.make` construit
+ un objet à partir d'arguments nommés, chacun étant un `Json` :

```scala fuel
run
##
Out.println(Json.make(a = 1.in[Json], b = t"two".in[Json], c = true.in[Json]).show)
```

- The `j"…"` [interpolator] writes JSON literally, and its syntax is checked at [compiletime]. A
- substitution in a value's place encodes that value. One followed by `*` splices a collection
- into an array, and one in a field's place merges a map of fields into an object:
+ L'[interpolateur] `j"…"` écrit du JSON littéralement, et sa syntaxe est vérifiée
+ [à la compilation][compilation]. Une substitution à la place d'une valeur encode cette valeur.
+ Une substitution suivie de `*` insère une collection dans un tableau, et une substitution à la
+ place d'un champ fusionne une map de champs dans un objet :

```scala
val x = 42
val xs = List(2, 3, 4)
val rest: Map[Text, Json] = Map(t"b" -> 2.in[Json], t"c" -> 3.in[Json])
```

```scala fuel
run
##
Out.println(j"""{"a": $x}""".show)
Out.println(j"""[1, $xs*]""".show)
Out.println(j"""{"a": 1, $rest}""".show)
```

- A malformed literal is rejected where it is written. The error points at the character at
- fault, not at the whole expression:
+ Un littéral mal formé est rejeté là où il est écrit. L'erreur désigne le caractère fautif, et
+ non l'expression entière :

```scala fuel
compile
errors show
##
j""" {"a": 1, } """
```

- ### Matching JSON
+ ### Filtrage de JSON

- The same `j"…"` interpolator is a pattern. In a `match` it takes a document apart, binding each
- substitution to the piece found in its place. A document of another shape falls through to the
- next case:
+ Le même interpolateur `j"…"` est un motif. Dans un `match`, il décompose un document, en liant
+ chaque substitution au morceau trouvé à sa place. Un document d'une autre forme passe au cas
+ suivant :

```scala
val answer = t"""{"a": 42}""".read[Json] match
  case j"""{"a": $a}""" => a.as[Int]
  case _                => -1
```

- An array pattern matches by length, and a trailing `*` captures the remaining elements:
+ Un motif de tableau filtre selon la longueur, et un `*` final capture les éléments restants :

```scala
val split = t"""[1, 2, 3, 4]""".read[Json] match
  case j"""[$head, $tail*]""" => (head.as[Int], tail.as[List[Int]])
  case _                      => (0, Nil)
```

```scala fuel
run
##
Out.println(answer.show)
Out.println(split.inspect)
```

- Each kind of substitution has a meaning as an expression and as a pattern:
+ Chaque sorte de substitution a un sens en tant qu'expression et en tant que motif :

```tabular
- column  Substitution
- column  In an expression
- column  In a pattern
- row  `$x` in a value's place  the value, encoded as `Json`  binds the value there, as `Json`
- row  `$xs*` at the end of an array  splices a collection's elements  binds the remaining elements, as an array
- row  `$rest` in a field's place  merges a `Map[Text, Json]` of fields  binds the other fields, as an object
- row  `$x` inside a string  the value, encoded as `Text`  not supported
+ column  Substitution
+ column  Dans une expression
+ column  Dans un motif
+ row  `$x` à la place d'une valeur  la valeur, encodée en `Json`  lie la valeur à cet endroit, en `Json`
+ row  `$xs*` à la fin d'un tableau  insère les éléments d'une collection  lie les éléments restants, en tableau
+ row  `$rest` à la place d'un champ  fusionne une `Map[Text, Json]` de champs  lie les autres champs, en objet
+ row  `$x` dans une chaîne  la valeur, encodée en `Text`  non pris en charge
```

- [Interpolation](interpolation.md) describes how interpolators like `j"…"` are built.
+ [Interpolation](interpolation.md) décrit comment sont construits les interpolateurs comme
+ `j"…"`.

- ### Navigating
+ ### Navigation

- Applying a `Json` value to a key reaches a field, and applying it to an index reaches an array
- element. The steps chain:
+ Appliquer une valeur `Json` à une clé atteint un champ, et l'appliquer à un indice atteint un
+ élément de tableau. Les étapes s'enchaînent :

```scala
val data = t"""{"a": {"b": {"c": 42}}}""".read[Json]
```

```scala fuel
run
##
Out.println(data(t"a")(t"b")(t"c").as[Int].show)
Out.println(t"[10, 20, 30]".read[Json](1).as[Int].show)
```

- Reaching for a field that is not there is not itself an error. It gives an absent `Json`, which
- decodes to `Unset` when an optional type is asked for, and raises only when a value is required:
+ Chercher un champ absent n'est pas en soi une erreur. On obtient un `Json` absent, qui se décode
+ en `Unset` lorsqu'on demande un type optionnel, et ne lève une erreur que lorsqu'une valeur est
+ exigée :

```scala
val person = t"""{"name": "Hamlet"}""".read[Json]
```

```scala fuel
run
##
Out.println(person(t"missing").as[Optional[Int]].inspect)
```

```scala fuel
run
errors show
##
person(t"missing").as[Int]
```

- #### Fields as members
+ #### Les champs comme membres

- The compiler knows nothing about which fields a `Json` value has, or even whether it is an
- object. Often the programmer knows more, and would like to write `person.name`. That syntax
- gives up checking, since a misspelled field is found only at [runtime], so it is not available
- by default:
+ Le compilateur ne sait rien des champs que possède une valeur `Json`, ni même si c'est un objet.
+ Souvent le programmeur en sait davantage, et aimerait écrire `person.name`. Cette syntaxe
+ renonce à la vérification, puisqu'un champ mal orthographié n'est découvert
+ qu'[à l'exécution][exécution] ; elle n'est donc pas disponible par défaut :

```scala fuel
compile
errors show
##
person.name
```

- Importing `dynamicAccess.dynamicJson` permits it for the rest of the scope:
+ Importer `dynamicAccess.dynamicJson` l'autorise pour le reste de la portée :

```scala
import dynamicAccess.dynamicJson
```

```scala fuel
run
##
Out.println(person.name.as[Text])
```

- The import grants a permission, `Json is Dynamical`. The same permission can be confined to one
- block with `dynamically`, which names the formats it covers:
+ L'import accorde une permission, `Json is Dynamical`. La même permission peut être limitée à un
+ bloc avec `dynamically`, qui nomme les formats qu'elle couvre :

```tabular
- column  Form
- column  Grants dynamic access to
- row  `dynamically[Json]`  JSON alone
- row  `dynamically[Json | Yaml]`  each format in the union
- row  `dynamically`  every format with dynamic access
+ column  Forme
+ column  Accorde l'accès dynamique à
+ row  `dynamically[Json]`  JSON seul
+ row  `dynamically[Json | Yaml]`  chaque format de l'union
+ row  `dynamically`  chaque format doté d'un accès dynamique
```

```scala
val config = t"""{"server": {"port": 8080}}""".read[Json]

val port = dynamically[Json]:
  config.server.port.as[Int]
```

- A document whose shape is known in advance is better [validated](#validation) once. Its fields
- are then members which the compiler does check.
+ Un document dont la forme est connue à l'avance gagne à être [validé](#validation) une fois. Ses
+ champs sont alors des membres que le compilateur vérifie bel et bien.

- ### Updating
+ ### Mise à jour

- Assigning to a field through dynamic access gives a new document. The original is unchanged,
- since a `Json` value is immutable. Assigning a value adds or replaces a field, and assigning
- `Unset` removes one:
+ Affecter un champ par accès dynamique donne un nouveau document. L'original est inchangé,
+ puisqu'une valeur `Json` est immuable. Affecter une valeur ajoute ou remplace un champ, et
+ affecter `Unset` en supprime un :

```scala
val base = t"""{"x": 1}""".read[Json]
```

```scala fuel
run
##
Out.println((base.y = 2).show)
Out.println((base.x = Unset).show)
Out.println(base.show)
```

- A deeper update is written with a [lens](optics.md). A lens reaches through several levels, and
- may carry optics such as `Each` or `Filter` to change many elements at once:
+ Une mise à jour plus profonde s'écrit avec une [lentille](optics.md). Une lentille traverse
+ plusieurs niveaux, et peut porter des optiques comme `Each` ou `Filter` pour modifier de
+ nombreux éléments à la fois :

```scala
case class Scene(place: Text)
case class Act(name: Text, number: Int, scenes: List[Scene])
case class Play(title: Text, opening: Act)

val play = Play(t"Macbeth", Act(t"Act I", 1, List(Scene(t"A heath")))).in[Json]

val renumbered =
  import conversions.encodableToJson
  import optics.jsonLens

  play.lens(_.opening.number = 2.in[Json])
```

```scala fuel
run
##
Out.println(renumbered.show)
```

- ### Formatting
+ ### Formatage

- The output format is a given in scope. Compact formatting leaves out all whitespace, and
- indented formatting adds newlines and indentation for a human reader:
+ Le format de sortie est un given dans la portée. Le formatage compact omet tous les espaces, et
+ le formatage indenté ajoute des sauts de ligne et une indentation pour un lecteur humain :

```scala fuel
run
##
locally:
  import formatting.indentedJsonFormatting

  Out.println(troupe.in[Json].show)
```

- `show` renders the whole document into one `Text`. A large document need not be held in memory
- before it is sent, and the other forms of output deliver it a block at a time. They differ in
- where the work of serializing happens, and in what they hand out:
+ `show` rend le document entier en un seul `Text`. Un grand document n'a pas besoin d'être tenu
+ en mémoire avant d'être envoyé, et les autres formes de sortie le délivrent un bloc à la fois.
+ Elles diffèrent par l'endroit où s'effectue le travail de sérialisation, et par ce qu'elles
+ fournissent :

```tabular
- column  Form
- column  Serializes on
- column  Hands out
- row  `json.show`  the caller's thread  one `Text` of the whole document
- row  `Json.emit[Text](json, deliver)`  the caller's thread  each block as `Text`, to a function
- row  `Json.emit[Data](json, deliver)`  the caller's thread  each block as UTF-8 `Data`, to a function
- row  `Json.lend(json)`  the caller's thread  the writer's own buffer, lent block by block
- row  `Json.emit(json)`  a fiber  an iterator of `Text`
- row  `Json` as `Streamable` by `Text`  a fiber  a stream of `Text`
+ column  Forme
+ column  Sérialise sur
+ column  Fournit
+ row  `json.show`  le thread de l'appelant  un seul `Text` du document entier
+ row  `Json.emit[Text](json, deliver)`  le thread de l'appelant  chaque bloc en `Text`, à une fonction
+ row  `Json.emit[Data](json, deliver)`  le thread de l'appelant  chaque bloc en `Data` UTF-8, à une fonction
+ row  `Json.lend(json)`  le thread de l'appelant  le tampon propre de l'écrivain, prêté bloc par bloc
+ row  `Json.emit(json)`  une fibre  un itérateur de `Text`
+ row  `Json` en tant que `Streamable` par `Text`  une fibre  un flux de `Text`
```

- The push form of `Json.emit` hands each block to a function as it fills. With `emit[Data]` the
- blocks are UTF-8 bytes, encoded straight from the serializer, which suits writing to a file, a
- [socket](sockets.md) or an `OutputStream`:
+ La forme poussée de `Json.emit` confie chaque bloc à une fonction dès qu'il est rempli. Avec
+ `emit[Data]`, les blocs sont des octets UTF-8, encodés directement par le sérialiseur, ce qui
+ convient à l'écriture dans un fichier, une [socket](sockets.md) ou un `OutputStream` :

```scala fuel
typecheck
##
val json = Person(t"Hamlet", 30).in[Json]

Json.emit[Data](json, chunk => socket.write(chunk))
```

- `Json.lend` avoids even the copy into each `Data` block. It lends the consumer the writer's own
- buffer as a `Region`, together with an interval which proves that every index in it is in range.
- Both are valid only for the duration of the call, the same rule as a
- [stream's](streams.md) `lend`:
+ `Json.lend` évite même la copie dans chaque bloc `Data`. Il prête au consommateur le tampon
+ propre de l'écrivain sous forme de `Region`, avec un intervalle qui prouve que chacun de ses
+ indices est dans les bornes. Tous deux ne sont valides que pendant la durée de l'appel, la même
+ règle que pour le `lend` d'un [flux](streams.md) :

```scala fuel
typecheck
##
Json.lend(json): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))
```

- Where the consumer pulls instead, `Json.emit(json)` serializes on a fiber and hands out the text
- through an iterator. A `Json` is `Streamable` by `Text` on the same terms. Both need a
- [`supervise`](concurrency.md) block:
+ Lorsque c'est le consommateur qui tire, `Json.emit(json)` sérialise sur une fibre et fournit le
+ texte par un itérateur. Un `Json` est `Streamable` par `Text` dans les mêmes conditions. Les deux
+ exigent un bloc [`supervise`](concurrency.md) :

```scala fuel
typecheck
##
import threads.virtualThreads
import probates.cancelProbate

supervise:
  Json.emit(json).each(chunk => out.write(chunk))
```

- ### Errors
+ ### Erreurs

- A conversion that cannot be made raises a `Json.Error`, whose `reason` says what went wrong:
+ Une conversion impossible lève une `Json.Error`, dont la `reason` dit ce qui s'est mal passé :

```tabular
- column  Reason
- column  Raised for
- row  `NotType(found, primitive)`  a value of the wrong type
- row  `Absent`  a required field that is absent
- row  `OutOfRange`  a number outside the target's range
+ column  Raison
+ column  Levée pour
+ row  `NotType(found, primitive)`  une valeur du mauvais type
+ row  `Absent`  un champ obligatoire absent
+ row  `OutOfRange`  un nombre hors de l'intervalle de la cible
```

- `capture` runs an expression that is expected to fail, and gives its error, so that the reason
- can be inspected:
+ `capture` exécute une expression censée échouer, et donne son erreur, afin qu'on puisse en
+ examiner la raison :

```scala fuel
run
##
Out.println(capture[Json.Error](t""""abc"""".read[Json].as[Int]).reason.inspect)
Out.println(capture[Json.Error](t"""{}""".read[Json].as[Person]).reason.inspect)
```

- Throwing at the first error is one [strategy] among several. A method that should not throw
- declares `raises Json.Error` instead, and its caller chooses what a failure does;
- [Errors](errors.md) covers the choices.
+ Lever une exception à la première erreur n'est qu'une [stratégie] parmi d'autres. Une méthode
+ qui ne doit pas lever d'exception déclare plutôt `raises Json.Error`, et son appelant choisit ce
+ que fait un échec ; [Erreurs](errors.md) présente les choix possibles.

- #### Collecting every error
+ #### Collecter toutes les erreurs

- Under a strategy that [accumulates failures](errors.md#accumulating-failures), decoding does
- not stop at the first error. Every error is collected, and each is tagged with a
- [JSON Pointer](https://en.wikipedia.org/wiki/JSON_Pointer) to the field that failed, so all of a
- document's problems are reported at once.
+ Sous une stratégie qui [accumule les échecs](errors.md#accumulating-failures), le décodage ne
+ s'arrête pas à la première erreur. Chaque erreur est collectée, et chacune est taguée par un
+ [JSON Pointer](https://en.wikipedia.org/wiki/JSON_Pointer) vers le champ en échec, de sorte que
+ tous les problèmes d'un document sont signalés en une fois.

- The pointer comes from a *focus*, which the decoder maintains as it descends, and which the
- strategy reads through `prior`. This contact has a person and an address which are each wrong in
- two places:
+ Le pointeur provient d'un *focus*, que le décodeur maintient à mesure qu'il descend, et que la
+ stratégie lit par `prior`. Ce contact a une personne et une adresse qui sont chacune erronées à
+ deux endroits :

```scala
import dysasymptotics.linearSize

case class Address(street: Text, postcode: Text)
case class Contact(person: Person, address: Address)

case class Issues(items: List[(Text, Json.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} problems"):
  def +(pointer: Text, error: Json.Error): Issues = Issues(items :+ (pointer, error))

val damaged = t"""{"person": {"name": 1}, "address": {"street": 2}}""".read[Json]

val problems =
  Validate[Issues, [r] =>> r raises Json.Error, Json.Focus]
    ( Issues(),
      { case error: Json.Error => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
  . protect(damaged.as[Contact])
  . items
```

- Decoding yields four errors, one for each fault:
+ Le décodage produit quatre erreurs, une pour chaque défaut :

```scala fuel
run
##
problems.each: (pointer, error) =>
  Out.println(t"$pointer: ${error.reason.inspect}")
```

- Missing fields and fields of the wrong type accumulate together. An absent field contributes
- exactly one error, however many times the decoder tries to read it.
+ Les champs manquants et les champs du mauvais type s'accumulent ensemble. Un champ absent
+ contribue exactement une erreur, quel que soit le nombre de fois où le décodeur tente de le
+ lire.

- ### Equality and structure
+ ### Égalité et structure

- Two documents are equal when they mean the same thing. The order of an object's keys does not
- matter, and a whole number equals the same value written with a decimal point:
+ Deux documents sont égaux lorsqu'ils signifient la même chose. L'ordre des clés d'un objet
+ n'importe pas, et un nombre entier est égal à la même valeur écrite avec un point décimal :

```scala fuel
run
##
Out.println((t"""{"x": 1, "y": 2}""".read[Json] == t"""{"y": 2, "x": 1}""".read[Json]).inspect)
Out.println((t"5".read[Json] == t"5.0".read[Json]).inspect)
```

- `Json.unseal` exposes the underlying tree where it is needed directly. The tree has a predicate
- for each kind of value, and a `primitive` which names the kind:
+ `Json.unseal` expose l'arbre sous-jacent là où on en a directement besoin. L'arbre a un prédicat
+ pour chaque nature de valeur, et un `primitive` qui nomme la nature :

```scala fuel
run
##
Out.println(Json.unseal(t"42".read[Json]).isLong.inspect)
Out.println(Json.unseal(t""""x"""".read[Json]).primitive.inspect)
```

- The predicates are finer than the primitives. Code which cares how a number is represented can
- ask, and code which does not need not:
+ Les prédicats sont plus fins que les primitives. Le code qui se soucie de la représentation d'un
+ nombre peut la demander, et celui qui ne s'en soucie pas n'a pas à le faire :

```tabular
- column  Predicate
- column  Primitive
- column  True for
- row  `isLong`  `Number`  a number held as a `Long`
- row  `isDouble`  `Number`  a number held as a `Double`
- row  `isNumber`  `Number`  any number
- row  `isString`  `String`  a string
- row  `isBoolean`  `Boolean`  `true` or `false`
- row  `isNull`  `Null`  `null`
- row  `isObject`  `Object`  an object
- row  `isArray`  `Array`  an array
+ column  Prédicat
+ column  Primitive
+ column  Vrai pour
+ row  `isLong`  `Number`  un nombre stocké sous forme de `Long`
+ row  `isDouble`  `Number`  un nombre stocké sous forme de `Double`
+ row  `isNumber`  `Number`  tout nombre
+ row  `isString`  `String`  une chaîne
+ row  `isBoolean`  `Boolean`  `true` ou `false`
+ row  `isNull`  `Null`  `null`
+ row  `isObject`  `Object`  un objet
+ row  `isArray`  `Array`  un tableau
```

- ### Numbers and precision
+ ### Nombres et précision

- A JSON number has no limit on its precision, and most parsers quietly impose one. Here the
- *number mode* in scope decides what happens to a number too long for the parser's fast path,
- which holds about fifteen digits. A shorter number is decoded identically in every mode:
+ Un nombre JSON n'a pas de limite de précision, et la plupart des analyseurs en imposent une en
+ silence. Ici, le *mode numérique* dans la portée décide de ce qu'il advient d'un nombre trop
+ long pour le chemin rapide de l'analyseur, qui contient environ quinze chiffres. Un nombre plus
+ court est décodé de manière identique dans tous les modes :

```scala
import numberModes.fullNumberMode
```

```tabular
- column  Mode
- column  With a number too long for the fast path
- row  `fullNumberMode`  keeps every digit as a binary-coded decimal, at the cost of an allocation
- row  `doubleNumberMode`  drops the excess digits and computes a `Double` from the leading ones
- row  `bcdNumberMode`  drops the excess digits and yields the raw packed accumulator, with no allocation
+ column  Mode
+ column  Avec un nombre trop long pour le chemin rapide
+ row  `fullNumberMode`  conserve chaque chiffre en décimal codé binaire, au prix d'une allocation
+ row  `doubleNumberMode`  abandonne les chiffres excédentaires et calcule un `Double` à partir des premiers
+ row  `bcdNumberMode`  abandonne les chiffres excédentaires et produit l'accumulateur brut compacté, sans allocation
```

- `fullNumberMode` is the default, because losing digits silently is the wrong thing to do
- unasked. `doubleNumberMode` does what Jawn, Circe, Jsoniter and Jackson do. It is the mode for
- a like-for-like comparison of speed, or for data known to fit in a `Double`. The `Long` which
- `bcdNumberMode` yields holds binary-coded decimal digits, not the number's value, so it buys
- speed at the price of treating long numbers as opaque. [Numbers](numbers.md) covers the numeric
- types a value can be decoded to.
+ `fullNumberMode` est le mode par défaut, car perdre des chiffres en silence sans qu'on l'ait
+ demandé n'est pas la bonne chose à faire. `doubleNumberMode` fait ce que font Jawn, Circe,
+ Jsoniter et Jackson. C'est le mode pour une comparaison de vitesse à armes égales, ou pour des
+ données dont on sait qu'elles tiennent dans un `Double`. Le `Long` que produit `bcdNumberMode`
+ contient des chiffres en décimal codé binaire, et non la valeur du nombre ; il achète donc la
+ vitesse au prix de traiter les nombres longs comme opaques. [Nombres](numbers.md) présente les
+ types numériques vers lesquels une valeur peut être décodée.

- ### NDJSON
+ ### NDJSON

- [Newline-delimited JSON](https://en.wikipedia.org/wiki/JSON_streaming) is a stream of
- independent documents, one on each line. It needs no type of its own. `delineate` splits a
- [source](streams.md#records) into records at its line boundaries, and each record is read as
- `Json`:
+ Le [JSON délimité par des sauts de ligne](https://en.wikipedia.org/wiki/JSON_streaming) est un
+ flux de documents indépendants, un par ligne. Il n'a pas besoin de type propre. `delineate`
+ découpe une [source](streams.md#records) en enregistrements à ses limites de ligne, et chaque
+ enregistrement se lit comme `Json` :

```scala
import lineSeparation.adaptiveLinefeedLineSeparation

val numbers = t"1\n2\n3".source[Text].delineate.records.map(_.read[Json].as[Int]).to[List]
```

```scala fuel
run
##
Out.println(numbers.inspect)
```

- Each line is read independently, so the documents need not share a shape. A log of mixed
- records reads as well as a uniform one.
+ Chaque ligne est lue indépendamment, de sorte que les documents n'ont pas à partager une forme.
+ Un journal d'enregistrements hétérogènes se lit aussi bien qu'un journal uniforme.

- ### JSON Pointers
+ ### JSON Pointers

- A [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901) names a location within a
- document. The `jp"…"` interpolator writes one, and checks it at compiletime. A pointer can also
- be built a segment at a time:
+ Un [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901) nomme un emplacement dans un
+ document. L'interpolateur `jp"…"` en écrit un, et le vérifie à la compilation. Un pointeur peut
+ aussi être construit un segment à la fois :

```scala fuel
run
##
Out.println(jp"#/verona/capulet".encode)
Out.println(JsonPointer()(t"a")(t"b").encode)
```

- Within a segment, the special characters `~` and `/` are escaped as `~0` and `~1`. A malformed
- pointer does not compile, and the error points at the character at fault:
+ Dans un segment, les caractères spéciaux `~` et `/` sont échappés en `~0` et `~1`. Un pointeur
+ mal formé ne compile pas, et l'erreur désigne le caractère fautif :

```scala fuel
compile
errors show
##
jp"#/verona~2capulet"
```

- ### Source positions
+ ### Positions dans le code source

- Ordinary parsing keeps no record of where each value sat in the text, since most programs never
- ask. A program which does ask, perhaps to underline the source of an error, brings
- `parsing.trackPositions` into scope. `read` then records positions, and `locate` gives the line
- and column of the value a pointer names:
+ L'analyse ordinaire ne garde aucune trace de l'endroit où se trouvait chaque valeur dans le
+ texte, puisque la plupart des programmes ne le demandent jamais. Un programme qui le demande,
+ par exemple pour souligner l'origine d'une erreur, met `parsing.trackPositions` dans la portée.
+ `read` enregistre alors les positions, et `locate` donne la ligne et la colonne de la valeur que
+ nomme un pointeur :

```scala
import parsing.trackPositions

val tracked = t"{\n  \"a\": 42\n}".read[Json]
```

```scala fuel
run
##
Out.println(tracked.locate(JsonPointer()(t"a")).let(_.line).inspect)
```

- A position covers the whole of the value it names. A negative number's extent includes its
- minus sign, and an object or array runs from its opening bracket to its closing one. `locateKey`
- gives the position of the *key* matching the pointer's last segment, not of its value, which is
- what underlining a misspelled field name needs.
+ Une position couvre l'intégralité de la valeur qu'elle nomme. L'étendue d'un nombre négatif
+ inclut son signe moins, et un objet ou un tableau va de son crochet ouvrant à son crochet
+ fermant. `locateKey` donne la position de la *clé* correspondant au dernier segment du
+ pointeur, et non de sa valeur, ce dont on a besoin pour souligner un nom de champ mal
+ orthographié.

- A pointer which does not resolve gives `Unset`, whether it names a key the document lacks or an
- index past the end of an array. Probing a document for a position is therefore always safe.
- Under ordinary parsing, without tracking, every `locate` gives `Unset`, and a program which does
- not want positions pays nothing for them.
+ Un pointeur qui ne se résout pas donne `Unset`, qu'il nomme une clé absente du document ou un
+ indice au-delà de la fin d'un tableau. Sonder un document à la recherche d'une position est
+ donc toujours sûr. Sous l'analyse ordinaire, sans suivi, chaque `locate` donne `Unset`, et un
+ programme qui ne veut pas de positions ne paie rien pour elles.

- ### Schemas
+ ### Schémas

- A [JSON Schema](https://en.wikipedia.org/wiki/JSON_Schema) describes the shape a document is
- allowed to have. A schema is derived from a Scala type, and a schema value renders as a standard
- JSON Schema document:
+ Un [JSON Schema](https://en.wikipedia.org/wiki/JSON_Schema) décrit la forme qu'un document a le
+ droit d'avoir. Un schéma se dérive d'un type Scala, et une valeur de schéma se rend sous forme
+ de document JSON Schema standard :

```scala fuel
run
##
Out.println((JsonSchema.Integer(): JsonSchema).in[Json].show)
```

- The `@memo` annotation on a field gives it a description in the generated schema. String
- formats such as `date-time` and `email` are represented by `JsonSchema.Format`. The derived
- schema is what [validation](#validation) rests on, and [OpenAPI](openapi.md) uses the same
- schemas to describe a service.
+ L'annotation `@memo` sur un champ lui donne une description dans le schéma généré. Les formats
+ de chaîne comme `date-time` et `email` sont représentés par `JsonSchema.Format`. Le schéma
+ dérivé est ce sur quoi repose la [validation](#validation), et [OpenAPI](openapi.md) utilise les
+ mêmes schémas pour décrire un service.

- ### Validation
+ ### Validation

- `verify` checks a document against the shape a type expects. On success it gives the same
- document, now carrying that type:
+ `verify` vérifie un document par rapport à la forme qu'attend un type. En cas de succès, il
+ donne le même document, portant désormais ce type :

```scala
case class Player(name: Text, age: Int, email: Text)

val viola = t"""{"name": "Viola", "age": 20, "email": "viola@example.com"}""".read[Json]
val player = viola.verify[Player]
```

- A document which does not conform raises a `Json.Error`:
+ Un document non conforme lève une `Json.Error` :

```scala fuel
run
errors show
##
t"""{"name": "Feste"}""".read[Json].verify[Player]
```

- The verified document carries the schema's structure in its type, so its fields are reached as
- members, with full checking and without the `dynamicAccess.dynamicJson` import:
+ Le document vérifié porte la structure du schéma dans son type, de sorte qu'on atteint ses
+ champs comme des membres, avec une vérification complète et sans l'import
+ `dynamicAccess.dynamicJson` :

```scala fuel
run
##
Out.println(player.name.as[Text])
Out.println(player.as[Player].inspect)
```

- Anything the schema does not permit is a compiletime error. That includes a field which does
- not exist, an index into something which is not an array, and a field of a scalar:
+ Tout ce que le schéma n'autorise pas est une erreur à la compilation. Cela comprend un champ qui
+ n'existe pas, un indice dans quelque chose qui n'est pas un tableau, et un champ d'un scalaire :

```scala fuel
compile
errors show
##
player.nope
```

```scala fuel
compile
errors show
##
player.name(0)
```

```scala fuel
compile
errors show
##
player.name.deeper
```

- Navigation runs to any depth, through nested objects and into arrays, and each step is checked:
+ La navigation va à n'importe quelle profondeur, à travers des objets imbriqués et dans des
+ tableaux, et chaque étape est vérifiée :

```scala
case class Venue(city: Text)
case class Premiere(title: Text, venue: Venue)
case class Court(members: List[Person])

val premiere = t"""{"title": "Hamlet", "venue": {"city": "London"}}""".read[Json]

val court =
  t"""{"members": [{"name": "Hamlet", "age": 30}, {"name": "Horatio", "age": 32}]}""".read[Json]
```

```scala fuel
run
##
Out.println(premiere.verify[Premiere].venue.city.as[Text])
Out.println(court.verify[Court].members(1).name.as[Text])
```

- It is the schema which licenses this access, not a blanket permission. Reaching a field of an
- *unverified* document still needs `dynamicJson`, so the two kinds of access cannot be confused.
- A path through a document is thereby checked against the type the document is supposed to have,
- before the program runs.
+ C'est le schéma qui autorise cet accès, et non une permission générale. Atteindre un champ d'un
+ document *non vérifié* exige toujours `dynamicJson`, de sorte que les deux sortes d'accès ne
+ peuvent pas être confondues. Un chemin à travers un document est ainsi vérifié par rapport au
+ type que le document est censé avoir, avant l'exécution du programme.

- ### Overriding one field's encoder
+ ### Remplacer l'encodeur d'un seul champ

- A derived encoder uses, for each field, whichever instance is in scope for that field's type.
- A different given in scope would change every field of that type at once, which is rarely what
- one awkward field calls for. A `Specific` names a path instead, and its override applies along
- that path and nowhere else:
+ Un encodeur dérivé utilise, pour chaque champ, l'instance présente dans la portée pour le type
+ de ce champ. Un autre given dans la portée modifierait d'un coup chaque champ de ce type, ce qui
+ est rarement ce qu'exige un unique champ récalcitrant. Un `Specific` nomme plutôt un chemin, et
+ son remplacement s'applique le long de ce chemin et nulle part ailleurs :

```scala
case class Fencer(name: Text, age: Int)
case class Duel(challenger: Fencer, opponent: Fencer)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)
```

- With the `Specific` given in scope, only the opponent's name is changed:
+ Avec le given `Specific` dans la portée, seul le nom de l'adversaire est modifié :

```scala fuel
run
##
locally:
  given (Duel is Specific over Json.Encodable) =
    specifically:
      case root.opponent.name() => shout

  Out.println(Duel(Fencer(t"hamlet", 30), Fencer(t"laertes", 25)).in[Json].show)
```

- A path which ends at a collection takes an encoder for the collection. Deriving that encoder
- again, with a local given in scope, changes how the members of that one list are written without
- changing their type's encoding everywhere:
+ Un chemin qui aboutit à une collection prend un encodeur pour la collection. Dériver de nouveau
+ cet encodeur, avec un given local dans la portée, change la manière dont sont écrits les
+ membres de cette seule liste sans changer partout l'encodage de leur type :

```scala
val nameOnly: Person is Json.Encodable =
  Json.Encodable(() => Morphology.Str): person => Json(person.name)
```

```scala fuel
run
##
locally:
  given (Troupe is Specific over Json.Encodable) =
    specifically:
      case root.players() =>
        given Person is Json.Encodable = nameOnly
        summon[List[Person] is Json.Encodable]

  Out.println(troupe.in[Json].show)
```

- ### Skipping the tree
+ ### Se passer de l'arbre

- `read[Person in Json]` ordinarily parses the text into a `Json` tree, and then decodes the tree.
- A type with a `Json.Parsable` instance is instead read *directly* from the source. Its parser is
- composed for that type at compiletime, and takes the fields from the text as they arrive, so no
- tree is built. The saving is the whole intermediate structure: its allocation, its boxing and a
- second traversal.
+ `read[Person in Json]` analyse ordinairement le texte en un arbre `Json`, puis décode l'arbre.
+ Un type doté d'une instance `Json.Parsable` est au contraire lu *directement* depuis la source.
+ Son analyseur est composé pour ce type à la compilation, et prend les champs du texte à mesure
+ qu'ils arrivent, si bien qu'aucun arbre n'est construit. L'économie porte sur toute la structure
+ intermédiaire : son allocation, son boxing et un second parcours.

```scala
given personParsable: (Person is Json.Parsable) = Json.Parsable.derived
```

- In ordinary code the instance belongs in the type's companion object, where it is found without
- an import.
+ Dans du code ordinaire, l'instance a sa place dans l'objet compagnon du type, où elle est
+ trouvée sans import.

- Writing has a matching instance, `Json.Printable`. A type which has one is written directly,
- with no tree, wherever the formatting is compact. Indented formatting still renders through the
- tree. Both routes write the same text:
+ L'écriture a une instance correspondante, `Json.Printable`. Un type qui en possède une est écrit
+ directement, sans arbre, partout où le formatage est compact. Le formatage indenté passe
+ toujours par l'arbre. Les deux chemins écrivent le même texte :

```scala fuel
alternative Tree
  code
      Person(t"Hamlet", 30).in[Text over Json]
alternative Direct
  code
      val direct =
        given Person is Json.Printable = Json.Direct.derived[Person].printable
        Person(t"Hamlet", 30).in[Text over Json]
##
```

- `Json.Printable.emit` writes the text a block at a time, handing each block to a function as it
- fills.
+ `Json.Printable.emit` écrit le texte un bloc à la fois, en confiant chaque bloc à une fonction
+ dès qu'il est rempli.

- A record can generate both instances at once, into its companion, with `derives Json.Direct`.
- A single import then selects the route for every record in scope:
+ Un enregistrement peut générer les deux instances à la fois, dans son compagnon, avec
+ `derives Json.Direct`. Un seul import sélectionne alors le chemin pour chaque enregistrement
+ dans la portée :

```tabular
- column  Import
- column  For a record
- column  For any other type
- row  `codecs.direct`  a generated parser and printer  its own direct instance, else the tree
- row  `codecs.derived`  the derived codecs, even over a direct instance  the derived codecs, even over a direct instance
- row  `codecs.strict`  a generated parser and printer  its own direct instance, else a compile error naming the type
+ column  Import
+ column  Pour un enregistrement
+ column  Pour tout autre type
+ row  `codecs.direct`  un analyseur et un afficheur générés  sa propre instance directe, sinon l'arbre
+ row  `codecs.derived`  les codecs dérivés, même en présence d'une instance directe  les codecs dérivés, même en présence d'une instance directe
+ row  `codecs.strict`  un analyseur et un afficheur générés  sa propre instance directe, sinon une erreur de compilation nommant le type
```

- Under `codecs.strict` the error is reported wherever `read[Play in Json]` or `in[Text over Json]`
- would otherwise fall back to the tree. With none of the three imported, a type takes the direct
- route exactly when it has a direct instance of its own. The modes are alternatives, so import
- one at a time.
+ Sous `codecs.strict`, l'erreur est signalée partout où `read[Play in Json]` ou
+ `in[Text over Json]` se replierait sinon sur l'arbre. Si aucun des trois n'est importé, un type
+ prend le chemin direct exactement lorsqu'il possède sa propre instance directe. Les modes sont
+ des alternatives : importez-en un seul à la fois.

- The tree remains the right route for a document whose shape is not known in advance, and
- wherever the tree itself is what is wanted.
+ L'arbre reste le bon chemin pour un document dont la forme n'est pas connue à l'avance, et
+ partout où c'est l'arbre lui-même que l'on veut.

- ### Typed records from a schema
+ ### Enregistrements typés à partir d'un schéma

- Sometimes the schema arrives as a JSON Schema document, not as a Scala type. A `Json.Provider`
- reads such a schema at compiletime, and produces typed records from JSON which matches it. A
- provider object, here `Catalogue`, holds the schema, and its `record` method turns a `Json`
- value into a record. Each field of the record has the type the schema declares, and a field the
- schema does not describe is a compiletime error:
+ Il arrive que le schéma se présente sous forme de document JSON Schema, et non de type Scala.
+ Un `Json.Provider` lit un tel schéma à la compilation, et produit des enregistrements typés à
+ partir du JSON qui s'y conforme. Un objet fournisseur, ici `Catalogue`, contient le schéma, et
+ sa méthode `record` transforme une valeur `Json` en enregistrement. Chaque champ de
+ l'enregistrement a le type que déclare le schéma, et un champ que le schéma ne décrit pas est
+ une erreur à la compilation :

```scala fuel
typecheck
##
object Catalogue extends Json.Provider(t"""{
  "type": "object",
  "required": ["name", "children"],
  "properties": {
    "name": { "type": "string" },
    "email": { "type": "string", "format": "email" },
    "children": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["weight"],
        "properties": { "weight": { "type": "number", "minimum": 0 } }
      }
    }
  }
}""".read[Json])

val input = t"""{"name": "Bicycle", "children": [{"weight": 9.5}]}""".read[Json]

val record = Catalogue.record(input)
val name: Text = record.name
val weight: Optional[Double] = record.children.head.let(_.weight)
```

- The provider takes its schema as anything readable as JSON. A schema kept as a
- [classpath](classpath.md) resource is bound directly, as
- `object Catalogue extends Json.Provider(cp"/schemas/catalogue.json")`, with a `Classloader` such
- as `classloaders.threadContextClassloader` in scope, and is read as the code compiles.
+ Le fournisseur prend son schéma sous la forme de tout ce qui peut se lire comme JSON. Un schéma
+ conservé comme ressource du [classpath](classpath.md) se lie directement, sous la forme
+ `object Catalogue extends Json.Provider(cp"/schemas/catalogue.json")`, avec un `Classloader` tel
+ que `classloaders.threadContextClassloader` dans la portée, et est lu pendant la compilation du
+ code.

- Each construct in the schema has a reading in Scala:
+ Chaque construction du schéma a une lecture en Scala :

```tabular
- column  Schema construct
- column  Reads as
- row  `"type": "string"`  `Text`
- row  `"type": "integer"`  `Int`, or `Long` with `"format": "int64"`
- row  `"type": "number"`  `Double`
- row  `"type": "boolean"`  `Boolean`
- row  a property in the `required` list  its type, with a `Json.Provider.Error` where the document omits it
- row  any other property, or one whose type admits `null`  an `Optional`, `Unset` where omitted or `null`
- row  an `array` with an `items` schema  a `List` of the items' type
- row  an `object` with `properties`  a nested record
- row  `additionalProperties`, or `patternProperties` all of one type  a `Map[Text, T]`, empty where absent
- row  a `$ref` into the same document  the definition it names
- row  an `allOf` of objects  one record with all their properties
- row  an `enum` (or `const`) without a `type`  the type of its values
- row  a `oneOf` or `anyOf` whose alternatives are all strings  `Text`, an `enum` of all their values where each alternative is one
- row  a property admitting several types  the union of their readings, chosen by the kind of value found
+ column  Construction du schéma
+ column  Se lit comme
+ row  `"type": "string"`  `Text`
+ row  `"type": "integer"`  `Int`, ou `Long` avec `"format": "int64"`
+ row  `"type": "number"`  `Double`
+ row  `"type": "boolean"`  `Boolean`
+ row  une propriété de la liste `required`  son type, avec une `Json.Provider.Error` lorsque le document l'omet
+ row  toute autre propriété, ou une propriété dont le type admet `null`  un `Optional`, `Unset` lorsqu'elle est omise ou `null`
+ row  un `array` avec un schéma `items`  une `List` du type des éléments
+ row  un `object` avec des `properties`  un enregistrement imbriqué
+ row  `additionalProperties`, ou des `patternProperties` toutes d'un même type  une `Map[Text, T]`, vide en cas d'absence
+ row  un `$ref` vers le même document  la définition qu'il nomme
+ row  un `allOf` d'objets  un seul enregistrement avec toutes leurs propriétés
+ row  un `enum` (ou `const`) sans `type`  le type de ses valeurs
+ row  un `oneOf` ou un `anyOf` dont toutes les alternatives sont des chaînes  `Text`, un `enum` de toutes leurs valeurs lorsque chaque alternative en est une
+ row  une propriété admettant plusieurs types  l'union de leurs lectures, choisie selon la nature de la valeur trouvée
```

- A `$ref` may take any JSON pointer, `#/definitions/…` and `#/$defs/…` alike. A union arises
- from the alternatives of an `anyOf` or `oneOf`, and from a `type` which names several. So
- `"exec": string | object` reads as `Text | Record { … }`, and `"ignore": string | array` as
- `Text | List[Text]`, and each alternative keeps its own checks. A dictionary nests freely: a
- dictionary of lists, a list of dictionaries, an array of arrays. A property named with a hyphen
- or a keyword is reached through a backticked name, such as ``record.`content-type` ``.
+ Un `$ref` peut prendre n'importe quel pointeur JSON, `#/definitions/…` comme `#/$defs/…`. Une
+ union naît des alternatives d'un `anyOf` ou d'un `oneOf`, et d'un `type` qui en nomme plusieurs.
+ Ainsi `"exec": string | object` se lit comme `Text | Record { … }`, et
+ `"ignore": string | array` comme `Text | List[Text]`, et chaque alternative conserve ses propres
+ vérifications. Un dictionnaire s'imbrique librement : un dictionnaire de listes, une liste de
+ dictionnaires, un tableau de tableaux. Une propriété dont le nom contient un trait d'union ou
+ est un mot-clé s'atteint par un nom entre backticks, comme ``record.`content-type` ``.

- The schema's constraints are checked as each field is read, as is the value's JSON type:
+ Les contraintes du schéma sont vérifiées à la lecture de chaque champ, de même que le type JSON
+ de la valeur :

```tabular
- column  Applies to
- column  Keywords checked
- row  a string  `enum` (or `const`), `pattern`, `minLength`, `maxLength`
- row  a number  `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`
+ column  S'applique à
+ column  Mots-clés vérifiés
+ row  une chaîne  `enum` (ou `const`), `pattern`, `minLength`, `maxLength`
+ row  un nombre  `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`
```

- The exclusive bounds are accepted in draft 4's boolean form and in the later numeric form. A
- failed check raises a `Json.Provider.Error`. Every error is raised through the [tactic] of the
- code which reads the field, so `validate`, with a `venture` for each field,
- [collects every problem](errors.md#ventures-and-guards) in a document.
+ Les bornes exclusives sont acceptées sous la forme booléenne du draft 4 et sous la forme
+ numérique ultérieure. Une vérification en échec lève une `Json.Provider.Error`. Chaque erreur
+ est levée par la [tactique] du code qui lit le champ, de sorte que `validate`, avec un `venture`
+ pour chaque champ, [collecte tous les problèmes](errors.md#ventures-and-guards) d'un document.

- Some constructs cannot be given a type, and remain raw `Json` to be read by hand:
+ Certaines constructions ne peuvent pas recevoir de type, et restent du `Json` brut à lire à la
+ main :

- - alternatives which the kind of value cannot tell apart, such as two object shapes in a
-   `oneOf`, or an alternative typed as anything;
- - a `not` or an `if`;
- - an object with neither `properties` nor a value schema;
- - an array with no `items` schema, or with a tuple-form `items` list;
- - `patternProperties` of differing types;
- - a reference to another document;
- - a recursive reference, at the point of recursion, since a record type cannot contain itself.
+ - des alternatives que la nature de la valeur ne permet pas de distinguer, comme deux formes
+   d'objet dans un `oneOf`, ou une alternative typée comme n'importe quoi ;
+ - un `not` ou un `if` ;
+ - un objet sans `properties` ni schéma de valeur ;
+ - un tableau sans schéma `items`, ou avec une liste `items` sous forme de tuple ;
+ - des `patternProperties` de types différents ;
+ - une référence à un autre document ;
+ - une référence récursive, au point de récursion, puisqu'un type d'enregistrement ne peut pas se
+   contenir lui-même.

- The schema's root must be an object with properties. A root which is an array, a bare string or
- a reference to another document is a compiletime error. Where the root offers alternatives, and
- one of them is such an object, the provider reads that one.
+ La racine du schéma doit être un objet doté de propriétés. Une racine qui est un tableau, une
+ simple chaîne ou une référence à un autre document est une erreur à la compilation. Lorsque la
+ racine propose des alternatives, et que l'une d'elles est un tel objet, le fournisseur lit
+ celle-ci.

- A property whose name is a method of every JVM object, such as `notify`, `wait`, `toString` or
- `hashCode`, cannot be selected by name. It is read through `record.selectDynamic("notify")`,
- whose result is the field's reading: cast it to `Record.Field[T, Json.Provider.Error]`, and it
- is applied to the `Tactic` in scope.
+ Une propriété dont le nom est une méthode de tout objet JVM, comme `notify`, `wait`, `toString`
+ ou `hashCode`, ne peut pas être sélectionnée par son nom. Elle se lit par
+ `record.selectDynamic("notify")`, dont le résultat est la lecture du champ : convertissez-le en
+ `Record.Field[T, Json.Provider.Error]`, et il s'applique à la `Tactic` dans la portée.

- A format which names a network or identity type reads as whichever type represents that domain
- where the record is used, chosen by importing its interface:
+ Un format qui nomme un type de réseau ou d'identité se lit comme le type qui représente ce
+ domaine là où l'enregistrement est utilisé, choisi en important son interface :

```tabular
- column  Format
- column  Interface
- column  Reads as
- row  `email`, `idn-email`  `emailAddressInterfaces.soundnessEmailAddress`  `EmailAddress`
- row  `hostname`  `hostnameInterfaces.soundnessHostname`  `Hostname`
- row  `ipv4`  `ipAddressInterfaces.soundnessIpv4`  `Ipv4`
- row  `ipv6`  `ipAddressInterfaces.soundnessIpv6`  `Ipv6`
- row  `uri`, `iri`  `urlInterfaces.soundnessUrl`  `HttpUrl`
- row  `uuid`  `uuidInterfaces.soundnessUuid`  `Uuid`
+ column  Format
+ column  Interface
+ column  Se lit comme
+ row  `email`, `idn-email`  `emailAddressInterfaces.soundnessEmailAddress`  `EmailAddress`
+ row  `hostname`  `hostnameInterfaces.soundnessHostname`  `Hostname`
+ row  `ipv4`  `ipAddressInterfaces.soundnessIpv4`  `Ipv4`
+ row  `ipv6`  `ipAddressInterfaces.soundnessIpv6`  `Ipv6`
+ row  `uri`, `iri`  `urlInterfaces.soundnessUrl`  `HttpUrl`
+ row  `uuid`  `uuidInterfaces.soundnessUuid`  `Uuid`
```

- So with `emailAddressInterfaces.soundnessEmailAddress` imported, `record.email` is an
- `EmailAddress`. [Network addresses](network-addresses.md) and [UUIDs](uuid.md) describe these
- types.
+ Ainsi, avec `emailAddressInterfaces.soundnessEmailAddress` importé, `record.email` est une
+ `EmailAddress`. [Adresses réseau](network-addresses.md) et [UUID](uuid.md) décrivent ces types.

- A provider can equally produce a named tuple, `(name: Text, children: List[(weight: Double)])`,
- whose elements are all read when it is built, by declaring a `tuple` method beside `record`.
- [Records](records.md) describes both forms and how they differ.
+ Un fournisseur peut tout aussi bien produire un tuple nommé,
+ `(name: Text, children: List[(weight: Double)])`, dont tous les éléments sont lus à sa
+ construction, en déclarant une méthode `tuple` à côté de `record`. [Enregistrements](records.md)
+ décrit les deux formes et ce qui les distingue.

- ### Dates and times
+ ### Dates et heures

- The temporal types of [Time](time.md) are not encoded in JSON until an import says how. An
- `Instant` and a `Duration` each travel as a whole number of milliseconds:
+ Les types temporels de [Time](time.md) ne sont pas encodés en JSON tant qu'un import ne dit pas
+ comment. Un `Instant` et une `Duration` voyagent chacun sous forme de nombre entier de
+ millisecondes :

```scala
import chronometries.unixChronometry
import encodables.instantJsonEncodable
import decodables.durationJsonDecodable
```

```scala fuel
run
##
Out.println(Instant(1700000000000L).in[Json].show)
Out.println(t"5000".read[Json].as[Duration].value.inspect)
```

- `encodables` and `decodables` each have an instance for both types, and `parsables` has the
- instances the [direct parsers](#skipping-the-tree) use.
+ `encodables` et `decodables` ont chacun une instance pour les deux types, et `parsables`
+ contient les instances qu'utilisent les [analyseurs directs](#skipping-the-tree).

- ### JSON over HTTP
+ ### JSON sur HTTP

- A `Json` value is a request body and a response body, sent as `application/json`. The
- [HTTP client](http-client.md#posting-a-body) posts one once `postables.jsonPostable` is
- imported, and the response's text is read back as `Json`:
+ Une valeur `Json` est un corps de requête et un corps de réponse, envoyé en
+ `application/json`. Le [client HTTP](http-client.md#posting-a-body) en envoie une par POST une
+ fois `postables.jsonPostable` importé, et le texte de la réponse se relit comme `Json` :

```scala fuel
typecheck
##
import charsets.utf8Charset
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import postables.jsonPostable

val reply: Json =
  url"https://example.com/people".submit(Http.Post)(Person(t"Hamlet", 30).in[Json]).receive[Text].read[Json]
```

- With `servables.jsonServable` imported, an [HTTP server](http-server.md#responses) answers a
- request with a `Json` value in the same way. No glue is needed between the two layers: the
- encoders and decoders described above carry the values across.
+ Avec `servables.jsonServable` importé, un [serveur HTTP](http-server.md#responses) répond de la
+ même manière à une requête par une valeur `Json`. Aucune colle n'est nécessaire entre les deux
+ couches : les encodeurs et décodeurs décrits ci-dessus transportent les valeurs.

- ### Related
+ ### Voir aussi

- - [YAML](yaml.md), [XML](xml.md), [CBOR](cbor.md), [TEL](tel.md) and [CSV](csv.md) use the same
-   `read`, `as` and `in` vocabulary, so a case class which works with one works with the others.
- - [Derivation](derivation.md) explains how the codecs for a case class are generated.
- - [Decoding](decoding.md) covers reading a single text value as a type.
- - [Errors](errors.md) covers the strategies that decide what a `Json.Error` does.
- - [Optics](optics.md) covers the lenses used for deep updates.
- - [Records](records.md) covers the typed records a schema provides.
+ - [YAML](yaml.md), [XML](xml.md), [CBOR](cbor.md), [TEL](tel.md) et [CSV](csv.md) utilisent le
+   même vocabulaire `read`, `as` et `in`, de sorte qu'une case class qui fonctionne avec l'un
+   fonctionne avec les autres.
+ - [Dérivation](derivation.md) explique comment sont générés les codecs d'une case class.
+ - [Décodage](decoding.md) présente la lecture d'une valeur textuelle unique comme un type.
+ - [Erreurs](errors.md) présente les stratégies qui décident de ce que fait une `Json.Error`.
+ - [Optiques](optics.md) présente les lentilles utilisées pour les mises à jour profondes.
+ - [Enregistrements](records.md) présente les enregistrements typés que fournit un schéma.
