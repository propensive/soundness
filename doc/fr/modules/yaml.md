- ## YAML
+ ## YAML

- ### About
+ ### À propos

- [YAML](https://en.wikipedia.org/wiki/YAML) is handled in the same way as [JSON](json.md). Text
- parses into a `Yaml` value. A `Yaml` value converts to and from case classes with
- [derived][derivation] codecs. The `y"…"` [interpolator] writes YAML which is checked at
- [compiletime].
+ [YAML](https://en.wikipedia.org/wiki/YAML) est traité de la même manière que [JSON](json.md). Un
+ texte s'analyse en une valeur `Yaml`. Une valeur `Yaml` se convertit vers et depuis des case
+ classes grâce à des codecs [dérivés][dérivation]. L'[interpolateur] `y"…"` écrit du YAML vérifié
+ [à la compilation][compilation].

- The parser covers the language as it is really used: block and flow styles, quoting, block
- scalars, tags, anchors and aliases, and streams of several documents. A `Yaml` value is written
- back out as clean block-style text.
+ L'analyseur couvre le langage tel qu'il est réellement employé : styles bloc et flux, guillemets,
+ scalaires bloc, tags, ancres et alias, et flux de plusieurs documents. Une valeur `Yaml` est
+ réécrite sous forme de texte propre en style bloc.

- Navigation, updating through lenses, tracking of source positions, and collecting several
- decoding errors at once all have the same shape as they do for JSON. Knowing one format is
- knowing the other.
+ La navigation, la mise à jour par lentilles, le suivi des positions dans le code source et la
+ collecte de plusieurs erreurs de décodage à la fois ont tous la même forme que pour JSON.
+ Connaître un format, c'est connaître l'autre.

- ### On YAML
+ ### À propos de YAML

- YAML is the configuration language of half the modern toolchain, and it is notoriously easy to
- consume badly. Its indentation is structure. Its scalars are typed by guesswork. Its extras,
- such as anchors, tags and multi-document files, defeat casual parsers. A consumer typically
- reads it into an untyped tree and picks values out by string key, discovering any mismatch at
- [runtime].
+ YAML est le langage de configuration de la moitié des outils modernes, et il est notoirement
+ facile de mal le consommer. Son indentation fait partie de la structure. Ses scalaires sont typés
+ par devinette. Ses extras, comme les ancres, les tags et les fichiers à plusieurs documents,
+ mettent en échec les analyseurs sommaires. Un consommateur le lit généralement dans un arbre non
+ typé et en extrait les valeurs par clé textuelle, découvrant toute incohérence
+ [à l'exécution][exécution].

- Here the language is parsed properly, and the untyped tree is then left behind. A document
- decodes into the case class it should be. If it cannot, a typed error names what was missing or
- of the wrong type and, with tracking on, the line and column where it sat. Decoding straight to
- a typed value, through the same derivation as every other format, is
- [correctness](../../philosophy/correctness.md) with one definition of the data.
+ Ici, le langage est analysé correctement, puis l'arbre non typé est laissé de côté. Un document se
+ décode dans la case class qu'il doit être. S'il ne le peut pas, une erreur typée indique ce qui
+ manquait ou n'avait pas le bon type et, si le suivi est activé, la ligne et la colonne où cela se
+ trouvait. Décoder directement vers une valeur typée, par la même dérivation que pour tout autre
+ format, c'est la [correction](../../philosophy/correctness.md) avec une seule définition des
+ données.

- Everything comes from the `soundness` package. The samples throw any error as an exception, and
- the last import lets them print their results:
+ Tout provient du package `soundness`. Les exemples lèvent toute erreur sous forme d'exception, et
+ le dernier import leur permet d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Parsing and decoding
+ ### Analyse et décodage

- `read[Yaml]` parses text into a `Yaml` value, and `as` decodes it to a Scala type. Reading
- straight to a type, with `read[Person in Yaml]`, combines the two steps. Flow style and block
- style give the same result:
+ `read[Yaml]` analyse un texte en une valeur `Yaml`, et `as` la décode vers un type Scala. Lire
+ directement vers un type, avec `read[Person in Yaml]`, combine les deux étapes. Le style flux et
+ le style bloc donnent le même résultat :

```scala
case class Person(name: Text, age: Int)

val hamlet = t"{name: Hamlet, age: 30}".read[Yaml].as[Person]
val horatio = t"name: Horatio\nage: 32".read[Person in Yaml]
```

```scala fuel
run
##
Out.println(hamlet.inspect)
Out.println(horatio.inspect)
```

- A document which lacks a required field, or holds a value of the wrong type, does not decode,
- and the error says why:
+ Un document auquel manque un champ requis, ou qui contient une valeur du mauvais type, ne se
+ décode pas, et l'erreur en indique la raison :

```scala fuel
run
errors show
##
t"name: Ophelia".read[Person in Yaml]
```

```scala fuel
run
errors show
##
t"{name: Yorick, age: unknown}".read[Person in Yaml]
```

- A stream of several documents, separated by `---`, reads as a list:
+ Un flux de plusieurs documents, séparés par `---`, se lit comme une liste :

```scala fuel
run
##
Out.println(t"---\n1\n---\n2\n---\n3".read[List[Yaml]].map(_.as[Int]).inspect)
```

- A source need not be text held in memory. The parser reads straight from a
- [stream](streams.md) of bytes or characters, refilling as it goes, without first joining the
- input together. A large document is therefore parsed without ever being held whole, and a `---`
- marker which is split across two chunks is still a document boundary.
+ Une source n'est pas forcément un texte gardé en mémoire. L'analyseur lit directement depuis un
+ [flux](streams.md) d'octets ou de caractères, en se réalimentant au fur et à mesure, sans d'abord
+ réunir l'entrée. Un document volumineux est donc analysé sans jamais être conservé en entier, et
+ un marqueur `---` coupé entre deux morceaux reste une frontière de document.

- ### Encoding
+ ### Encodage

- `in[Yaml]` encodes a value as `Yaml`, and `show` writes it as block-style text, in the
- formatting in scope:
+ `in[Yaml]` encode une valeur en `Yaml`, et `show` l'écrit sous forme de texte en style bloc, selon
+ la mise en forme dans la portée :

```scala
import formatting.blockYamlFormatting
```

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).in[Yaml].show)
```

- `in[Text over Yaml]` encodes a value straight to YAML text, in the `Yaml.Formatting` in scope,
- and `in[Data over Yaml]` writes the same text as bytes, in the `Codepage` in scope.
+ `in[Text over Yaml]` encode une valeur directement en texte YAML, selon le `Yaml.Formatting` dans
+ la portée, et `in[Data over Yaml]` écrit le même texte sous forme d'octets, selon le `Codepage`
+ dans la portée.

- The `@name` annotation renames a field in the document, exactly as it does for
- [JSON](json.md#renaming-fields). An enumeration encodes with a discriminator field, chosen by
- importing `yamlByTypeDiscriminable` or `yamlByKindDiscriminable`, as a
- [tagged union](json.md#tagged-unions) does.
+ L'annotation `@name` renomme un champ dans le document, exactement comme pour
+ [JSON](json.md#renaming-fields). Une énumération s'encode avec un champ discriminant, choisi en
+ important `yamlByTypeDiscriminable` ou `yamlByKindDiscriminable`, comme le fait une
+ [union étiquetée](json.md#tagged-unions).

- ### Optional fields
+ ### Champs optionnels

- An `Optional` field has three policies, from the package `optionalityOptions`, as
- [Decoding](decoding.md#policies-for-optional-fields) describes:
+ Un champ `Optional` dispose de trois politiques, issues du package `optionalityOptions`, comme le
+ décrit [Décodage](decoding.md#policies-for-optional-fields) :

```tabular
- column  A key which is
- column  Reads as
- column  Unless
- row  omitted from the mapping  `Unset`  `strictYamlAbsence` makes it an error
- row  given an explicit `null`  `Unset`  `strictYamlNulls` hands it to the inner decoder
- row  present but rejected by the inner decoder  an error  `lenientYamlFaults` reads it as `Unset`
+ column  Une clé qui est
+ column  Se lit comme
+ column  Sauf si
+ row  omise de la correspondance  `Unset`  `strictYamlAbsence` en fait une erreur
+ row  donnée avec un `null` explicite  `Unset`  `strictYamlNulls` la confie au décodeur interne
+ row  présente mais rejetée par le décodeur interne  une erreur  `lenientYamlFaults` la lit comme `Unset`
```

```scala
case class Member(name: Text, age: Optional[Int])

val fool = t"name: Fool\nage: null".read[Yaml].as[Member]

val strict =
  import optionalityOptions.strictYamlNulls
  capture[Yaml.Error](t"name: Fool\nage: null".read[Yaml].as[Member]).reason
```

```scala fuel
run
##
Out.println(fool.inspect)
Out.println(strict.inspect)
```

- ### Scalars, quoting and block styles
+ ### Scalaires, guillemets et styles bloc

- Most of YAML's surprises live in its scalars. Each style is handled as the specification says:
+ La plupart des surprises de YAML résident dans ses scalaires. Chaque style est traité comme le
+ dit la spécification :

```tabular
- column  Scalar
- column  Resolves as
- row  plain `true`  a boolean
- row  plain `42`  an integer
- row  plain `~` or `null`  a null
- row  `'…'`, single-quoted  text, taking no escapes; a quote is doubled to include one
- row  `"…"`, double-quoted  text, taking the full escape set
+ column  Scalaire
+ column  Se résout en
+ row  `true` simple  un booléen
+ row  `42` simple  un entier
+ row  `~` ou `null` simple  un null
+ row  `'…'`, entre guillemets simples  du texte, sans échappement ; on double un guillemet pour l'inclure
+ row  `"…"`, entre guillemets doubles  du texte, avec l'ensemble complet des échappements
```

- A quoted scalar is always text, so `"42"` stays a string:
+ Un scalaire entre guillemets est toujours du texte, donc `"42"` reste une chaîne :

```scala fuel
run
errors show
##
t"""age: "42"""".read[Yaml].as[Map[Text, Int]]
```

- A block scalar carries its own handling of newlines. It has a style indicator, and an optional
- *chomping* indicator for the trailing newline:
+ Un scalaire bloc porte son propre traitement des sauts de ligne. Il a un indicateur de style, et
+ un indicateur de *chomping* optionnel pour le saut de ligne final :

```tabular
- column  Indicator
- column  Effect
- row  `|`  keeps line breaks
- row  `>`  folds line breaks into spaces
- row  no chomping indicator  keeps one trailing newline
- row  `-`  strips the trailing newline
- row  `+`  keeps every trailing newline
+ column  Indicateur
+ column  Effet
+ row  `|`  conserve les sauts de ligne
+ row  `>`  replie les sauts de ligne en espaces
+ row  aucun indicateur de chomping  conserve un saut de ligne final
+ row  `-`  supprime le saut de ligne final
+ row  `+`  conserve tous les sauts de ligne finaux
```

```scala fuel
run
##
Out.println(t"text: |\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
Out.println(t"text: >\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
Out.println(t"text: |-\n  line1\n  line2".read[Yaml].as[Map[Text, Text]].inspect)
```

- Comments are ignored wherever they may appear, whether after a scalar or on a line of their
- own. Leading whitespace, trailing whitespace and whitespace at the end of a line are all
- treated as the specification requires.
+ Les commentaires sont ignorés partout où ils peuvent apparaître, après un scalaire ou sur une
+ ligne à part. Les espaces de début, les espaces de fin et les espaces en fin de ligne sont tous
+ traités comme l'exige la spécification.

- ### Tags
+ ### Tags

- A tag overrides the type which a scalar's content would otherwise be given. It is how YAML says
- "this number is a string", and the reverse:
+ Un tag remplace le type que le contenu d'un scalaire recevrait autrement. C'est ainsi que YAML dit
+ « ce nombre est une chaîne », et l'inverse :

```tabular
- column  Tag
- column  Resolves a scalar as
- row  `!!str`, `!`, `!!`  text
- row  `!!int`  an integer
- row  `!!float`  a decimal
- row  `!!bool`  a boolean, from `true`, `True` or `TRUE`, and likewise `false`
- row  `!!null`  a null
+ column  Tag
+ column  Résout un scalaire en
+ row  `!!str`, `!`, `!!`  du texte
+ row  `!!int`  un entier
+ row  `!!float`  un décimal
+ row  `!!bool`  un booléen, à partir de `true`, `True` ou `TRUE`, et de même pour `false`
+ row  `!!null`  un null
```

```scala fuel
run
##
Out.println(t"!!str 42".read[Yaml].as[Text])
Out.println(t"!!int \"42\"".read[Yaml].as[Int].show)
Out.println(t"!!float 7".read[Yaml].as[Double].inspect)
```

- ### Anchors and aliases
+ ### Ancres et alias

- An anchor names a node, and an alias refers back to it, so a document need not repeat itself.
- Aliases are resolved as the document is read, whatever the anchored node was: a scalar, a flow
- sequence or mapping, or a whole block.
+ Une ancre nomme un nœud, et un alias y renvoie, si bien qu'un document n'a pas à se répéter. Les
+ alias sont résolus à la lecture du document, quel que soit le nœud ancré : un scalaire, une
+ séquence ou une correspondance en style flux, ou un bloc entier.

```scala
case class Inner(n: Int)
```

```scala fuel
run
##
Out.println(t"a: &x 1\nb: *x".read[Yaml].as[Map[Text, Int]].inspect)
Out.println(t"defaults: &d\n  n: 7\nuse: *d".read[Yaml].as[Map[Text, Inner]].inspect)
```

- This is what makes YAML configuration files terse, and what makes a naive parser get them
- wrong.
+ C'est ce qui rend les fichiers de configuration YAML concis, et ce qui fait qu'un analyseur naïf
+ les interprète mal.

- ### Writing YAML literally
+ ### Écrire du YAML littéralement

- The `y"…"` interpolator writes YAML directly. It is checked at compiletime, and values are
- substituted into it. The same literal takes a document apart in a pattern, as
- [`j"…"`](json.md#matching-json) does:
+ L'interpolateur `y"…"` écrit directement du YAML. Il est vérifié à la compilation, et des valeurs
+ y sont substituées. Le même littéral décompose un document dans un motif, comme le fait
+ [`j"…"`](json.md#matching-json) :

```scala
val name = t"Hamlet"

val person = y"""
  name: $name
  age: 30
""".as[Person]
```

```scala fuel
run
##
Out.println(person.inspect)
```

- ### Navigating and updating
+ ### Navigation et mise à jour

- With dynamic access enabled, a field is read as a member, and assigning to a field gives a new
- document. Assigning `Unset` removes a field:
+ Avec l'accès dynamique activé, un champ se lit comme un membre, et affecter un champ donne un
+ nouveau document. Affecter `Unset` supprime un champ :

```scala
import dynamicAccess.dynamicYaml

val doc = t"{name: Hamlet, age: 30}".read[Yaml]
```

```scala fuel
run
##
Out.println(doc.name.as[Text])
Out.println((doc.age = 31).show)
```

- The import grants the permission `Yaml is Dynamical` to the rest of its scope.
- `dynamically[Yaml]:` grants it to a single block instead, as
- [JSON](json.md#fields-as-members) describes.
+ L'import accorde la permission `Yaml is Dynamical` au reste de sa portée. `dynamically[Yaml]:`
+ l'accorde plutôt à un seul bloc, comme le décrit [JSON](json.md#fields-as-members).

- A deeper change goes through a [lens](optics.md), with optics such as `Each` and `Filter` to
- reach many elements at once:
+ Une modification plus profonde passe par une [lentille](optics.md), avec des optiques comme
+ `Each` et `Filter` pour atteindre plusieurs éléments à la fois :

```scala
val older = doc.lens(_.age = 31.in[Yaml])
```

- ### Positions and accumulated errors
+ ### Positions et erreurs accumulées

- Bringing `parsing.trackPositions` into scope switches on tracking, as it does for
- [JSON](json.md#source-positions). Every value then remembers its line and column, so a decoding
- failure can point into the source file. That is the difference between "field missing" and
- "field missing at line 12".
+ Mettre `parsing.trackPositions` dans la portée active le suivi, comme pour
+ [JSON](json.md#source-positions). Chaque valeur se souvient alors de sa ligne et de sa colonne,
+ de sorte qu'un échec de décodage peut désigner un endroit du fichier source. C'est la différence
+ entre « champ manquant » et « champ manquant à la ligne 12 ».

```scala
import parsing.trackPositions

val tracked = t"{a: 1, b: 2}".read[Yaml]
val position = tracked.locate(YamlPath()(t"a"))
```

- Under a strategy which [accumulates failures](errors.md#accumulating-failures), decoding
- collects every fault in a document instead of stopping at the first. Each is tagged with its
- path, such as `#/age` or `#/email`, so the problems of a whole configuration file are reported
- together.
+ Sous une stratégie qui [accumule les échecs](errors.md#accumulating-failures), le décodage
+ recueille toutes les fautes d'un document au lieu de s'arrêter à la première. Chacune est
+ étiquetée par son chemin, comme `#/age` ou `#/email`, si bien que les problèmes d'un fichier de
+ configuration entier sont signalés ensemble.

- ### Numbers
+ ### Nombres

- A YAML number, like a JSON one, has no limit on its precision. A number beyond the range of a
- `Long` or a `Double` is kept exactly, as a binary-coded decimal. A long identifier or a precise
- decimal read from a configuration file is therefore never silently rounded on the way through.
+ Un nombre YAML, comme un nombre JSON, n'a pas de limite de précision. Un nombre hors de la plage
+ d'un `Long` ou d'un `Double` est conservé exactement, sous forme de décimal codé en binaire. Un
+ long identifiant ou un décimal précis lu dans un fichier de configuration n'est donc jamais
+ arrondi en silence au passage.

- ### Skipping the tree
+ ### Se passer de l'arbre

- The conversions above all go through the tree. Text is parsed to a `Yaml` and decoded, or a
- value is encoded to a `Yaml` and written out. A record can instead carry a *direct* parser and
- printer, derived once into its companion with `derives Yaml.Direct`. A single import then
- selects the route for every record in scope, as it does for
- [JSON](json.md#skipping-the-tree):
+ Les conversions ci-dessus passent toutes par l'arbre. Le texte est analysé en `Yaml` puis décodé,
+ ou une valeur est encodée en `Yaml` puis écrite. Un enregistrement peut à la place porter un
+ analyseur et un afficheur *directs*, dérivés une fois pour toutes dans son objet compagnon avec
+ `derives Yaml.Direct`. Un seul import choisit alors la voie pour chaque enregistrement dans la
+ portée, comme pour [JSON](json.md#skipping-the-tree) :

```tabular
- column  Import
- column  For a record
- column  For any other type
- row  `codecs.direct`  a direct parser and printer  the tree codecs, unless it has a direct instance of its own
- row  `codecs.derived`  the tree codecs, even where a direct instance is in scope  the tree codecs
- row  `codecs.strict`  a direct parser and printer  a compile error naming the type, unless it has a direct instance of its own
+ column  Import
+ column  Pour un enregistrement
+ column  Pour tout autre type
+ row  `codecs.direct`  un analyseur et un afficheur directs  les codecs de l'arbre, sauf s'il possède sa propre instance directe
+ row  `codecs.derived`  les codecs de l'arbre, même lorsqu'une instance directe est dans la portée  les codecs de l'arbre
+ row  `codecs.strict`  un analyseur et un afficheur directs  une erreur de compilation nommant le type, sauf s'il possède sa propre instance directe
```

- A type which is not a record is an enumeration or a sealed hierarchy. The modes are
- alternatives, so import one at a time.
+ Un type qui n'est pas un enregistrement est une énumération ou une hiérarchie scellée. Les modes
+ sont des alternatives : importez-en un seul à la fois.

- A direct printer writes a record's text straight from its fields, with no tree built. The text
- is the same as `show` writes.
+ Un afficheur direct écrit le texte d'un enregistrement directement à partir de ses champs, sans
+ construire d'arbre. Le texte est le même que celui qu'écrit `show`.

- A direct parser reads the fields straight from the nodes of the parsed document. No `Yaml` is
- built around any of them, and no codec is looked up for each field. The document is still
- parsed whole first, though. Anchors and aliases mean that a node may refer back to any earlier
- one, so a record cannot be read before the whole document is held.
+ Un analyseur direct lit les champs directement depuis les nœuds du document analysé. Aucun `Yaml`
+ n'est construit autour d'eux, et aucun codec n'est recherché pour chaque champ. Le document est
+ toutefois d'abord analysé en entier. Les ancres et les alias font qu'un nœud peut renvoyer à
+ n'importe quel nœud antérieur, si bien qu'un enregistrement ne peut pas être lu avant que le
+ document entier soit en mémoire.

- ### Conformance
+ ### Conformité

- The parser is checked against the [YAML test suite](https://github.com/yaml/yaml-test-suite),
- the community's shared collection of cases, including those designed to break parsers. Cases
- outside the supported subset are recorded as known gaps, not quietly skipped, so the coverage
- is stated and not merely claimed.
+ L'analyseur est vérifié avec la [suite de tests YAML](https://github.com/yaml/yaml-test-suite),
+ la collection de cas partagée par la communauté, y compris ceux conçus pour casser les
+ analyseurs. Les cas hors du sous-ensemble pris en charge sont consignés comme des lacunes
+ connues, et non ignorés en silence : la couverture est ainsi établie, et pas seulement affirmée.

- ### YAML over HTTP
+ ### YAML sur HTTP

- A `Yaml` value is a request body or a response body, with the media type `application/yaml`,
- and a body is parsed back to `Yaml` on arrival. A YAML API is therefore consumed and offered
- with the same code as a [JSON one](json.md#json-over-http). Only the format's type differs.
+ Une valeur `Yaml` peut être un corps de requête ou un corps de réponse, avec le type de média
+ `application/yaml`, et un corps est réanalysé en `Yaml` à son arrivée. Une API YAML se consomme
+ et se propose donc avec le même code qu'une [API JSON](json.md#json-over-http). Seul le type du
+ format diffère.

- ### Related
+ ### Voir aussi

- - [Annotations](annotations.md), [CBOR](cbor.md), [CSV](csv.md), [XML](xml.md), [network addresses](network-addresses.md) and [OpenAPI](openapi.md) share YAML's codecs and carry its documents.
+ - [Annotations](annotations.md), [CBOR](cbor.md), [CSV](csv.md), [XML](xml.md),
+   [adresses réseau](network-addresses.md) et [OpenAPI](openapi.md) partagent les codecs de YAML et
+   transportent ses documents.
