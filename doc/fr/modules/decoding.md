- ## Decoding
+ ## Décodage

- ### About
+ ### Présentation

- A running program receives much of its data as loose, untyped text: command-line arguments,
- environment variables, configuration files, the groups captured by a regular expression, a line
- typed at a prompt. Decoding turns that text into typed values, such as an `Int`, a `Uuid` or an
- email address.
+ Un programme en cours d'exécution reçoit une grande partie de ses données sous forme de texte
+ libre et non typé : arguments de la ligne de commande, variables d'environnement, fichiers de
+ configuration, groupes capturés par une expression régulière, ligne tapée à une invite. Le
+ décodage transforme ce texte en valeurs typées, comme un `Int`, un `Uuid` ou une adresse
+ e-mail.

- Every such conversion shares one fact: it might not succeed. The types say so. A conversion
- never returns a plausible value for input it could not read, which is what
- [total transitions](../../philosophy/total-transitions.md) demands.
+ Toutes ces conversions partagent un fait : elles peuvent ne pas réussir. Les types le disent.
+ Une conversion ne renvoie jamais une valeur plausible pour une entrée qu'elle n'a pas pu lire,
+ ce qu'exigent les [transitions totales](../../philosophy/total-transitions.md).

- ### On conversion
+ ### Sur la conversion

- A conversion from one representation to another can fail in more than one way, and the
- differences matter:
+ Une conversion d'une représentation à une autre peut échouer de plus d'une façon, et les
+ différences comptent :

- - Reading an integer from arbitrary text can fail, because the text might not be a number.
- - Narrowing a `Long` to an `Int` can fail, because the value might not fit.
- - Widening a `Byte` to an `Int` can never fail.
+ - Lire un entier dans un texte arbitraire peut échouer, car le texte n'est peut-être pas un
+   nombre.
+ - Restreindre un `Long` en `Int` peut échouer, car la valeur ne tient peut-être pas.
+ - Élargir un `Byte` en `Int` ne peut jamais échouer.

- Treating these alike, by returning `null`, throwing from a getter or quietly truncating, is
- where bugs begin. Here each has its own [typeclass], so the way a conversion can fail is visible
- in the code which calls it:
+ Les traiter de la même façon, en renvoyant `null`, en levant une exception depuis un accesseur
+ ou en tronquant discrètement, c'est là que commencent les bugs. Ici, chacune a sa propre
+ [classe de types] ; la façon dont une conversion peut échouer est donc visible dans le code qui
+ l'appelle :

```tabular
- column  Typeclass
- column  Written
- column  When the conversion fails
- row  `Decodable`  `Int is Decodable in Text`  raises a typed error
- row  `Extractable`  `Text is Extractable to Int`  yields `Unset`
- row  `Irrefutable`  `Byte is Irrefutable to Int`  cannot fail
- row  `Identifiable`  `Identifiable[Column]`  cannot fail; it maps a name both ways
+ column  Classe de types
+ column  Écrite
+ column  Quand la conversion échoue
+ row  `Decodable`  `Int is Decodable in Text`  lève une erreur typée
+ row  `Extractable`  `Text is Extractable to Int`  donne `Unset`
+ row  `Irrefutable`  `Byte is Irrefutable to Int`  ne peut pas échouer
+ row  `Identifiable`  `Identifiable[Column]`  ne peut pas échouer ; elle convertit un nom dans les deux sens
```

- The names read as English, and state the direction of the conversion. A value which
- `is Decodable in Text` is read *out of* text. Text which `is Extractable to Int` is read
- *toward* an integer. An `Extractable` gives no result, instead of raising an error, which is
- exactly what pattern matching wants.
+ Les noms se lisent comme de l'anglais, et indiquent le sens de la conversion. Une valeur qui
+ `is Decodable in Text` se lit *à partir* d'un texte. Un texte qui `is Extractable to Int` se lit
+ *vers* un entier. Un `Extractable` ne donne aucun résultat, au lieu de lever une erreur, ce qui
+ est exactement ce que veut le filtrage par motif.

- The everyday surface is small: the `as` method and the `As` extractor. The sections below begin
- there, and then turn to the typeclasses underneath and to writing instances for new types.
+ La surface de tous les jours est réduite : la méthode `as` et l'extracteur `As`. Les sections
+ ci-dessous commencent par là, puis se tournent vers les classes de types sous-jacentes et vers
+ l'écriture d'instances pour de nouveaux types.

- Everything comes from the `soundness` package. Decoding can fail, so it needs a [strategy] in
- scope, and `throwUnsafely` throws an exception when a decode fails. The last import lets the
- samples print their results:
+ Tout provient du package `soundness`. Le décodage peut échouer ; il a donc besoin d'une
+ [stratégie] dans la portée, et `throwUnsafely` lève une exception quand un décodage échoue. Le
+ dernier import permet aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Decoding a value
+ ### Décoder une valeur

- The `as` method reads a typed value out of text. The target type is named, and a `Decodable`
- instance for it does the work:
+ La méthode `as` lit une valeur typée dans un texte. On nomme le type cible, et une instance
+ `Decodable` pour celui-ci fait le travail :

```scala fuel
run
##
Out.println(t"123".as[Int].show)
```

- Text which is not a number raises a `Number.Error`:
+ Un texte qui n'est pas un nombre lève une `Number.Error` :

```scala fuel
run
errors show
##
t"hello".as[Int]
```

- A `Number.Error` has a reason. Text which is not a number at all is *unparseable*, while a
- number outside the target type's range is *out of range*. Decoding to a `Byte` shows both:
+ Une `Number.Error` a une raison. Un texte qui n'est pas du tout un nombre est *inanalysable*,
+ tandis qu'un nombre hors de l'intervalle du type cible est *hors limites*. Le décodage en `Byte`
+ montre les deux :

```scala fuel
run
##
Out.println(capture[Number.Error](t"300".as[Byte]).reason.inspect)
Out.println(capture[Number.Error](t"abc".as[Byte]).reason.inspect)
```

- The strategy which handles a failed decode is chosen by the caller, not fixed by the decoder.
- [`safely`](errors.md#choosing-a-strategy) turns a failure into `Unset`, so falling back to a
- default is one more method call:
+ La stratégie qui traite un décodage échoué est choisie par l'appelant, et non fixée par le
+ décodeur. [`safely`](errors.md#choosing-a-strategy) transforme un échec en `Unset` ; se replier
+ sur une valeur par défaut ne coûte donc qu'un appel de méthode de plus :

```scala fuel
run
##
Out.println(safely(t"hello".as[Int]).or(0).show)
```

- A type with no `Decodable` instance cannot be asked for, and the compiler says so:
+ On ne peut pas demander un type dépourvu d'instance `Decodable`, et le compilateur le signale :

```scala
class Widget()
```

```scala fuel
compile
errors show
##
t"sprocket".as[Widget]
```

- Instances are built in for the primitive number types, for `Char`, and for types defined
- elsewhere in Soundness, such as `Uuid` and `Fqcn`. Other parts of Soundness add their own, which
- is why the same `as` reads a [date](time.md), a
- [hostname or a URL](network-addresses.md#parsing-at-runtime):
+ Des instances sont intégrées pour les types numériques primitifs, pour `Char`, et pour des types
+ définis ailleurs dans Soundness, comme `Uuid` et `Fqcn`. D'autres parties de Soundness ajoutent
+ les leurs ; c'est pourquoi le même `as` lit une [date](time.md), un
+ [nom d'hôte ou une URL](network-addresses.md#parsing-at-runtime) :

```scala
val date = t"2024-01-15".as[Date]
val host = t"example.com".as[Hostname]
```

- #### Booleans
+ #### Booléens

- A `Boolean` decodes from `true` or `false`, in any case, with or without space around it.
- Anything else raises a `Truth.Error`. The other spellings a tool might accept are each a `Truth`
- in the package `truths`:
+ Un `Boolean` se décode à partir de `true` ou `false`, quelle que soit la casse, avec ou sans
+ espaces autour. Toute autre chose lève une `Truth.Error`. Les autres graphies qu'un outil
+ pourrait accepter sont chacune une `Truth` du package `truths` :

```tabular
- column  Given
- column  True
- column  False
- row  `yesNoTruth`  `yes`  `no`
- row  `onOffTruth`  `on`  `off`
- row  `oneZeroTruth`  `1`  `0`
+ column  Given
+ column  Vrai
+ column  Faux
+ row  `yesNoTruth`  `yes`  `no`
+ row  `onOffTruth`  `on`  `off`
+ row  `oneZeroTruth`  `1`  `0`
```

- Every `Truth` in scope is accepted beside the default. Importing one adds spellings, and takes
- none away:
+ Chaque `Truth` dans la portée est acceptée en plus de la valeur par défaut. En importer une
+ ajoute des graphies, et n'en retire aucune :

```scala
import truths.yesNoTruth
```

```scala fuel
run
##
Out.println(t"yes".as[Boolean].inspect)
Out.println(t"True".as[Boolean].inspect)
```

- ### Policies for optional fields
+ ### Politiques pour les champs optionnels

- Every data format which decodes an `Optional` field answers the same three questions, and by
- default answers them the same way. That covers [JSON](json.md#optional-fields-and-defaults),
- [CBOR](cbor.md), [YAML](yaml.md), [XML](xml.md), [TEL](tel.md) and
- [delimited text](csv.md). Taking JSON as the example:
+ Chaque format de données qui décode un champ `Optional` répond aux trois mêmes questions, et par
+ défaut y répond de la même façon. Cela couvre [JSON](json.md#optional-fields-and-defaults),
+ [CBOR](cbor.md), [YAML](yaml.md), [XML](xml.md), [TEL](tel.md) et le
+ [texte délimité](csv.md). Avec JSON pour exemple :

```tabular
- column  A field which is
- column  Reads as
- column  Unless
- row  omitted from the document  `Unset`  `strictJsonAbsence` makes it an error
- row  an explicit null  `Unset`  `strictJsonNulls` hands it to the inner decoder
- row  present but rejected by the inner decoder  an error  `lenientJsonFaults` reads it as `Unset`
+ column  Un champ qui est
+ column  Se lit comme
+ column  Sauf si
+ row  omis du document  `Unset`  `strictJsonAbsence` en fait une erreur
+ row  un null explicite  `Unset`  `strictJsonNulls` le transmet au décodeur interne
+ row  présent mais rejeté par le décodeur interne  une erreur  `lenientJsonFaults` le lit comme `Unset`
```

- Each answer is a policy, indexed by the format, with a default in its companion:
+ Chaque réponse est une politique, indexée par le format, avec une valeur par défaut dans son
+ compagnon :

```scala fuel
run
##
Out.println(summon[Decodable.Absence in Text].strict.inspect)
Out.println(summon[Decodable.Nullity in Text].strict.inspect)
Out.println(summon[Decodable.Fault in Text].strict.inspect)
```

- A format's `optionalityOptions` package offers the other answer to each question, named for the
- format. One answer can therefore be changed without touching the others, and JSON can be strict
- where, in the same scope, CSV stays lenient. These are the givens which change a default. A
- format with no null has no policy for nulls:
+ Le package `optionalityOptions` d'un format propose l'autre réponse à chaque question, nommée
+ d'après le format. On peut donc changer une réponse sans toucher aux autres, et JSON peut être
+ strict là où, dans la même portée, CSV reste tolérant. Voici les givens qui changent une valeur
+ par défaut. Un format sans null n'a pas de politique pour les nulls :

```tabular
- column  Format
- column  Omitted is an error
- column  Null goes to the decoder
- column  Rejected is `Unset`
- row  JSON  `strictJsonAbsence`  `strictJsonNulls`  `lenientJsonFaults`
- row  YAML  `strictYamlAbsence`  `strictYamlNulls`  `lenientYamlFaults`
- row  CBOR  `strictCborAbsence`  `strictCborNulls`  `lenientCborFaults`
- row  TEL
-   cell  `strictTelAbsence`
-   cell
-   cell  `lenientTelFaults`
- row  XML
-   cell  `strictXmlAbsence`
-   cell
-   cell  `lenientXmlFaults`
- row  Delimited text
-   cell  `strictDsvAbsence`
-   cell
-   cell  `lenientDsvFaults`
+ column  Format
+ column  L'omission est une erreur
+ column  Le null va au décodeur
+ column  Le rejet donne `Unset`
+ row  JSON  `strictJsonAbsence`  `strictJsonNulls`  `lenientJsonFaults`
+ row  YAML  `strictYamlAbsence`  `strictYamlNulls`  `lenientYamlFaults`
+ row  CBOR  `strictCborAbsence`  `strictCborNulls`  `lenientCborFaults`
+ row  TEL
+   cell  `strictTelAbsence`
+   cell
+   cell  `lenientTelFaults`
+ row  XML
+   cell  `strictXmlAbsence`
+   cell
+   cell  `lenientXmlFaults`
+ row  Texte délimité
+   cell  `strictDsvAbsence`
+   cell
+   cell  `lenientDsvFaults`
```

- Each package also names the default answers, `lenientJsonAbsence`, `strictJsonFaults` and so on,
- for a scope which must restore one.
+ Chaque package nomme aussi les réponses par défaut, `lenientJsonAbsence`, `strictJsonFaults` et
+ ainsi de suite, pour une portée qui doit en rétablir une.

- A lenient *fault* decodes the value under the ambient tactic's
- [`tolerate`](errors.md#tolerating-a-failure), so it records nothing even where errors are being
- accumulated. `Option` fields follow the same policies, and give `None`. The tutorial for each
- format shows the policies in use.
+ Un *défaut* toléré décode la valeur sous le
+ [`tolerate`](errors.md#tolerating-a-failure) de la tactique ambiante ; il n'enregistre donc rien,
+ même là où les erreurs sont accumulées. Les champs `Option` suivent les mêmes politiques, et
+ donnent `None`. Le tutoriel de chaque format montre ces politiques à l'œuvre.

- ### Extracting in patterns
+ ### Extraire dans les motifs

- Pattern matching needs a conversion which declines to match, instead of raising an error. That
- is the `As` extractor. `As[Int]` succeeds when the value being matched yields an `Int`, and
- falls through to the next case when it does not:
+ Le filtrage par motif a besoin d'une conversion qui refuse de correspondre, au lieu de lever
+ une erreur. C'est l'extracteur `As`. `As[Int]` réussit quand la valeur filtrée donne un `Int`,
+ et passe au cas suivant sinon :

```scala
def number(text: Text): Int = text match
  case As[Int](number) => number
  case _               => 0
```

```scala fuel
run
##
Out.println(number(t"123").show)
Out.println(number(t"abc").show)
```

- `As` is an ordinary [extractor](https://docs.scala-lang.org/tour/extractor-objects.html), so it
- composes with every pattern Scala offers. It binds the groups of a
- [regular expression](patterns.md), turning captured text into typed values in one step:
+ `As` est un [extracteur](https://docs.scala-lang.org/tour/extractor-objects.html) ordinaire ; il
+ se compose donc avec tous les motifs qu'offre Scala. Il lie les groupes d'une
+ [expression régulière](patterns.md), transformant le texte capturé en valeurs typées en une
+ seule étape :

```scala
val time = t"12:13:14" match
  case r"${As[Int](hours)}(\d+):${As[Int](minutes)}(\d+):${As[Int](seconds)}(\d+)" =>
    List(hours, minutes, seconds)
  case _ =>
    List(0, 0, 0)
```

- It nests inside larger patterns just as freely. This match takes a list apart and decodes each
- element at once:
+ Il s'imbrique tout aussi librement dans des motifs plus grands. Ce filtrage décompose une liste
+ et décode chaque élément d'un coup :

```scala
val triple = List(t"12", t"13", t"14") match
  case As[Int](first) :: As[Int](second) :: As[Int](third) :: Nil => (first, second, third)
  case _                                                          => (0, 0, 0)
```

```scala fuel
run
##
Out.println(time.inspect)
Out.println(triple.inspect)
```

- The extractor works for any type with a suitable instance, not only numbers. An email address
- is extracted when it is well-formed, and declined otherwise, so a single match both validates
- and converts:
+ L'extracteur fonctionne pour tout type doté d'une instance adéquate, et pas seulement les
+ nombres. Une adresse e-mail est extraite quand elle est bien formée, et refusée sinon ; un seul
+ filtrage valide et convertit donc à la fois :

```scala
def recipient(text: Text): EmailAddress = text match
  case As[EmailAddress](email) => email
  case _                       => email"nobody@example.com"
```

```scala fuel
run
##
Out.println(recipient(t"hamlet@example.com").show)
Out.println(recipient(t"hamlet.example.com").show)
```

- ### Narrowing and widening
+ ### Restreindre et élargir

- Numbers convert between widths, and the direction decides whether the conversion can fail.
- Widening always succeeds: every `Byte` is an `Int`. Narrowing succeeds only when the value fits,
- so it belongs to `As`, which simply declines when it does not:
+ Les nombres se convertissent entre largeurs, et le sens détermine si la conversion peut
+ échouer. L'élargissement réussit toujours : tout `Byte` est un `Int`. La restriction ne réussit
+ que si la valeur tient ; elle relève donc de `As`, qui refuse simplement quand ce n'est pas le
+ cas :

```scala
def narrow(value: Int): Byte = value match
  case As[Byte](byte) => byte
  case _              => -1
```

```scala fuel
run
##
Out.println(narrow(100).show)
Out.println(narrow(300).show)
```

- An [enumeration] is extracted by the name of its cases. There is no instance to write, since
- the cases are read from the type:
+ Une [énumération] s'extrait par le nom de ses cas. Il n'y a aucune instance à écrire, puisque
+ les cas sont lus dans le type :

```scala
enum Direction:
  case North, South, East, West

def direction(text: Text): Optional[Direction] = text match
  case As[Direction](direction) => direction
  case _                        => Unset
```

```scala fuel
run
##
Out.println(direction(t"North").inspect)
Out.println(direction(t"Up").inspect)
```

- ### Requiring a value
+ ### Exiger une valeur

- Some types accept an empty input and some do not, and a form or a configuration often needs to
- know which before it asks for a value. A type is *required* when an empty input cannot produce
- one. That follows from the type's decoder, so no separate declaration is needed. The decoder is
- simply run against the empty text, and whether it succeeds is the answer:
+ Certains types acceptent une entrée vide et d'autres non, et un formulaire ou une configuration
+ a souvent besoin de le savoir avant de demander une valeur. Un type est *requis* quand une
+ entrée vide ne peut en produire une. Cela découle du décodeur du type ; aucune déclaration
+ séparée n'est donc nécessaire. Le décodeur est simplement exécuté sur le texte vide, et sa
+ réussite ou non est la réponse :

```scala fuel
run
##
Out.println(safely(t"".as[Int]).present.inspect)
Out.println(safely(t"".as[Text]).present.inspect)
```

- An `Int` is required, since the empty text is not a number. A `Text` is not, since the empty
- text is a `Text`. A [form](forms.md) marks its mandatory fields this way, without a second
- declaration which could disagree with the decoder.
+ Un `Int` est requis, puisque le texte vide n'est pas un nombre. Un `Text` ne l'est pas, puisque
+ le texte vide est un `Text`. Un [formulaire](forms.md) marque ainsi ses champs obligatoires,
+ sans seconde déclaration qui pourrait contredire le décodeur.

- ### Enumerations by name and position
+ ### Énumérations par nom et par position

- An enumeration's cases are known to the compiler, so its `Enumerable` instance is
- [derived][derivation] from the type, not written. The instance gives the enumeration's cases in
- the order they were declared, and looks a case up by name or by [ordinal](numbers.md#ordinals).
- Each lookup gives an `Optional`, since neither a name nor a position is guaranteed to correspond
- to a case:
+ Les cas d'une énumération sont connus du compilateur ; son instance `Enumerable` est donc
+ [dérivée][dérivation] du type, et non écrite. L'instance donne les cas de l'énumération dans
+ l'ordre de leur déclaration, et recherche un cas par son nom ou par son
+ [ordinal](numbers.md#ordinals). Chaque recherche donne un `Optional`, puisque ni un nom ni une
+ position ne correspondent à coup sûr à un cas :

```scala
val enumerable = summon[Direction is Enumerable]
```

```scala fuel
run
##
Out.println(enumerable.values.inspect)
Out.println(enumerable.value(t"North").inspect)
Out.println(enumerable.value(t"Up").inspect)
Out.println(enumerable.value(Prim).inspect)
Out.println(enumerable.name(Direction.South))
Out.println(enumerable.index(Direction.East).show)
```

- This is the machinery beneath `As[Direction]`, beneath [bit flags](flags.md), and beneath the
- handling of enumerations in every format's derived codecs, so all of them agree about what a
- case is called.
+ C'est la mécanique sous-jacente à `As[Direction]`, aux [drapeaux de bits](flags.md), et au
+ traitement des énumérations dans les codecs dérivés de chaque format ; tous s'accordent donc sur
+ le nom d'un cas.

- ### Round-tripping an identifier
+ ### Aller-retour d'un identifiant

- Some conversions are not to a Scala type at all, but from one textual form to another: a
- database column named `first_name` and a field named `firstName`, or a header written
- `Content-Type` and read as `contentType`. An `Identifiable` states both directions of such a
- mapping, as a pair of functions on text:
+ Certaines conversions ne mènent pas du tout à un type Scala, mais d'une forme textuelle à une
+ autre : une colonne de base de données nommée `first_name` et un champ nommé `firstName`, ou un
+ en-tête écrit `Content-Type` et lu comme `contentType`. Un `Identifiable` énonce les deux sens
+ d'une telle correspondance, sous forme de paire de fonctions sur du texte :

```scala
case class Column()

val snakeCase = Identifiable[Column](_.uncamel.snake, _.cut(t"_").camel)
```

- The two functions convert a name into the convention and back, using the
- [case conventions](text.md#case-conventions) of text.
+ Les deux fonctions convertissent un nom vers la convention et inversement, en utilisant les
+ [conventions de casse](text.md#case-conventions) du texte.

- `Identifiable` is a `Typeclass.Pure`, so the compiler knows that the conversion captures
- nothing. It may be shared freely, and applied wherever the naming convention is needed, without
- being passed along as an argument.
+ `Identifiable` est une `Typeclass.Pure` ; le compilateur sait donc que la conversion ne capture
+ rien. Elle peut être partagée librement, et appliquée partout où la convention de nommage est
+ nécessaire, sans être transmise comme argument.

- ### Defining your own
+ ### Définir les vôtres

- The four typeclasses are where decoding is extended. Writing an instance means writing the one
- conversion function. The `as` method, the `As` extractor and the rest follow from it.
+ Les quatre classes de types sont le point d'extension du décodage. Écrire une instance revient à
+ écrire l'unique fonction de conversion. La méthode `as`, l'extracteur `As` et le reste en
+ découlent.

- A `Decodable` instance maps each form of input it recognizes to a value. This one reads a
- compass heading from a word:
+ Une instance `Decodable` associe chaque forme d'entrée qu'elle reconnaît à une valeur. Celle-ci
+ lit un cap de boussole à partir d'un mot :

```scala
enum Heading:
  case N, S, E, W

object Heading:
  given (Heading is Decodable in Text) =
    case t"north" => N
    case t"south" => S
    case t"east"  => E
    case t"west"  => W
```

```scala fuel
run
##
Out.println(t"north".as[Heading].inspect)
```

- An `Extractable` instance gives an `Optional` result, which is `Unset` for the inputs it
- declines. It can be built on an existing instance. This bounded type borrows the extractor for
- `Int`, and keeps only the values in its range:
+ Une instance `Extractable` donne un résultat `Optional`, qui vaut `Unset` pour les entrées
+ qu'elle refuse. Elle peut être construite sur une instance existante. Ce type borné emprunte
+ l'extracteur de `Int`, et ne garde que les valeurs de son intervalle :

```scala
case class Percent(value: Int)

given (Text is Extractable to Int) => Text is Extractable to Percent =
  case As[Int](number) if number >= 0 && number <= 100 => Percent(number)
  case _                                               => Unset
```

- The conversions built into Soundness follow these same shapes. A type defined this way is
- decoded with `as`, matched with `As`, and composed into the patterns of every other type, with
- no further wiring. The conversion is named once, and its fallibility lives in its type.
+ Les conversions intégrées à Soundness suivent ces mêmes formes. Un type défini ainsi se décode
+ avec `as`, se filtre avec `As`, et se compose dans les motifs de tout autre type, sans autre
+ câblage. La conversion est nommée une seule fois, et sa faillibilité réside dans son type.

- ### Related
+ ### Voir aussi

- - [JSON](json.md#case-classes-and-enumerations) and the other formats decode a whole document
-   into a case class, using a `Decodable` instance for each text field.
- - [Command-line parameters](cli.md) and [environment variables](environment.md) are decoded
-   with `as`.
- - [Patterns](patterns.md) captures the groups which `As` converts.
- - [Errors](errors.md) covers the strategies which decide what a failed decode does.
- - [Internationalization](internationalization.md), [interpolation](interpolation.md), [paths](paths.md), [sockets](sockets.md), [UUIDs](uuid.md) and [versioning](versioning.md) decode their values from text.
+ - [JSON](json.md#case-classes-and-enumerations) et les autres formats décodent un document
+   entier en une case class, en utilisant une instance `Decodable` pour chaque champ textuel.
+ - Les [paramètres de ligne de commande](cli.md) et les
+   [variables d'environnement](environment.md) se décodent avec `as`.
+ - [Motifs](patterns.md) capture les groupes que convertit `As`.
+ - [Erreurs](errors.md) couvre les stratégies qui décident de ce que fait un décodage échoué.
+ - [Internationalisation](internationalization.md), [interpolation](interpolation.md),
+   [chemins](paths.md), [sockets](sockets.md), [UUID](uuid.md) et [versionnage](versioning.md)
+   décodent leurs valeurs à partir de texte.
