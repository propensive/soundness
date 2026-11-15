- ## TEL
+ ## TEL

- ### About
+ ### Présentation

- [TEL](https://github.com/propensive/tel), the Typed Element Language, is a tree-structured
- data language, written with indentation, which has schemas. Soundness is its reference
- implementation.
+ [TEL](https://github.com/propensive/tel), le Typed Element Language, est un langage de données
+ arborescent, écrit avec indentation, qui possède des schémas. Soundness en est
+ l'implémentation de référence.

- A TEL document parses into a `Tel` value, which preserves its presentation: comments, layout
- and blank lines survive being read and written. It decodes into case classes with
- [derived][derivation] codecs. A schema, which can itself be written in TEL, assigns types to a
- document and validates it, and a document typed by a schema is navigated with the compiler
- checking each field.
+ Un document TEL s'analyse en une valeur `Tel`, qui préserve sa présentation : les
+ commentaires, la mise en page et les lignes vides survivent à la lecture et à l'écriture. Il
+ se décode en case classes grâce à des codecs [dérivés][dérivation]. Un schéma, qui peut
+ lui-même être écrit en TEL, attribue des types à un document et le valide, et un document
+ typé par un schéma se parcourt avec le compilateur vérifiant chaque champ.

- Beside the text form there is BinTEL, a compact binary encoding of the same data. It is guided
- by the schema, written as bytes for storage and transmission, and has a content hash for
- integrity.
+ À côté de la forme textuelle existe BinTEL, un encodage binaire compact des mêmes données. Il
+ est guidé par le schéma, écrit en octets pour le stockage et la transmission, et possède une
+ empreinte de contenu pour l'intégrité.

- ### On TEL
+ ### Sur TEL

- Configuration languages force a choice. YAML is friendly to people, and ambiguous. JSON is
- rigorous, and noisy. A schema language can be bolted onto either afterwards. TEL is designed to
- be all three at once:
+ Les langages de configuration imposent un choix. YAML est agréable pour les humains, et
+ ambigu. JSON est rigoureux, et verbeux. Un langage de schéma peut être greffé après coup sur
+ l'un ou l'autre. TEL est conçu pour être les trois à la fois :

- - structured by indentation, for people;
- - typed by a schema, for machines;
- - preserving of presentation, so that a program which modifies a configuration file does not
-   destroy its comments and formatting.
+ - structuré par l'indentation, pour les humains ;
+ - typé par un schéma, pour les machines ;
+ - respectueux de la présentation, de sorte qu'un programme qui modifie un fichier de
+   configuration n'en détruise pas les commentaires ni la mise en forme.

- A TEL document is a tree of *compounds*. A compound is a keyword, its *atoms*, which are
- separated by spaces, and its children, which are indented beneath it. The schema assigns each
- compound a type, and that typing is carried through to Scala. Typing a document against its
- schema at [compiletime] is
- [safety by construction](../../philosophy/safety-by-construction.md) applied to configuration.
+ Un document TEL est un arbre de *composés*. Un composé est un mot-clé, ses *atomes*, séparés
+ par des espaces, et ses enfants, indentés en dessous de lui. Le schéma attribue un type à
+ chaque composé, et ce typage est transmis jusqu'à Scala. Typer un document selon son schéma
+ [à la compilation][compilation], c'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) appliquée à la
+ configuration.

- Everything comes from the `soundness` package. The last import lets the samples print their
- results:
+ Tout provient du package `soundness`. Le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import threads.virtualThreads
import strategies.throwUnsafely
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### The language
+ ### Le langage

- Each line is a compound: a keyword, then its atoms. Indentation nests one compound inside
- another:
+ Chaque ligne est un composé : un mot-clé, puis ses atomes. L'indentation imbrique un composé
+ dans un autre :

```
name Hamlet
age 30
address
  street 1 High Street
  city Elsinore
```

- ### Parsing and decoding
+ ### Analyse et décodage

- `read[Tel]` parses text into a `Tel`, and `as` decodes it to a case class. Each field is read
- from the child compound with the matching keyword. `read[Person in Tel]` does both steps at
- once:
+ `read[Tel]` analyse un texte en un `Tel`, et `as` le décode en une case class. Chaque champ
+ est lu dans le composé enfant dont le mot-clé correspond. `read[Person in Tel]` fait les deux
+ étapes à la fois :

```scala
case class Person(name: Text, age: Int)

val hamlet = t"name Hamlet\nage 30\n".read[Tel].as[Person]
val horatio = t"name Horatio\nage 32\n".read[Person in Tel]
```

```scala fuel
run
##
Out.println(hamlet.inspect)
Out.println(horatio.inspect)
```

- A document which lacks a field, or holds a value of the wrong type, does not decode:
+ Un document auquel il manque un champ, ou qui contient une valeur du mauvais type, ne se
+ décode pas :

```scala fuel
run
errors show
##
t"name Ophelia\n".read[Person in Tel]
```

```scala fuel
run
errors show
##
t"name Yorick\nage unknown\n".read[Person in Tel]
```

- A single source may hold several documents in sequence, and is read as a list of them with
- `read[List[Tel]]`.
+ Une même source peut contenir plusieurs documents à la suite, et se lit comme une liste de
+ documents avec `read[List[Tel]]`.

- ### Encoding
+ ### Encodage

- `in[Tel]` produces a `Tel` from a value, with a compound for each field, ready to be written
- out or embedded in a larger document. `show` writes a `Tel` as text:
+ `in[Tel]` produit un `Tel` à partir d'une valeur, avec un composé pour chaque champ, prêt à
+ être écrit ou intégré dans un document plus grand. `show` écrit un `Tel` sous forme de texte :

```scala fuel
run
##
Out.println(Person(t"Hamlet", 30).in[Tel].show)
```

- `in[Text over Tel]` encodes a value straight to its TEL text, and `in[Data over Tel]` writes
- the same text as bytes, in the `Codepage` in scope.
+ `in[Text over Tel]` encode une valeur directement en son texte TEL, et `in[Data over Tel]`
+ écrit le même texte sous forme d'octets, dans la `Codepage` dans la portée.

- `show` renders a `Tel` into one `Text`. A large document need not be held in memory before it
- is sent:
+ `show` rend un `Tel` en un seul `Text`. Un grand document n'a pas besoin d'être tenu en
+ mémoire avant d'être envoyé :

```tabular
- column  Form
- column  Serializes on
- column  Hands out
- row  `show`  the caller's thread  one `Text`
- row  `Tel.emit[Text](document, f)`  the caller's thread  each block of text to `f`, as it fills
- row  `Tel.emit[Data](document, f)`  the caller's thread  each block as UTF-8 bytes to `f`, as it fills
- row  `Tel.lend(document)`  the caller's thread  each block lent to a function, not copied
- row  `Tel.emit(document)`  a fiber  the text, through an iterator
- row  `Streamable` by `Text`  a fiber  the text, as a stream
+ column  Forme
+ column  Sérialise sur
+ column  Délivre
+ row  `show`  le thread de l'appelant  un seul `Text`
+ row  `Tel.emit[Text](document, f)`  le thread de l'appelant  chaque bloc de texte à `f`, à mesure qu'il se remplit
+ row  `Tel.emit[Data](document, f)`  le thread de l'appelant  chaque bloc en octets UTF-8 à `f`, à mesure qu'il se remplit
+ row  `Tel.lend(document)`  le thread de l'appelant  chaque bloc prêté à une fonction, sans copie
+ row  `Tel.emit(document)`  une fibre  le texte, par un itérateur
+ row  `Streamable` par `Text`  une fibre  le texte, sous forme de flux
```

- The push forms suit writing to a file, a [socket](sockets.md) or an `OutputStream`. The pull
- forms suit a consumer which asks for each block:
+ Les formes « push » conviennent à l'écriture vers un fichier, un [socket](sockets.md) ou un
+ `OutputStream`. Les formes « pull » conviennent à un consommateur qui demande chaque bloc :

```scala fuel
typecheck
##
import probates.cancelProbate

val outgoing = Person(t"Hamlet", 30).in[Tel]

Tel.emit[Data](outgoing, chunk => socket.write(chunk))

Tel.lend(outgoing): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))

supervise:
  Tel.emit(outgoing).each(chunk => out.write(chunk))
```

- [JSON](json.md#formatting) describes each of these forms.
+ [JSON](json.md#formatting) décrit chacune de ces formes.

- ### Optional fields
+ ### Champs optionnels

- An `Optional` field has two policies, from the package `optionalityOptions`, as
- [Decoding](decoding.md#policies-for-optional-fields) describes:
+ Un champ `Optional` obéit à deux politiques, issues du package `optionalityOptions`, comme le
+ décrit [Décodage](decoding.md#policies-for-optional-fields) :

```tabular
- column  A field whose keyword is
- column  Reads as
- column  Unless
- row  missing, or present with neither an atom nor a child  `Unset`  `strictTelAbsence` makes it an error
- row  present but rejected by the inner decoder  an error  `lenientTelFaults` reads it as `Unset`
+ column  Un champ dont le mot-clé est
+ column  Se lit comme
+ column  Sauf si
+ row  absent, ou présent sans atome ni enfant  `Unset`  `strictTelAbsence` en fait une erreur
+ row  présent mais rejeté par le décodeur interne  une erreur  `lenientTelFaults` le lit comme `Unset`
```

- A missing keyword and an empty one are TEL's two spellings of absence. Under
- `strictTelAbsence`, absence is a `Tel.Error.Reason.Absent`, and the field is marked as required
- in the derived schema. The direct parser enforces the policy for absence only.
+ Un mot-clé absent et un mot-clé vide sont les deux graphies de l'absence en TEL. Sous
+ `strictTelAbsence`, l'absence est une `Tel.Error.Reason.Absent`, et le champ est marqué comme
+ obligatoire dans le schéma dérivé. L'analyseur direct n'applique la politique qu'à
+ l'absence.

```scala
case class Flags(active: Boolean, verbose: Optional[Boolean])

val tolerant =
  import optionalityOptions.lenientTelFaults
  t"active false\nverbose maybe\n".read[Tel].as[Flags]
```

```scala fuel
run
##
Out.println(tolerant.inspect)
```

- ### Writing TEL literally
+ ### Écrire du TEL littéralement

- The `tel"…"` [interpolator] writes a literal document. It is parsed at compiletime, and each
- substitution is encoded through its own static type:
+ L'[interpolateur] `tel"…"` écrit un document littéral. Il est analysé à la compilation, et
+ chaque substitution est encodée selon son propre type statique :

```scala
val name = t"Hamlet"
val contact = tel"name $name"
```

- The same syntax is a pattern, which binds the values it names:
+ La même syntaxe est un motif, qui lie les valeurs qu'il nomme :

```scala
val matched = contact match
  case tel"name $name" => name.as[Text]
  case _               => t"anonymous"
```

```scala fuel
run
##
Out.println(contact.show)
Out.println(matched)
```

- A malformed literal is a compile error, positioned at the offending part of the literal, not
- at the start of the string.
+ Un littéral mal formé est une erreur de compilation, positionnée sur la partie fautive du
+ littéral, et non au début de la chaîne.

- ### Verified navigation
+ ### Navigation vérifiée

- `verify` checks a document against the shape of a type, and gives it that type. Its fields are
- then navigated with full compiletime checking:
+ `verify` vérifie un document par rapport à la forme d'un type, et lui donne ce type. Ses
+ champs se parcourent alors avec une vérification complète à la compilation :

```scala
case class Castle(name: Text, city: Text)
case class Residence(prince: Person, castle: Castle)

val doc = Residence(Person(t"Hamlet", 30), Castle(t"Kronborg", t"Elsinore")).in[Tel]
```

```scala fuel
run
##
Out.println(doc.verify[Residence].castle.city.as[Text])
```

- A misspelled field, or a field the type does not have, does not compile:
+ Un champ mal orthographié, ou un champ que le type ne possède pas, ne compile pas :

```scala fuel
compile
errors show
##
doc.verify[Residence].castle.town
```

- Unverified, dynamic access reaches into a document whose shape is known only informally. It is
- enabled by `import dynamicAccess.dynamicTel` or, for a single block, by `dynamically[Tel]:`, so
- the checked and unchecked styles stay visibly distinct, as they do for
- [JSON](json.md#fields-as-members).
+ L'accès dynamique, non vérifié, permet d'explorer un document dont la forme n'est connue que
+ de façon informelle. Il s'active par `import dynamicAccess.dynamicTel` ou, pour un seul bloc,
+ par `dynamically[Tel]:`, si bien que les styles vérifié et non vérifié restent visiblement
+ distincts, comme pour [JSON](json.md#fields-as-members).

- A keyword may legitimately repeat, which an accessor for a single value cannot express.
- `fields` gives every matching child, in document order:
+ Un mot-clé peut légitimement se répéter, ce qu'un accesseur pour une valeur unique ne peut
+ pas exprimer. `fields` donne chaque enfant correspondant, dans l'ordre du document :

```scala fuel
run
##
Out.println(t"item 1\nitem 2\nitem 3\n".read[Tel].fields(t"item").map(_.primaryAtom).to[List].inspect)
```

- ### Paths
+ ### Chemins

- A location in a document is a `Telp`, a TEL path. Its first character chooses the delimiter.
- Each further component names a keyword, or selects one occurrence of a repeatable keyword by a
- key value or an index. A `telp"…"` literal is checked at compiletime, and a path is the same
- path whichever delimiter writes it:
+ Un emplacement dans un document est un `Telp`, un chemin TEL. Son premier caractère choisit le
+ délimiteur. Chaque composante suivante nomme un mot-clé, ou sélectionne une occurrence d'un
+ mot-clé répétable par une valeur de clé ou par un indice. Un littéral `telp"…"` est vérifié à
+ la compilation, et un chemin est le même chemin quel que soit le délimiteur qui l'écrit :

```scala fuel
run
##
Out.println((telp"/castle/city" == telp".castle.city").inspect)
```

- A literal which is not a path does not compile:
+ Un littéral qui n'est pas un chemin ne compile pas :

```scala fuel
compile
errors show
##
telp"castle/city"
```

- The path which a decoding error is tagged with, such as `/castle/city`, is itself a `Telp`.
+ Le chemin dont est étiquetée une erreur de décodage, tel que `/castle/city`, est lui-même un
+ `Telp`.

- ### Editing without destroying the file
+ ### Modifier sans détruire le fichier

- A TEL document keeps its comments, blank lines and layout so that a program can change one
- value and write the file back, without reformatting everything a person wrote.
+ Un document TEL conserve ses commentaires, ses lignes vides et sa mise en page, si bien qu'un
+ programme peut changer une valeur et réécrire le fichier, sans reformater tout ce qu'une
+ personne a écrit.

- `modify` replaces a field's compound, or appends it where the field is absent:
+ `modify` remplace le composé d'un champ, ou l'ajoute là où le champ est absent :

```scala
import dynamicAccess.dynamicTel

val document = t"# The account holder\nname Hamlet\n\nage 30\n".read[Tel]
val renamed = document.modify("name", Tel.scalar(t"Horatio"))
```

```scala fuel
run
##
Out.println(renamed.show)
```

- Finer edits are *mutations*, addressed by a pointer. A mutation changes exactly what it names,
- and leaves the surrounding presentation alone. It may rewrite a single atom of a line, attach a
- remark to a compound, or insert or remove a child:
+ Les modifications plus fines sont des *mutations*, adressées par un pointeur. Une mutation
+ change exactement ce qu'elle nomme, et laisse intacte la présentation environnante. Elle peut
+ réécrire un seul atome d'une ligne, attacher une remarque à un composé, ou insérer ou retirer
+ un enfant :

```scala
val pointer = Tel.Pointer.of(t"name")
val mutated = Tel.Mutation(document, List(Tel.Mutation.Op.UpdateAtom(pointer, 0, t"Horatio")))
```

```scala fuel
run
##
Out.println(mutated.show)
```

- The result is a document, not a string, so a sequence of edits composes, and the file is
- written once at the end.
+ Le résultat est un document, et non une chaîne, si bien qu'une suite de modifications se
+ compose, et que le fichier est écrit une seule fois à la fin.

- ### Schemas
+ ### Schémas

- A `Tels` is a TEL schema: the structure a document must have, with named definitions of
- records, scalars and selects, which compose in layers. A schema is derived from a Scala type,
- or is itself read from a TEL document, since the schema language is TEL:
+ Un `Tels` est un schéma TEL : la structure qu'un document doit avoir, avec des définitions
+ nommées d'enregistrements, de scalaires et de sélections, qui se composent en couches. Un
+ schéma est dérivé d'un type Scala, ou est lui-même lu à partir d'un document TEL, puisque le
+ langage de schéma est TEL :

```scala
val schema = Tels.tels[Person](t"person")
```

- Each record or select in a derived schema is named after its type's simple name. Where two
- different types reached from the root share a simple name, such as `Phrase.Code` and
- `Element.Code`, each is qualified by its enclosing type, as `PhraseCode` and `ElementCode`. A
- derived schema whose names cannot be told apart is rejected (E210), as a hand-written one with
- two definitions of one name is.
+ Chaque enregistrement ou sélection d'un schéma dérivé est nommé d'après le nom simple de son
+ type. Lorsque deux types différents atteints depuis la racine partagent un nom simple, comme
+ `Phrase.Code` et `Element.Code`, chacun est qualifié par son type englobant, sous la forme
+ `PhraseCode` et `ElementCode`. Un schéma dérivé dont les noms ne peuvent pas être distingués
+ est rejeté (E210), comme l'est un schéma écrit à la main comportant deux définitions d'un
+ même nom.

- `Tel.Type.assign` types a document against a schema. It validates the document, and reports
- failures as typed errors, each with a path to the offending element. Under a strategy which
- [accumulates failures](errors.md#accumulating-failures), every fault in a document is collected
- and reported together.
+ `Tel.Type.assign` type un document selon un schéma. Il valide le document, et signale les
+ échecs sous forme d'erreurs typées, chacune avec un chemin vers l'élément fautif. Sous une
+ stratégie qui [accumule les échecs](errors.md#accumulating-failures), chaque défaut d'un
+ document est collecté et signalé avec les autres.

- A schema is built in *layers*: a base structure, and overlays which add or refine fields. This
- is how a schema shared between several programs is extended by one of them without forking it,
- and how the fields of an optional feature appear only where the feature is in use.
+ Un schéma se construit en *couches* : une structure de base, et des surcouches qui ajoutent ou
+ affinent des champs. C'est ainsi qu'un schéma partagé entre plusieurs programmes est étendu
+ par l'un d'eux sans être dupliqué, et que les champs d'une fonctionnalité optionnelle
+ n'apparaissent que là où la fonctionnalité est utilisée.

- The values of fields are checked by named *validators*. The built-in registry covers the kinds
- a configuration language needs: `string`, which accepts anything, `identifier`, which accepts
- kebab-case names and rejects a leading hyphen, and the numeric and enumerated kinds. A schema
- may name validators of its own, so that a constraint of the domain lives in the schema, not in
- every program which reads it.
+ Les valeurs des champs sont vérifiées par des *validateurs* nommés. Le registre intégré
+ couvre les sortes dont a besoin un langage de configuration : `string`, qui accepte tout,
+ `identifier`, qui accepte les noms en kebab-case et rejette un tiret initial, et les sortes
+ numériques et énumérées. Un schéma peut nommer ses propres validateurs, si bien qu'une
+ contrainte du domaine vit dans le schéma, et non dans chaque programme qui le lit.

- A scalar may also be constrained by one or more RE2 *patterns*, which must match the whole
- value, and which combine by intersection:
+ Un scalaire peut aussi être contraint par un ou plusieurs *motifs* RE2, qui doivent
+ correspondre à la valeur entière, et qui se combinent par intersection :

```
scalar Code
  pattern [A-Z]{2}-[0-9]{4}
```

- Patterns are the one constraint whose meaning a schema can inspect. A validator is just a name,
- so a layer may only ever add validators. But RE2 excludes backreferences and lookaround, which
- makes it decidable whether one pattern's language contains another's. A layer may therefore
- *replace* a scalar's patterns, provided the new language is contained in the old. Widening is
- rejected, and so is a containment which the analysis cannot prove within its budget. The check
- fails closed, so a layer can never loosen what the base accepted.
- [Patterns](patterns.md) describes the RE2 engine.
+ Les motifs sont la seule contrainte dont un schéma peut examiner le sens. Un validateur n'est
+ qu'un nom, si bien qu'une couche ne peut jamais qu'ajouter des validateurs. Mais RE2 exclut
+ les références arrière et les assertions avant et arrière, ce qui rend décidable la question
+ de savoir si le langage d'un motif contient celui d'un autre. Une couche peut donc
+ *remplacer* les motifs d'un scalaire, pourvu que le nouveau langage soit contenu dans
+ l'ancien. L'élargissement est rejeté, de même qu'une inclusion que l'analyse ne peut pas
+ prouver dans les limites de son budget. La vérification échoue en position fermée, si bien
+ qu'une couche ne peut jamais assouplir ce que la base acceptait.
+ [Motifs](patterns.md) décrit le moteur RE2.

- ### Recovering from errors
+ ### Se rétablir après des erreurs

- A language structured by indentation is easy to get slightly wrong, and a parser which gives up
- at the first mistake is unhelpful to a person editing a file. Parsing therefore recovers, and
- the schema informs the recovery. A line which is indented ambiguously is attached to the
- candidate which the schema admits. A `child` keyword which is valid inside `parent`, but not at
- the root, is recovered to the deeper position.
+ Un langage structuré par l'indentation est facile à écrire légèrement de travers, et un
+ analyseur qui abandonne à la première erreur n'aide guère une personne qui modifie un
+ fichier. L'analyse se rétablit donc, et le schéma guide ce rétablissement. Une ligne indentée
+ de façon ambiguë est rattachée au candidat que le schéma admet. Un mot-clé `child` valide à
+ l'intérieur de `parent`, mais pas à la racine, est rétabli à la position plus profonde.

- Errors are reported with their line and column. A document whose faults were recovered still
- yields a tree, which is what an editor, a [language server](lsp.md), or a tool reporting
- several problems at once requires.
+ Les erreurs sont signalées avec leur ligne et leur colonne. Un document dont les défauts ont
+ été rétablis donne tout de même un arbre, ce dont a besoin un éditeur, un
+ [serveur de langage](lsp.md), ou un outil qui signale plusieurs problèmes à la fois.

- ### Typed records from a schema
+ ### Enregistrements typés à partir d'un schéma

- Where the schema is authoritative, a `Tel.Provider` reads it at compiletime, and produces typed
- records from matching documents. The provider is an object, declared in a file of its own,
- which holds the schema. Each field is read as its schema declares it:
+ Lorsque le schéma fait autorité, un `Tel.Provider` le lit à la compilation, et produit des
+ enregistrements typés à partir des documents correspondants. Le fournisseur est un objet,
+ déclaré dans un fichier à part, qui contient le schéma. Chaque champ se lit tel que son
+ schéma le déclare :

```tabular
- column  The schema declares
- column  The field reads as
- row  a scalar  the type its validator names
- row  an optional field  an `Optional`, absent where the document omits it
- row  a required field  its value, failing with a `Tel.Provider.Error` when it is missing
- row  a repeatable field  a `List` of every occurrence
- row  a struct, or a reference to a record definition  a nested record
- row  a select  one optional field per variant
- row  a *flag*, a keyword with no value  a `Boolean`: `true` where present, `false` where not
+ column  Le schéma déclare
+ column  Le champ se lit comme
+ row  un scalaire  le type que nomme son validateur
+ row  un champ optionnel  un `Optional`, absent là où le document l'omet
+ row  un champ obligatoire  sa valeur, échouant avec une `Tel.Provider.Error` lorsqu'il manque
+ row  un champ répétable  une `List` de chaque occurrence
+ row  une structure, ou une référence à une définition d'enregistrement  un enregistrement imbriqué
+ row  une sélection  un champ optionnel par variante
+ row  un *drapeau*, un mot-clé sans valeur  un `Boolean` : `true` là où il est présent, `false` sinon
```

- A missing required field fails through the [tactic] of the code which reads it. The built-in
- scalars are provided:
+ Un champ obligatoire manquant échoue par la [tactique] du code qui le lit. Les scalaires
+ intégrés sont fournis :

```tabular
- column  Scalar
- column  Validator
- column  Reads as
- row  `String`  `string`  `Text`
- row  `Identifier`  `identifier`  `Text`
- row  `TypeName`  `type-name`  `Text`
- row  `Sigil`  `sigil`  `Char`, failing on an atom of more than one character
+ column  Scalaire
+ column  Validateur
+ column  Se lit comme
+ row  `String`  `string`  `Text`
+ row  `Identifier`  `identifier`  `Text`
+ row  `TypeName`  `type-name`  `Text`
+ row  `Sigil`  `sigil`  `Char`, échouant sur un atome de plus d'un caractère
```

- A custom validator is read through a given with the same label, in scope where `record` is
- called, such as
- `given ("handle" is Intensional in Tel.Provider from Tel to Handle) = Intensional(…)`.
+ Un validateur personnalisé se lit par un given portant la même étiquette, dans la portée là
+ où `record` est appelé, comme
+ `given ("handle" is Intensional in Tel.Provider from Tel to Handle) = Intensional(…)`.

- A provider, and a record read through it:
+ Un fournisseur, et un enregistrement lu par son intermédiaire :

```scala fuel
typecheck
##
object ContactRecords extends Tel.Provider(Tels.tels[Person](t"person"))
```

```scala fuel
typecheck
##
val record = ContactRecords.record(t"name Hamlet\nage 30\n".read[Tel])
val recordName: Text = record.name
val recordAge: Int = record.age
```

- A field which the schema does not describe is a compile error. The shape of the data is taken
- from the schema and checked by the compiler, with no Scala type restating it.
- [JSON](json.md#typed-records-from-a-schema) and
- [XML](xml.md#typed-records-from-an-xml-schema) have the same facility, and
- [Records](records.md) describes the records themselves.
+ Un champ que le schéma ne décrit pas est une erreur de compilation. La forme des données est
+ tirée du schéma et vérifiée par le compilateur, sans qu'aucun type Scala ne la reformule.
+ [JSON](json.md#typed-records-from-a-schema) et
+ [XML](xml.md#typed-records-from-an-xml-schema) offrent la même possibilité, et
+ [Enregistrements](records.md) décrit les enregistrements eux-mêmes.

- ### BinTEL
+ ### BinTEL

- A typed document encodes to BinTEL, a compact binary form guided by the schema, and decodes
- back. A *value hash* fingerprints the content, for integrity checks:
+ Un document typé s'encode en BinTEL, une forme binaire compacte guidée par le schéma, et se
+ décode en retour. Une *empreinte de valeur* identifie le contenu, pour les contrôles
+ d'intégrité :

```scala
val binary = document.bintel(schema)
val hash = document.valueHash(schema)
```

- The value hash is taken over the document's content, not its bytes. Two documents which differ
- only in comments, whitespace or the order of keys have the same hash, while a document whose
- values differ does not. It is therefore usable as a cache key, or to detect a change, where
- hashing the file would report a change whenever someone reflowed it.
+ L'empreinte de valeur est calculée sur le contenu du document, et non sur ses octets. Deux
+ documents qui ne diffèrent que par les commentaires, les espaces ou l'ordre des clés ont la
+ même empreinte, alors qu'un document dont les valeurs diffèrent ne l'a pas. Elle peut donc
+ servir de clé de cache, ou à détecter une modification, là où le hachage du fichier
+ signalerait une modification chaque fois que quelqu'un le remet en forme.

- A *self-contained* BinTEL frame carries its schema with it, so a document can travel to a
- reader which has never seen its schema, and still be decoded:
+ Une trame BinTEL *autonome* porte son schéma avec elle, si bien qu'un document peut parvenir
+ à un lecteur qui n'a jamais vu son schéma, et être tout de même décodé :

```scala
val greeting = t"name Hamlet\n".read[Tel]
val greetingSchema = t"name greeting\n\ndocument\n  field name Identifier\n".read[Tel]

val frame = Bintel.selfContained(greeting, greetingSchema)
```

- The alternative is to send the data alone, and identify the schema by its *signature*. A
- signature is a short fingerprint computed from the schema itself, so a reader can confirm that
- it holds the right schema before it decodes a byte of data. A schema with no layers has a
- signature of thirty-three bytes.
+ L'autre possibilité est d'envoyer les données seules, et d'identifier le schéma par sa
+ *signature*. Une signature est une courte empreinte calculée à partir du schéma lui-même, si
+ bien qu'un lecteur peut confirmer qu'il détient le bon schéma avant de décoder le moindre
+ octet de données. Un schéma sans couches a une signature de trente-trois octets.

- Numbers in BinTEL are encoded in a variable number of bytes, and values are typed by the
- schema, not tagged in the stream, which is where the compactness comes from. Every framed
- document also declares its own length, immediately after the magic number. A reader can
- therefore delimit, skip or forward documents without holding their schema, and several
- documents may share one stream:
+ Les nombres en BinTEL sont encodés sur un nombre variable d'octets, et les valeurs sont
+ typées par le schéma, et non étiquetées dans le flux, d'où la compacité. Chaque document
+ tramé déclare aussi sa propre longueur, juste après le nombre magique. Un lecteur peut donc
+ délimiter, sauter ou transmettre des documents sans détenir leur schéma, et plusieurs
+ documents peuvent partager un même flux :

```tabular
- column  Method
- column  Reads
- row  `Bintel.decodeStream`  every document, in order
- row  `Bintel.decodeDocument`  one document, reporting where its continuation begins
- row  `Bintel.decodeWholeDocument`  one document, insisting that nothing follows
+ column  Méthode
+ column  Lit
+ row  `Bintel.decodeStream`  chaque document, dans l'ordre
+ row  `Bintel.decodeDocument`  un document, en indiquant où commence la suite
+ row  `Bintel.decodeWholeDocument`  un document, en exigeant que rien ne le suive
```

- ### RaTEL
+ ### RaTEL

- BinTEL is read from front to back. *RaTEL*, the random-access encoding, is read in place: it
- lays a document out as tables of fixed-width rows, one table for each record type, with sorted
- indexes beside them. A program which [maps the file](filesystem.md#positional-access) finds one
- record by touching that record and nothing else, and several programs which map it share one
- copy. It is the form for a large document which one program writes and others consult.
+ BinTEL se lit du début à la fin. *RaTEL*, l'encodage à accès aléatoire, se lit sur place : il
+ dispose un document en tables de lignes à largeur fixe, une table par type d'enregistrement,
+ avec des index triés à côté. Un programme qui
+ [mappe le fichier](filesystem.md#positional-access) trouve un enregistrement en touchant cet
+ enregistrement et rien d'autre, et plusieurs programmes qui le mappent en partagent une
+ seule copie. C'est la forme d'un grand document qu'un programme écrit et que d'autres
+ consultent.

- A schema says how its records are found. A record's `key` field gives the records of a
- repeatable field a *primary index*, and an `index` beneath the field declares another, over one
- or more paths into each record. A scalar with a standard encoding, such as `u32`, is stored in
- its row at a fixed width, and one marked `intern` has each distinct value stored once:
+ Un schéma indique comment ses enregistrements sont trouvés. Le champ `key` d'un enregistrement
+ donne aux enregistrements d'un champ répétable un *index primaire*, et un `index` sous le
+ champ en déclare un autre, sur un ou plusieurs chemins dans chaque enregistrement. Un
+ scalaire doté d'un encodage standard, comme `u32`, est stocké dans sa ligne à largeur fixe,
+ et un scalaire marqué `intern` voit chaque valeur distincte stockée une seule fois :

```scala
val castSchema = t"""
  tel 1.0 specification.tel/tels:2.1.0

  name cast

  scalar Count
    encoding u32

  record Role
    field name String key
    field play String
    field lines Count optional

  document
    field role Role repeatable
      index /play
""".read[Tel].as[Tels]

val cast = t"role Hamlet\n  play Hamlet\n  lines 1506\nrole Viola\n  play Twelfth Night\n".read[Tel]
```

- `ratel` encodes a document under its schema as a `Ratel`, an image which carries the schema
- with it. An image is `Encodable in Data`, so it is
- [published](filesystem.md#publishing-versions) like any other value:
+ `ratel` encode un document selon son schéma sous forme de `Ratel`, une image qui porte le
+ schéma avec elle. Une image est `Encodable in Data`, si bien qu'elle est
+ [publiée](filesystem.md#publishing-versions) comme n'importe quelle autre valeur :

```scala
val image: Ratel = cast.ratel(castSchema)
file.claim[Ratel](_.publish(image))
```

- A file is opened as `Ratel`, like a file in any other form. It is mapped for the length of
- the block, and the block is given a reader, `ratel`. Opening checks only what decides how the
- rest is read: the header, the schema and its signature, and the directories of tables and
- indexes. Records are then read as they are asked for. `/` gives the records of a repeatable
- field, `find` looks one up by its key with a binary search of the primary index, and `where`
- gives those with a value at an indexed path:
+ Un fichier s'ouvre en tant que `Ratel`, comme un fichier sous toute autre forme. Il est mappé
+ pour la durée du bloc, et le bloc reçoit un lecteur, `ratel`. L'ouverture ne vérifie que ce
+ qui détermine comment le reste est lu : l'en-tête, le schéma et sa signature, et les
+ répertoires des tables et des index. Les enregistrements sont ensuite lus au fur et à mesure
+ qu'on les demande. `/` donne les enregistrements d'un champ répétable, `find` en recherche un
+ par sa clé avec une recherche dichotomique dans l'index primaire, et `where` donne ceux qui
+ ont une valeur à un chemin indexé :

```scala
file.open[Ratel]():
  val roles = ratel.root / t"role"
  val hamlet = roles.find(t"Hamlet")
  val lines = hamlet.let(_.text(t"lines"))
  val twelfth = roles.where(t"/play", t"Twelfth Night")
```

- An image in memory, and a `Terrain` view which is already in hand, such as the one a
- [published](filesystem.md#publishing-versions) reader lends, are opened the same way, as
- `image.open[Ratel]()` and `view.open[Ratel]()`.
+ Une image en mémoire, et une vue `Terrain` déjà en main, comme celle que prête un lecteur
+ [publié](filesystem.md#publishing-versions), s'ouvrent de la même façon, sous la forme
+ `image.open[Ratel]()` et `view.open[Ratel]()`.

- A `Ratel.Row` reads its members by keyword:
+ Un `Ratel.Row` lit ses membres par mot-clé :

```tabular
- column  Method
- column  Reads
- row  `text`  a scalar, as its text, or `Unset` if it is absent
- row  `data`  a scalar, as the bytes which are stored
- row  `texts`  the values of a repeatable scalar
- row  `flag`, `count`  whether a flag is present, and how many times
- row  `record`  a struct which is not repeatable, as another row
- row  `/`  the records of a repeatable struct, as a `Ratel.Rows`
- row  `has`  whether an optional member is present
- row  `as`  the whole record, decoded as a value
+ column  Méthode
+ column  Lit
+ row  `text`  un scalaire, sous forme de texte, ou `Unset` s'il est absent
+ row  `data`  un scalaire, sous forme des octets stockés
+ row  `texts`  les valeurs d'un scalaire répétable
+ row  `flag`, `count`  si un drapeau est présent, et combien de fois
+ row  `record`  une structure non répétable, sous forme d'une autre ligne
+ row  `/`  les enregistrements d'une structure répétable, sous forme de `Ratel.Rows`
+ row  `has`  si un membre optionnel est présent
+ row  `as`  l'enregistrement entier, décodé en valeur
```

- A `Ratel.Rows` has a `size`, is read by position or with `each`, and has `find`, `where` and
- `sorted`, which gives its records in the order of their keys. A row is a plain value which
- says where a record lies, and is read through the reader which its block is given, so it is
- read only while its image is open. Its type, `Ratel.Row of brand`, carries the brand of that
- reader. Each accessor asks for the reader with that brand, so where several images are open a
- row is read through its own, and a row which is taken to where its reader is not in scope
- does not compile there. `as` decodes one record, and whatever it holds, through the same
- typeclasses as a TEL document:
+ Un `Ratel.Rows` a une `size`, se lit par position ou avec `each`, et dispose de `find`,
+ `where` et `sorted`, qui donne ses enregistrements dans l'ordre de leurs clés. Une ligne est
+ une simple valeur qui indique où se trouve un enregistrement, et se lit par le lecteur que
+ reçoit son bloc, si bien qu'elle ne se lit que tant que son image est ouverte. Son type,
+ `Ratel.Row of brand`, porte la marque de ce lecteur. Chaque accesseur demande le lecteur de
+ cette marque, si bien que, lorsque plusieurs images sont ouvertes, une ligne se lit par le
+ sien, et qu'une ligne emportée là où son lecteur n'est pas dans la portée n'y compile pas.
+ `as` décode un enregistrement, et tout ce qu'il contient, par les mêmes classes de types
+ qu'un document TEL :

```scala
case class Role(name: Text, play: Text, lines: Optional[Int])

val role = file.open[Ratel]():
  (ratel.root / t"role").find(t"Hamlet").let(_.as[Role])
```

- An object which extends `Ratel.Provider` with a schema reads the records of an image without
- a case class for each. It gives the root of an open image as a record whose members are those
- of the schema's document, each typed as the schema types it, and each read in place when it is
- selected:
+ Un objet qui étend `Ratel.Provider` avec un schéma lit les enregistrements d'une image sans
+ case class pour chacun. Il donne la racine d'une image ouverte sous forme d'un enregistrement
+ dont les membres sont ceux du document du schéma, chacun typé comme le schéma le type, et
+ chacun lu sur place lorsqu'il est sélectionné :

- <!-- doccheck: skip -->
+ <!-- doccheck: skip -->
```scala
object Cast extends Ratel.Provider(castSchema)

file.open[Ratel]():
  val cast = Cast()
  val hamlet = cast.role(t"Hamlet")            // an optional record, by its key
  val play: Optional[Text] = hamlet.let(_.play)
  val lines: Optional[Long] = hamlet.let(_.lines)
  val comedy = cast.role.where(_.play = t"Twelfth Night")
```

- A `Ratel.Provider` is a [specification](records.md#defining-a-specification), as
- `Tel.Provider` is, whose records are read in the context of an open image. A scalar is read
- through the `Intensional` instance named by its encoding, or by its first validator if it has
- none: as `Text`, or as `Int`, `Long` or `Double` if its encoding is a standard codec whose
- every value one of them holds exactly, as `u32` is by `Long`, or as a type of your own, by an
- instance of your own. A flag is a `Boolean`, and a repeatable flag the `Long` number of times
- it occurs. A struct is a nested record, a repeatable scalar a `List`, and a repeatable struct
- a `Ratel.Table`, which has the `size`, `each` and `sorted` of a `Ratel.Rows`, and is
- applied to a key or a position. Its `where` finds records by a secondary index, and names the
- indexed scalar as a [lens](optics.md) names what it updates, by a path which ends in an
- assignment, such as `_.play = t"Twelfth Night"` or `_.author.born = 1564L`: the path is
- checked against the record, and the value against the type its member is read as. Each variant of a select is a member which is absent unless
- it is the one chosen, and a member which is optional is an `Optional`. A member the schema
- does not declare is not a member of the record, and does not compile. The provider's object
- must be compiled before the code which uses it, so it is written in a file of its own.
- Opening an image which was written with another schema raises a `Ratel.Error`, of
- `Mismatched`.
+ Un `Ratel.Provider` est une [spécification](records.md#defining-a-specification), comme
+ l'est `Tel.Provider`, dont les enregistrements se lisent dans le contexte d'une image
+ ouverte. Un scalaire se lit par l'instance `Intensional` que nomme son encodage, ou son
+ premier validateur s'il n'en a pas : comme `Text`, ou comme `Int`, `Long` ou `Double` si son
+ encodage est un codec standard dont chaque valeur est contenue exactement par l'un d'eux,
+ comme `u32` l'est par `Long`, ou comme un type qui vous est propre, par une instance qui vous
+ est propre. Un drapeau est un `Boolean`, et un drapeau répétable le nombre `Long` de ses
+ occurrences. Une structure est un enregistrement imbriqué, un scalaire répétable une `List`,
+ et une structure répétable une `Ratel.Table`, qui a les `size`, `each` et `sorted` d'un
+ `Ratel.Rows`, et s'applique à une clé ou à une position. Son `where` trouve des
+ enregistrements par un index secondaire, et nomme le scalaire indexé comme une
+ [lentille](optics.md) nomme ce qu'elle met à jour, par un chemin qui se termine par une
+ affectation, comme `_.play = t"Twelfth Night"` ou `_.author.born = 1564L` : le chemin est
+ vérifié par rapport à l'enregistrement, et la valeur par rapport au type sous lequel son
+ membre est lu. Chaque variante d'une sélection est un membre absent à moins d'être celle qui
+ est choisie, et un membre optionnel est un `Optional`. Un membre que le schéma ne déclare pas
+ n'est pas un membre de l'enregistrement, et ne compile pas. L'objet du fournisseur doit être
+ compilé avant le code qui l'utilise ; il s'écrit donc dans un fichier à part. Ouvrir une
+ image écrite avec un autre schéma lève une `Ratel.Error`, de raison `Mismatched`.

- A reader which opens a file it did not write trusts nothing in it: every offset is checked
- before it is followed, and a fault is raised as a `Ratel.Error`, by the access which meets it,
- while other records remain readable. The reader's `verify` checks the whole image at once, its
- digests, its indexes against its rows and its value hash, and opening with `Ratel.Flag.Verify`
- does so before the block runs. `materialise` gives the whole document back as the model it was
- written from, so the three encodings convert between one another without loss. An encoding
- which is the application's own is bound to its codec with `Ratel.Flag.Codecs`.
+ Un lecteur qui ouvre un fichier qu'il n'a pas écrit ne s'y fie en rien : chaque décalage est
+ vérifié avant d'être suivi, et un défaut est levé sous forme de `Ratel.Error`, par l'accès
+ qui le rencontre, tandis que les autres enregistrements restent lisibles. Le `verify` du
+ lecteur vérifie toute l'image d'un coup, ses empreintes, ses index par rapport à ses lignes
+ et son empreinte de valeur, et ouvrir avec `Ratel.Flag.Verify` le fait avant l'exécution du
+ bloc. `materialise` restitue le document entier sous la forme du modèle à partir duquel il a
+ été écrit, si bien que les trois encodages se convertissent les uns dans les autres sans
+ perte. Un encodage propre à l'application est lié à son codec avec `Ratel.Flag.Codecs`.

- ### Acceptances
+ ### Acceptations

- Two peers may hold different libraries of schemas. A reader then tells a writer what it can
- consume, with an *acceptance*. An acceptance is a small TEL document which lists, in order of
- preference, the compositions the reader accepts, each as a schema signature. Beside each it
- lists the further layers or atoms which the reader can resolve, and would like included if the
- writer holds them, named by a prefix of their hash.
+ Deux pairs peuvent détenir des bibliothèques de schémas différentes. Un lecteur indique alors
+ à un écrivain ce qu'il peut consommer, au moyen d'une *acceptation*. Une acceptation est un
+ petit document TEL qui liste, par ordre de préférence, les compositions que le lecteur
+ accepte, chacune sous forme de signature de schéma. À côté de chacune, elle liste les couches
+ ou atomes supplémentaires que le lecteur sait résoudre, et qu'il aimerait voir inclus si
+ l'écrivain les détient, nommés par un préfixe de leur empreinte.

- The message is itself TEL, so it travels in three forms:
+ Le message est lui-même du TEL, si bien qu'il circule sous trois formes :

```tabular
- column  Form
- column  Written by
- column  Read back by
- row  TEL text  `acceptance.encode`  `as[Tel.Acceptance]` on the text
- row  framed BinTEL  `acceptance.framed`  `Tel.Acceptance(framed)`
- row  one BASE-256 word  `acceptance.text`  `Tel.Acceptance(text)`
+ column  Forme
+ column  Écrite par
+ column  Relue par
+ row  texte TEL  `acceptance.encode`  `as[Tel.Acceptance]` sur le texte
+ row  BinTEL tramé  `acceptance.framed`  `Tel.Acceptance(framed)`
+ row  un mot BASE-256  `acceptance.text`  `Tel.Acceptance(text)`
```

- The BinTEL form may also be the bare root of the document, for an embedding which already
- knows what it is carrying. In full:
+ La forme BinTEL peut aussi être la racine nue du document, pour une intégration qui sait déjà
+ ce qu'elle transporte. En entier :

```scala fuel
parse
##
val acceptance = Tel.Acceptance
  ( Tel.Acceptance.Alternative
      ( Tel.Acceptance.Signature(contact.signature(List(t"with-address"))),
        components = List(Tel.Acceptance.Component(contact.prefix(t"with-phone"))) ),
    Tel.Acceptance.Alternative(Tel.Acceptance.Signature(contact.signature(List())), selfContained = true) )

val asText = acceptance.encode
val asBintel = acceptance.framed
val asWord = acceptance.text

val fromText = acceptance.encode.as[Tel.Acceptance]
val fromBintel = Tel.Acceptance(acceptance.framed)
val fromWord = Tel.Acceptance(acceptance.text)
```

- Here `contact` is a `SchemaSignature.Lineage`: a base schema, with its layers and atoms and
- their hashes. A signature for any selection of layers is computed from it, and a writer decodes
- against it the signatures which a reader sends.
+ Ici, `contact` est une `SchemaSignature.Lineage` : un schéma de base, avec ses couches, ses
+ atomes et leurs empreintes. Une signature pour toute sélection de couches se calcule à partir
+ d'elle, et un écrivain décode par rapport à elle les signatures qu'envoie un lecteur.

- A writer which holds a value under some composition serves the first alternative it can, with
- the richest composition that alternative permits. The reader takes the first alternative whose
- schema the arriving document's schema is a subtype of:
+ Un écrivain qui détient une valeur sous une certaine composition sert la première
+ alternative qu'il peut, avec la composition la plus riche que cette alternative permet. Le
+ lecteur prend la première alternative dont le schéma a pour sous-type le schéma du document
+ reçu :

```scala fuel
parse
##
val served = Tel.Acceptance.serve(acceptance, contact, composition, element)

served.let: served =>
  Tel.Acceptance.receive(acceptance, library, served.document)
```

- Compatibility is the subtype relation on composed schemas:
+ La compatibilité est la relation de sous-typage entre schémas composés :

```tabular
- column  Definition
- column  A subtype
- row  record  extends it
- row  select  narrows it
- row  scalar  tightens it
+ column  Définition
+ column  Un sous-type
+ row  enregistrement  l'étend
+ row  sélection  la restreint
+ row  scalaire  le resserre
```

- `Tels.Subtyping.subtype` decides it, and `check` reports the first premise which fails.
- `Tels.Projection` restricts a document to what a supertype can address.
+ `Tels.Subtyping.subtype` en décide, et `check` signale la première prémisse qui échoue.
+ `Tels.Projection` restreint un document à ce qu'un supertype peut adresser.

- #### Acceptances from Scala types
+ #### Acceptations à partir de types Scala

- Where the formats are Scala types, the acceptance and the reading of it are derived. Each
- type's required fields form the base which a reader requires. Its optional fields are offered
- as atoms, so two types which agree on their required fields interoperate, however their
- optional fields differ.
+ Lorsque les formats sont des types Scala, l'acceptation et sa lecture sont dérivées. Les
+ champs obligatoires de chaque type forment la base qu'exige un lecteur. Ses champs optionnels
+ sont proposés comme atomes, si bien que deux types qui s'accordent sur leurs champs
+ obligatoires interopèrent, quelles que soient les différences de leurs champs optionnels.

- A field can instead be offered as part of a named layer. A layer is declared once, as an object
- whose name is part of its type, and a field is assigned to it with `@assign`:
+ Un champ peut aussi être proposé comme partie d'une couche nommée. Une couche se déclare une
+ seule fois, sous forme d'un objet dont le nom fait partie de son type, et un champ lui est
+ affecté avec `@assign` :

```scala fuel
typecheck
##
object withPhone extends Tel.Layer("with-phone")

case class Contact(name: Text, email: Optional[Text], @assign(withPhone) phone: Optional[Text])
case class ContactV1(name: Text, email: Optional[Text])
```

- A tuple names the accepted types, in order of preference, and what arrives is decoded to their
- union:
+ Un tuple nomme les types acceptés, par ordre de préférence, et ce qui arrive est décodé en
+ leur union :

```scala fuel
typecheck
##
val accepting = Tel.Acceptance[(Contact, ContactV1)]()
send(accepting.acceptance.framed)

accepting.read(reply) match
  case contact: Contact   => contact.name
  case contact: ContactV1 => contact.name
```

- A layer can never add a case to a sum, but it can take cases away. A case of an `enum`, or of a
- sealed trait, which is annotated `@retract` remains a variant of the base, and the named layer
- excludes it. A reader which accepts that layer thereby says that it will never receive the
- case. A case may be retracted from several layers, though a layer cannot retract every case of
- a sum:
+ Une couche ne peut jamais ajouter de cas à une somme, mais elle peut en retirer. Un cas d'une
+ `enum`, ou d'un trait scellé, annoté `@retract` reste une variante de la base, et la couche
+ nommée l'exclut. Un lecteur qui accepte cette couche indique par là qu'il ne recevra jamais
+ ce cas. Un cas peut être retiré de plusieurs couches, mais une couche ne peut pas retirer
+ tous les cas d'une somme :

```scala fuel
typecheck
##
object lean extends Tel.Layer("lean")

enum Message:
  @retract(lean) case Ping
  @retract(lean) case Data(payload: Text)
  case Stop
```

- A writer answers from a value with `contact.fulfil(accepting.acceptance)`. It leaves out any
- layer which retracts a case the value uses, so that what it serves is always valid under the
- layers it names.
+ Un écrivain répond à partir d'une valeur avec `contact.fulfil(accepting.acceptance)`. Il
+ omet toute couche qui retire un cas qu'utilise la valeur, si bien que ce qu'il sert est
+ toujours valide sous les couches qu'il nomme.

- ### Related
+ ### Voir aussi

- - [Daemons](daemons.md) and [RPC](rpc.md) carry BinTEL messages, and [optics](optics.md) updates a TEL document.
+ - Les [démons](daemons.md) et le [RPC](rpc.md) transportent des messages BinTEL, et les
+   [optiques](optics.md) mettent à jour un document TEL.
