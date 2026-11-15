- ## Records
+ ## Enregistrements

- ### About
+ ### Présentation

- The shape of some data is defined outside the program: by a database table, a JSON Schema or a
- configuration format. The program should still read its fields with static types.
+ La forme de certaines données est définie hors du programme : par une table de base de
+ données, un JSON Schema ou un format de configuration. Le programme devrait malgré tout lire
+ leurs champs avec des types statiques.

- A `Record` is a value whose fields come from such an external *specification*, which is read as
- the code compiles. So `record.name` typechecks as `Text`, and `record.nope` does not compile,
- and no class is written by hand to mirror the schema.
+ Un `Record` est une valeur dont les champs proviennent d'une telle *spécification* externe,
+ lue lors de la compilation du code. Ainsi, `record.name` est typé `Text`, `record.nope` ne
+ compile pas, et aucune classe n'est écrite à la main pour refléter le schéma.

- The [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) and [TEL](tel.md) *providers*
- are built on this. A JSON Schema, an XML Schema or a TEL schema document produces typed records.
- So does the response schema of an operation, through `record()` in the [OpenAPI](openapi.md)
- client. Any source of schemas which a program can read at [compiletime] can do the same.
+ Les *fournisseurs* [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) et
+ [TEL](tel.md) reposent sur ce mécanisme. Un JSON Schema, un XML Schema ou un document de
+ schéma TEL produit des enregistrements typés. C'est aussi le cas du schéma de réponse d'une
+ opération, par `record()` dans le client [OpenAPI](openapi.md). Toute source de schémas
+ qu'un programme peut lire [à la compilation][compilation] peut faire de même.

- ### On external schemas
+ ### À propos des schémas externes

- When the authoritative shape of the data lives elsewhere, a case class written by hand is a
- copy, and copies drift. The schema gains a field, the class does not, and the mismatch appears
- at [runtime]. The usual alternative is to read fields dynamically, by string. That gives up
- checking altogether.
+ Lorsque la forme qui fait autorité pour des données se trouve ailleurs, une case class écrite
+ à la main en est une copie, et les copies divergent. Le schéma gagne un champ, la classe non,
+ et la discordance apparaît [à l'exécution][exécution]. L'alternative habituelle consiste à lire
+ les champs dynamiquement, par chaîne de caractères. On renonce alors à toute vérification.

- A `Record` avoids both. The schema is read once, as the program compiles. Scala's
- [structural types](https://docs.scala-lang.org/scala3/book/types-structural.html) then build
- the exact interface of the record from it, with every field at the type which the schema
- declares. The schema cannot drift from the code, because the types in the code *are* the
- schema's. This is [safety by construction](../../philosophy/safety-by-construction.md), with no
- class to maintain.
+ Un `Record` évite les deux écueils. Le schéma est lu une fois, lors de la compilation du
+ programme. Les [types structurels](https://docs.scala-lang.org/scala3/book/types-structural.html)
+ de Scala en construisent ensuite l'interface exacte de l'enregistrement, chaque champ ayant
+ le type que déclare le schéma. Le schéma ne peut pas diverger du code, parce que les types du
+ code *sont* ceux du schéma. C'est la
+ [sûreté par construction](../../philosophy/safety-by-construction.md), sans classe à
+ maintenir.

- Everything comes from the `soundness` package. The other imports choose an encoding, and a
- [strategy](errors.md#choosing-a-strategy) for errors:
+ Tout provient du package `soundness`. Les autres imports choisissent un encodage, et une
+ [stratégie](errors.md#choosing-a-strategy) pour les erreurs :

```scala
import soundness.*
import codepages.utf8Codepage
import strategies.throwUnsafely
```

- ### Using records
+ ### Utiliser les enregistrements

- A provider object holds a schema. This one is a `Json.Provider`, built from a JSON Schema
- document. In a real program it is declared in a file of its own, for the reason which
- [compilation order](#compilation-order) explains. For the same reason, the samples on this page
- are checked but not run:
+ Un objet fournisseur contient un schéma. Celui-ci est un `Json.Provider`, construit à partir
+ d'un document JSON Schema. Dans un vrai programme, il est déclaré dans un fichier à part, pour
+ la raison qu'explique l'[ordre de compilation](#compilation-order). Pour la même raison, les
+ exemples de cette page sont vérifiés mais pas exécutés :

```scala fuel
typecheck
##
object Catalogue extends Json.Provider(t"""{
  "type": "object",
  "required": ["name", "children"],
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer" },
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
```

- The `record` method of the provider turns raw data into a typed record. Each field has the type
- which the schema gives it. `name` is a string in the schema, so it is a `Text`. `weight` is a
- number, so it is a `Double`. The schema does not require `age`, so it is an
- [`Optional[Int]`](optional-values.md):
+ La méthode `record` du fournisseur transforme des données brutes en un enregistrement typé.
+ Chaque champ a le type que lui donne le schéma. `name` est une chaîne dans le schéma ; c'est
+ donc un `Text`. `weight` est un nombre ; c'est donc un `Double`. Le schéma n'exige pas `age` ;
+ c'est donc un [`Optional[Int]`](optional-values.md) :

```scala fuel
typecheck
##
val record = Catalogue.record(input)

val name: Text = record.name
val weight: Optional[Double] = record.children.head.let(_.weight)
val age: Optional[Int] = record.age
```

- The schema has no field called `nope`, so `record.nope` does not compile.
+ Le schéma n'a pas de champ nommé `nope` ; `record.nope` ne compile donc pas.

- A nested object becomes a nested record, and an array becomes a list. Every access is checked
- against the specification. A class written by hand would give the same guarantee, but there is
- no class.
+ Un objet imbriqué devient un enregistrement imbriqué, et un tableau devient une liste. Chaque
+ accès est vérifié par rapport à la spécification. Une classe écrite à la main donnerait la
+ même garantie, mais il n'y a pas de classe.

- ### Tuples instead of records
+ ### Des tuples à la place des enregistrements

- The same provider can produce a
- [named tuple](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html)
- instead of a record, through `tuple`, the twin of `record`.
+ Le même fournisseur peut produire un
+ [tuple nommé](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html)
+ plutôt qu'un enregistrement, au moyen de `tuple`, le jumeau de `record`.

- For the schema above, `Catalogue.tuple(input)` has the type
- `(name: Text, age: Optional[Int], children: List[(weight: Double)])`. It has one element for
- each field, named as the schema names it, in the order of the schema. Nested objects become
- nested named tuples, and arrays become lists of them.
+ Pour le schéma ci-dessus, `Catalogue.tuple(input)` a le type
+ `(name: Text, age: Optional[Int], children: List[(weight: Double)])`. Il a un élément par
+ champ, nommé comme le nomme le schéma, dans l'ordre du schéma. Les objets imbriqués
+ deviennent des tuples nommés imbriqués, et les tableaux des listes de tels tuples.

- A named tuple is read by name, like a record. It is also an ordinary tuple, so it is taken apart
- by position, and converts to its unnamed form:
+ Un tuple nommé se lit par nom, comme un enregistrement. C'est aussi un tuple ordinaire ; il se
+ décompose donc par position, et se convertit en sa forme non nommée :

```scala fuel
typecheck
##
val tuple = Catalogue.tuple(input)

val name: Text = tuple.name
val (first, second, third) = tuple
val plain: (Text, Optional[Int], List[(weight: Double)]) = tuple.toTuple
```

- #### Eager and lazy
+ #### Évaluation stricte et paresseuse

- The two forms differ in *when* the data is read.
+ Les deux formes diffèrent par le *moment* où les données sont lues.

- A record is lazy. It holds the raw data, and each access reads and converts its field afresh. A
- malformed field is noticed only when that field is read, and a field read twice is converted
- twice.
+ Un enregistrement est paresseux. Il contient les données brutes, et chaque accès lit et
+ convertit son champ à nouveau. Un champ mal formé n'est remarqué que lorsque ce champ est lu,
+ et un champ lu deux fois est converti deux fois.

- A tuple is eager. Every field is read once, when the tuple is built, and the tuple holds the
- converted values. A malformed field fails the construction of the tuple, whether or not the
- program ever reads it.
+ Un tuple est strict. Chaque champ est lu une fois, lors de la construction du tuple, et le
+ tuple contient les valeurs converties. Un champ mal formé fait échouer la construction du
+ tuple, que le programme le lise ou non.

- This changes the types of the fields. Reading any field can fail: a required value may be
- absent, a value may be of the wrong kind, or it may break a constraint of the schema. It fails
- with the specification's own error, such as `Json.Provider.Error`, through the [tactic] of the
- code which reads it.
+ Cela change les types des champs. La lecture de n'importe quel champ peut échouer : une
+ valeur requise peut être absente, une valeur peut être de la mauvaise nature, ou elle peut
+ enfreindre une contrainte du schéma. L'échec se produit avec l'erreur propre à la
+ spécification, comme `Json.Provider.Error`, au moyen de la [tactique] du code qui la lit.

- In a record, a field has the type `Record.Field[Int, Json.Provider.Error]`. That is a context
- function over the tactic, applied wherever the field is selected, so each access needs a handler
- in scope. In a tuple, a failure can happen only during construction. The type of the element is
- plainly `Int`, and the handler must be in scope where the tuple is built.
+ Dans un enregistrement, un champ a le type `Record.Field[Int, Json.Provider.Error]`. C'est une
+ fonction contextuelle sur la tactique, appliquée partout où le champ est sélectionné ; chaque
+ accès a donc besoin d'un gestionnaire dans la portée. Dans un tuple, un échec ne peut se
+ produire que pendant la construction. Le type de l'élément est simplement `Int`, et le
+ gestionnaire doit être dans la portée là où le tuple est construit.

- Since every failure goes through the caller's tactic, a reader chooses how to meet it. One field
- can be wrapped in `attempt` or `recover`. A whole record can be read with `validate` and
- `track`, with a `venture` for each field, which collects every problem in a document and reports
- them together. [Errors](errors.md) describes these.
+ Comme chaque échec passe par la tactique de l'appelant, un lecteur choisit comment y faire
+ face. Un champ peut être enveloppé dans `attempt` ou `recover`. Un enregistrement entier peut
+ se lire avec `validate` et `track`, avec un `venture` pour chaque champ, ce qui recueille tous
+ les problèmes d'un document et les signale ensemble. [Erreurs](errors.md) les décrit.

- In summary:
+ En résumé :

```tabular
- column  Aspect
- column  Record
- column  Tuple
- row  Data is read  at each field access  once, when the tuple is built
- row  A malformed field fails  when it is accessed  the construction
- row  A field of schema type integer has type  `Record.Field[Int, Json.Provider.Error]`  `Int`
- row  The `Tactic` must be in scope  wherever a field is selected  where the tuple is built
- row  Destructures positionally  no  yes
+ column  Aspect
+ column  Enregistrement
+ column  Tuple
+ row  Les données sont lues  à chaque accès à un champ  une fois, lors de la construction du tuple
+ row  Un champ mal formé fait échouer  l'accès à ce champ  la construction
+ row  Un champ entier du schéma a le type  `Record.Field[Int, Json.Provider.Error]`  `Int`
+ row  La `Tactic` doit être dans la portée  à chaque sélection d'un champ  à la construction du tuple
+ row  Décomposition par position  non  oui
```

- ### Defining a specification
+ ### Définir une spécification

- A new source of schemas plugs in by implementing `Specification`. Such an implementation is a
- *provider*. The source might be the table definitions of a database, an XML Schema, or a
- proprietary format.
+ Une nouvelle source de schémas se branche en implémentant `Specification`. Une telle
+ implémentation est un *fournisseur*. La source peut être les définitions de tables d'une base
+ de données, un XML Schema, ou un format propriétaire.

- A provider supplies the names and types of the fields as an ordered list of `Member` values. The
- elements of a tuple follow that order. It also supplies primitive operations over the underlying
- data:
+ Un fournisseur donne les noms et les types des champs sous forme de liste ordonnée de valeurs
+ `Member`. Les éléments d'un tuple suivent cet ordre. Il fournit aussi des opérations
+ primitives sur les données sous-jacentes :

```tabular
- column  Hook
- column  Default
- column  Supplies
- row  `access`  none: must be implemented  a named field's value, fetched from a value
- row  `absent`  none: must be implemented  whether a fetched value stands for a missing field
- row  `repeated`  none: must be implemented  every value of a field which occurs many times
- row  `required`  returns the value as it is  the value of a field read under `One`; overridden to fail when it is absent
- row  `entries`  fails  a keyed field's values by name
- row  `kind`  fails  the kind of a value, by which a union chooses its alternative
- row  `elements`  fails  the elements of a value which is itself a sequence
- row  `pairs`  fails  the named values of a value which is itself a dictionary
- row  `unoffered`  fails  the failure for a value of a kind no alternative of a union offers
+ column  Point d'accroche
+ column  Défaut
+ column  Fournit
+ row  `access`  aucun : doit être implémenté  la valeur d'un champ nommé, extraite d'une valeur
+ row  `absent`  aucun : doit être implémenté  si une valeur extraite représente un champ manquant
+ row  `repeated`  aucun : doit être implémenté  chaque valeur d'un champ présent plusieurs fois
+ row  `required`  renvoie la valeur telle quelle  la valeur d'un champ lu sous `One` ; redéfini pour échouer en cas d'absence
+ row  `entries`  échoue  les valeurs d'un champ indexé, par nom
+ row  `kind`  échoue  la nature d'une valeur, par laquelle une union choisit son alternative
+ row  `elements`  échoue  les éléments d'une valeur qui est elle-même une séquence
+ row  `pairs`  échoue  les valeurs nommées d'une valeur qui est elle-même un dictionnaire
+ row  `unoffered`  échoue  l'échec pour une nature qu'aucune alternative d'une union ne propose
```

- The values of a repeated field are the elements of a JSON array, say. In a format which repeats
- a field, they are every sibling with the field's keyword. A format which has unions or
- dictionaries provides `entries`, `kind`, `elements` and `pairs`. One which does not leaves their
- defaults.
+ Les valeurs d'un champ répété sont, par exemple, les éléments d'un tableau JSON. Dans un
+ format qui répète un champ, ce sont tous les éléments frères portant le mot-clé du champ. Un
+ format qui a des unions ou des dictionnaires fournit `entries`, `kind`, `elements` et `pairs`.
+ Un format qui n'en a pas laisse leurs valeurs par défaut.

- A `Member` is one of three things. A `Value` names the scalar type which the schema declared. A
- `Record` holds further members. A `Union` holds alternatives, each keyed by the *kind* of value
- which selects it. Every member carries a `Multiplicity`:
+ Un `Member` est l'une de trois choses. Un `Value` nomme le type scalaire que le schéma a
+ déclaré. Un `Record` contient d'autres membres. Une `Union` contient des alternatives,
+ chacune indexée par la *nature* de valeur qui la sélectionne. Chaque membre porte une
+ `Multiplicity` :

```tabular
- column  Multiplicity
- column  Reads as
- column  Through
- row  `One`  the value  `access`, then `required`
- row  `Optional`  an `Optional`, `Unset` where `absent`  `access` and `absent`
- row  `Many`  a `List`  `repeated`
- row  `Keyed`  a `Map[Text, _]`  `entries`
- row  `Series`  the provider's own collection  `series`
+ column  Multiplicité
+ column  Se lit comme
+ column  Au moyen de
+ row  `One`  la valeur  `access`, puis `required`
+ row  `Optional`  un `Optional`, `Unset` là où `absent` est vrai  `access` et `absent`
+ row  `Many`  une `List`  `repeated`
+ row  `Keyed`  une `Map[Text, _]`  `entries`
+ row  `Series`  la collection propre au fournisseur  `series`
```

- The provider says nothing more about optionality or repetition. The macro applies the
- multiplicity itself. An alternative of a union may carry its own, so a union with a single
- alternative nests one multiplicity inside another.
+ Le fournisseur n'en dit pas plus sur le caractère optionnel ou la répétition. La macro
+ applique elle-même la multiplicité. Une alternative d'une union peut porter la sienne ; une
+ union à une seule alternative imbrique ainsi une multiplicité dans une autre.

- A provider names the error which its fields fail with as its `Failure` type. The primitives
- which can fail are `access`, `repeated`, `required`, `entries`, `elements`, `pairs` and
- `unoffered`, and each takes the tactic for that error.
+ Un fournisseur nomme l'erreur avec laquelle ses champs échouent comme son type `Failure`. Les
+ primitives qui peuvent échouer sont `access`, `repeated`, `required`, `entries`, `elements`,
+ `pairs` et `unoffered`, et chacune prend la tactique pour cette erreur.

- One [typeclass] completes the picture. There is an `Intensional` instance for each scalar type
- name which the schema can declare. It says which Scala type the name becomes, and how to read a
- single value of it. There are three ways to make one:
+ Une [classe de types] complète le tableau. Il existe une instance `Intensional` pour chaque
+ nom de type scalaire que le schéma peut déclarer. Elle indique en quel type Scala le nom se
+ traduit, et comment en lire une valeur. Il y a trois manières d'en créer une :

```tabular
- column  Constructor
- column  From
- row  `Intensional(accessor)`  a function of the value
- row  `Intensional.parametric`  a function of the value and the member's parameters, for a type such as a bounded integer
- row  `Intensional.Reading`  a function of the value, its parameters and a `Tactic`, for a reading which can fail
+ column  Constructeur
+ column  À partir de
+ row  `Intensional(accessor)`  une fonction de la valeur
+ row  `Intensional.parametric`  une fonction de la valeur et des paramètres du membre, pour un type comme un entier borné
+ row  `Intensional.Reading`  une fonction de la valeur, de ses paramètres et d'une `Tactic`, pour une lecture qui peut échouer
```

- A reading may fail: the value may be of the wrong kind, or outside its bounds, or a string which
- does not match its pattern. Such a reading is an `Intensional.Fallible`, whose `transform` takes
- the tactic for its `Error`. That `Error` must be the provider's `Failure`, or a subtype of it,
- so that every field fails in one way. An instance which raises anything else is a compile error.
+ Une lecture peut échouer : la valeur peut être de la mauvaise nature, hors de ses bornes, ou
+ une chaîne qui ne correspond pas à son motif. Une telle lecture est un
+ `Intensional.Fallible`, dont `transform` prend la tactique pour son `Error`. Cette `Error`
+ doit être le `Failure` du fournisseur, ou l'un de ses sous-types, afin que tous les champs
+ échouent d'une même manière. Une instance qui lève autre chose est une erreur de compilation.

- The instances live in the companion of the provider, so they are found without an import. A
- provider over XML, for instance, would fetch a child element by its label, find a field absent
- where no such child exists, and repeat over every child with the label.
+ Les instances se trouvent dans le compagnon du fournisseur ; elles sont donc trouvées sans
+ import. Un fournisseur sur XML, par exemple, extrairait un élément enfant par son étiquette,
+ considérerait un champ comme absent là où un tel enfant n'existe pas, et répéterait sur chaque
+ enfant portant l'étiquette.

- Some sources are not in the values which are read from them. A record over a file which is
- mapped into memory holds only where to look, and the file itself is open for a while, and
- closed. A provider for such a source names a `Context`, and makes its records with
- `Specification.recordWith`, giving the type of the context each is to be read in. A member of
- such a record is read only where a value of that type is [given], beside its tactic, so a
- record cannot be read once its source has gone, and the type may say which of several sources
- a record came from. The provider's `accessWith`, `repeatedWith` and `entriesWith` are given the
- context, and stand for `access`, `repeated` and `entries`.
+ Certaines sources ne se trouvent pas dans les valeurs qu'on en lit. Un enregistrement sur un
+ fichier projeté en mémoire ne contient que l'endroit où regarder, et le fichier lui-même est
+ ouvert un certain temps, puis fermé. Un fournisseur pour une telle source nomme un `Context`,
+ et crée ses enregistrements avec `Specification.recordWith`, en donnant le type du contexte
+ dans lequel chacun doit être lu. Un membre d'un tel enregistrement ne se lit que là où une
+ valeur de ce type est [given], à côté de sa tactique ; un enregistrement ne peut donc pas
+ être lu une fois sa source disparue, et le type peut indiquer de laquelle de plusieurs
+ sources provient un enregistrement. Les méthodes `accessWith`, `repeatedWith` et
+ `entriesWith` du fournisseur reçoivent le contexte, et tiennent lieu d'`access`, de
+ `repeated` et d'`entries`.

- A `Series` is for such a provider too. It reads as the provider's `Series` type, a collection
- of its own, which `series` makes from a function which reads one element. The collection reads
- an element when it is asked for one, in the context it is then given, so a field of a hundred
- thousand records costs nothing until one is looked up.
+ Une `Series` est elle aussi destinée à un tel fournisseur. Elle se lit comme le type `Series`
+ du fournisseur, une collection qui lui est propre, que `series` crée à partir d'une fonction
+ qui lit un élément. La collection lit un élément lorsqu'on le lui demande, dans le contexte
+ qui lui est alors donné ; un champ de cent mille enregistrements ne coûte donc rien tant
+ qu'on n'en consulte aucun.

- Every specification object gets `record` and `tuple` from `Specification` itself. Nothing need
- be declared in the object beyond its schema. Both methods are `transparent inline`. They are
- expanded where they are called, with the specification object known, so the macro can find that
- object by its type and evaluate its `fields`.
+ Tout objet de spécification reçoit `record` et `tuple` de `Specification` elle-même. Rien
+ n'a besoin d'être déclaré dans l'objet au-delà de son schéma. Les deux méthodes sont
+ `transparent inline`. Elles sont expansées là où elles sont appelées, l'objet de
+ spécification étant connu ; la macro peut donc trouver cet objet par son type et évaluer ses
+ `fields`.

- The declared return type of `record` is `Record`. What a call actually has is a *structural
- refinement* of it. For a schema of three fields, it is a type such as this:
+ Le type de retour déclaré de `record` est `Record`. Ce qu'a réellement un appel est un
+ *raffinement structurel* de ce type. Pour un schéma de trois champs, c'est un type comme
+ celui-ci :

```scala fuel
typecheck
##
type Person = Record { def age: Double; def name: Text; def employed: Boolean }
```

- Only a transparent method lets that more precise type reach the place where it is called.
+ Seule une méthode transparente permet à ce type plus précis d'atteindre l'endroit où elle est
+ appelée.

- ### Compilation order
+ ### Ordre de compilation

- The schema object must be in a *different file* from the code which calls its `record` method.
+ L'objet schéma doit se trouver dans un *fichier différent* du code qui appelle sa méthode
+ `record`.

- The macro runs while the calling code compiles. It evaluates the schema object as a runtime
- value, by reading the JSON Schema document or querying the database. So that object must already
- be compiled. Scala guarantees this order for a macro and its use, provided they are in separate
- files with no cycle between them. If they are in one file, the schema is not yet available when
- the macro needs it.
+ La macro s'exécute pendant la compilation du code appelant. Elle évalue l'objet schéma comme
+ une valeur d'exécution, en lisant le document JSON Schema ou en interrogeant la base de
+ données. Cet objet doit donc déjà être compilé. Scala garantit cet ordre pour une macro et son
+ utilisation, à condition qu'elles se trouvent dans des fichiers séparés sans cycle entre eux.
+ Si elles se trouvent dans un même fichier, le schéma n'est pas encore disponible lorsque la
+ macro en a besoin.

- This is also what makes the pattern a type provider, in the sense of F#. The compiler consults
- the authoritative schema, and the types which the program is checked against are built from it.
+ C'est aussi ce qui fait de ce motif un fournisseur de types, au sens de F#. Le compilateur
+ consulte le schéma qui fait autorité, et les types par rapport auxquels le programme est
+ vérifié sont construits à partir de lui.

- ### Related
+ ### Voir aussi

- - [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) and [TEL](tel.md) each describe
-   the provider for their own schema language.
- - [OpenAPI](openapi.md) builds records from the response schemas of an API.
- - [Errors](errors.md) describes `attempt`, `recover`, `validate` and `track`, which meet the
-   failure of a field.
- - [Optional values](optional-values.md) describes the type of a field which the schema does not
-   require.
- - [Metaprogramming](metaprogramming.md) describes the macros which a provider relies on.
- - [Catalogs](catalogs.md), [databases](database.md) and [tables](tables.md) work with the fields of a record by name.
+ - [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) et [TEL](tel.md) décrivent chacun le
+   fournisseur de leur propre langage de schéma.
+ - [OpenAPI](openapi.md) construit des enregistrements à partir des schémas de réponse d'une API.
+ - [Erreurs](errors.md) décrit `attempt`, `recover`, `validate` et `track`, qui font face à l'échec
+   d'un champ.
+ - [Valeurs optionnelles](optional-values.md) décrit le type d'un champ que le schéma n'exige pas.
+ - [Métaprogrammation](metaprogramming.md) décrit les macros sur lesquelles s'appuie un
+   fournisseur.
+ - [Catalogues](catalogs.md), [bases de données](database.md) et [tableaux](tables.md) manipulent
+   les champs d'un enregistrement par leur nom.
