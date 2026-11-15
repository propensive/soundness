- ## Forms
+ ## Formulaires

- ### About
+ ### Présentation

- An HTML form is a user interface to a data type, and here one is generated from the type
- itself. A case class produces a form, with a labeled input for each field, and a widget
- chosen by the field's type. A submission is decoded back into the case class, and any
- failures of validation are attached to the fields which caused them. Nested case classes
- become nested fieldsets, so the form mirrors the structure of the data.
+ Un formulaire HTML est une interface utilisateur pour un type de données, et il est ici
+ généré à partir du type lui-même. Une case class produit un formulaire, avec un champ de
+ saisie étiqueté pour chaque champ, et un widget choisi selon le type du champ. Une soumission
+ est décodée en retour vers la case class, et tout échec de validation est rattaché aux champs
+ qui l'ont causé. Les case classes imbriquées deviennent des fieldsets imbriqués : le
+ formulaire reflète ainsi la structure des données.

- ### On forms
+ ### À propos des formulaires

- Forms are written twice in most web applications: once as HTML inputs, and again as the
- parsing and validation of what comes back. The two halves are kept consistent by hand. Every
- change to the data, such as a new field, a renamed one or a stricter type, must be made in
- both, or the form drifts from the data it claims to collect.
+ Dans la plupart des applications web, les formulaires sont écrits deux fois : une fois sous
+ forme de champs de saisie HTML, et une seconde fois sous forme d'analyse et de validation de
+ ce qui revient. Les deux moitiés sont maintenues cohérentes à la main. Chaque modification
+ des données, comme un nouveau champ, un champ renommé ou un type plus strict, doit être
+ faite des deux côtés, sans quoi le formulaire s'éloigne des données qu'il prétend recueillir.

- [Deriving][derivation] both halves from one type removes the duplication. The type says what
- fields exist, and what each accepts. The widgets follow from the types of the fields.
- Decoding a submission applies the same validated types which govern the rest of the program,
- so a malformed email address fails at the form, with a message which points at the email
- field. That is [correctness](../../philosophy/correctness.md) through a single source of truth.
+ [Dériver][dérivation] les deux moitiés d'un seul type supprime la duplication. Le type dit
+ quels champs existent, et ce que chacun accepte. Les widgets découlent des types des champs.
+ Le décodage d'une soumission applique les mêmes types validés qui régissent le reste du
+ programme : une adresse email mal formée échoue donc au niveau du formulaire, avec un
+ message qui désigne le champ de l'email. C'est la
+ [correction](../../philosophy/correctness.md) par une source unique de vérité.

- Everything comes from the `soundness` package. The last import lets the samples print their
- results:
+ Tout provient du package `soundness`. Le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import formulations.postFormulation
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Rendering a form
+ ### Rendu d'un formulaire

- `elicit` renders the form for a type as [HTML](html.md), ready to serve. `edit` renders it
- filled in from an existing value:
+ `elicit` rend le formulaire d'un type en [HTML](html.md), prêt à être servi. `edit` le rend
+ prérempli à partir d'une valeur existante :

```scala
case class Person(name: Text, email: EmailAddress)
case class Troupe(leader: Person, name: Text)

val form = elicit[Troupe](validation = Validation(), submit = t"Save")
```

```scala fuel
run
##
Out.println(form.show)
```

- Each field appears with a label derived from its name, so `leader` becomes "Leader". A nested
- case class becomes a `<fieldset>` with a legend, so the structure of the form explains
- itself.
+ Chaque champ apparaît avec une étiquette dérivée de son nom : `leader` devient ainsi
+ « Leader ». Une case class imbriquée devient un `<fieldset>` avec une légende, de sorte que
+ la structure du formulaire s'explique d'elle-même.

- The widget for each field is chosen by its type:
+ Le widget de chaque champ est choisi selon son type :

```tabular
- column  Field type
- column  Widget
- column  Rendered as
- row  `Boolean`  `Checkbox`  a checkbox
- row  an enumeration  `Combobox`  a text input with a `<datalist>` of the cases
- row  `Reference[entity]`  `Dropdown`  a `<select>` of the listed entities
- row  any other type encodable as `Text`  `Field`  a text input
- row  a case class  none  a `<fieldset>` of its fields
+ column  Type du champ
+ column  Widget
+ column  Rendu
+ row  `Boolean`  `Checkbox`  une case à cocher
+ row  une énumération  `Combobox`  un champ texte avec une `<datalist>` des cas
+ row  `Reference[entity]`  `Dropdown`  un `<select>` des entités listées
+ row  tout autre type encodable en `Text`  `Field`  un champ texte
+ row  une case class  aucun  un `<fieldset>` de ses champs
```

- ### Submissions
+ ### Soumissions

- A submission arrives as a `Query`: the pairs of keys and values of a form post, in which
- nested fields are dotted, as in `leader.name`. `as` decodes it to the type:
+ Une soumission arrive sous la forme d'une `Query` : les paires de clés et de valeurs d'un
+ envoi de formulaire, dans lesquelles les champs imbriqués sont pointés, comme dans
+ `leader.name`. `as` la décode vers le type :

```scala
val query = t"leader.name=Quince&leader.email=quince%40example.com&name=Mechanicals".as[Query]
```

```scala fuel
run
##
Out.println(query.as[Troupe].inspect)
```

- The fields are decoded through the same types which are used everywhere. An
- [`EmailAddress`](network-addresses.md#email-addresses) must parse, and a
- [`Name`](names.md) must satisfy its rules. The validation of a form is therefore not a
- separate set of rules. It is the type system doing its usual work at the boundary, and a
- failure carries a pointer to the field at fault:
+ Les champs sont décodés par les mêmes types que ceux utilisés partout ailleurs. Une
+ [`EmailAddress`](network-addresses.md#email-addresses) doit pouvoir être analysée, et un
+ [`Name`](names.md) doit respecter ses règles. La validation d'un formulaire n'est donc pas un
+ ensemble de règles à part. C'est le système de types qui fait son travail habituel à la
+ frontière, et un échec porte une référence vers le champ fautif :

```scala fuel
run
errors show
##
t"leader.name=Quince&leader.email=nobody&name=Mechanicals".as[Query].as[Troupe]
```

- [Decoding](decoding.md#requiring-a-value) explains how a field's type decides whether the
- field is required.
+ [Le décodage](decoding.md#requiring-a-value) explique comment le type d'un champ détermine
+ si ce champ est obligatoire.

- ### The form cycle
+ ### Le cycle du formulaire

- A form is a loop: render, submit, and render again with errors, until the value is complete.
- `elicit` takes the submitted `Query` and the state of validation, so the form which is
- rendered again shows what was entered, with the faults attached to their fields.
+ Un formulaire est une boucle : rendu, soumission, puis nouveau rendu avec les erreurs,
+ jusqu'à ce que la valeur soit complète. `elicit` prend la `Query` soumise et l'état de la
+ validation, de sorte que le formulaire rendu à nouveau montre ce qui a été saisi, avec les
+ fautes rattachées à leurs champs.

- A [server](http-server.md#responses) handler runs the loop, by decoding on success and
- rendering again on failure. It therefore deals in values, not in requests. The
- [HTTP client](http-client.md#posting-a-body) posts a `Query` as a form submission.
+ Un gestionnaire de [serveur](http-server.md#responses) exécute la boucle, en décodant en cas
+ de succès et en rendant à nouveau en cas d'échec. Il manipule donc des valeurs, et non des
+ requêtes. Le [client HTTP](http-client.md#posting-a-body) envoie une `Query` comme
+ soumission de formulaire.

- ### Customizing appearance
+ ### Personnaliser l'apparence

- How a form and its rows are rendered is a `Formulation`: the frame around the widgets, and
- the placement of labels and error messages. `postFormulation` gives a plain, unstyled
- rendering. An application supplies its own, to match its design, without touching how fields
- are derived. [CSS](css.md) styles the result.
+ La manière dont un formulaire et ses lignes sont rendus est une `Formulation` : le cadre
+ autour des widgets, et le placement des étiquettes et des messages d'erreur.
+ `postFormulation` fournit un rendu simple, sans style. Une application fournit le sien,
+ adapté à son design, sans toucher à la manière dont les champs sont dérivés. [CSS](css.md)
+ met en forme le résultat.
