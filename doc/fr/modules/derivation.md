- ## Derivation
+ ## Dérivation

- ### About
+ ### Présentation

- The derived codecs which appear throughout these libraries, such as [JSON](json.md) encoders
- from case classes, [CSV](csv.md) rows from fields and digests from structures, are all produced
- by one derivation engine. It generates a [typeclass] instance for any *product*, which is a case
- class, whose instance combines those of its fields, or any *coproduct*, which is an
- [enumeration] or sealed trait, whose instance dispatches on the variant. A typeclass author
- writes the two rules of combination once, and every user-defined type gains an instance for
- free.
+ Les codecs dérivés que l'on rencontre dans toutes ces bibliothèques, comme les encodeurs
+ [JSON](json.md) à partir de case classes, les lignes [CSV](csv.md) à partir de champs et les
+ condensés à partir de structures, sont tous produits par un même moteur de dérivation. Il
+ génère une instance de [classe de types] pour tout *produit*, c'est-à-dire une case class, dont
+ l'instance combine celles de ses champs, ou pour tout *coproduit*, c'est-à-dire une
+ [énumération] ou un sealed trait, dont l'instance effectue une répartition selon la variante.
+ L'auteur d'une classe de types écrit une seule fois les deux règles de combinaison, et chaque
+ type défini par l'utilisateur obtient gratuitement une instance.

- To the user of a typeclass, [derivation] is invisible. Defining a case class is enough. To the
- author of a typeclass, it is two methods: one which says how fields combine, and one which says
- how variants are told apart.
+ Pour l'utilisateur d'une classe de types, la [dérivation] est invisible. Définir une case class
+ suffit. Pour l'auteur d'une classe de types, elle se résume à deux méthodes : l'une qui dit
+ comment les champs se combinent, l'autre qui dit comment distinguer les variantes.

- ### On derivation
+ ### À propos de la dérivation

- The instances of most typeclasses over most data types are mechanical. A `Show` for a case class
- shows each field. An `Eq` compares each field. A JSON encoder encodes each field under its name.
- Writing them by hand is boilerplate which drifts. Writing a macro for each typeclass needs
- expertise which most libraries do not have. What is wanted is a statement of the two general
- rules, that products combine and coproducts choose, from which every concrete instance follows.
+ Les instances de la plupart des classes de types pour la plupart des types de données sont
+ mécaniques. Un `Show` pour une case class affiche chaque champ. Un `Eq` compare chaque champ.
+ Un encodeur JSON encode chaque champ sous son nom. Les écrire à la main, c'est produire du code
+ répétitif qui dérive avec le temps. Écrire une macro pour chaque classe de types exige une
+ expertise que la plupart des bibliothèques n'ont pas. Ce que l'on veut, c'est énoncer les deux
+ règles générales, à savoir que les produits combinent et que les coproduits choisissent, d'où
+ découle chaque instance concrète.

- The engine here, the successor to Magnolia, provides exactly that, as ordinary inheritance and
- not as macro-writing. Derivation is what makes [small APIs](../../philosophy/small-apis.md)
- possible: a typeclass needs two rules, not an instance for each type.
+ Le moteur présenté ici, successeur de Magnolia, fournit exactement cela, sous forme d'héritage
+ ordinaire et non d'écriture de macros. La dérivation est ce qui rend possibles les
+ [petites API](../../philosophy/small-apis.md) : une classe de types a besoin de deux règles, et
+ non d'une instance pour chaque type.

- Everything comes from the `soundness` package. The second import brings in the `label` of a
- field, whose name the umbrella shares with a browser element's. The third permits indexing the
- arrays of fields which a derivation produces, and the fourth chooses a [strategy] for errors:
+ Tout provient du package `soundness`. Le deuxième import apporte le `label` d'un champ, dont le
+ nom est partagé, dans le package parapluie, avec celui d'un élément de navigateur. Le troisième
+ permet d'indexer les tableaux de champs que produit une dérivation, et le quatrième choisit une
+ [stratégie] pour les erreurs :

```scala
import soundness.*
import wisteria.label
import dysasymptotics.linearAccess
import strategies.throwUnsafely
```

- The samples on this page are checked by a compiled test in the library, `wisteria_doccheck.scala`.
- They are not run here.
+ Les exemples de cette page sont vérifiés par un test compilé de la bibliothèque,
+ `wisteria_doccheck.scala`. Ils ne sont pas exécutés ici.

- ### Products, sums and typeclasses
+ ### Produits, sommes et classes de types

- Two words carry most of the weight, *product* and *sum*:
+ Deux mots portent l'essentiel du sens, *produit* et *somme* :

```tabular
- column  Kind
- column  A type which is
- column  For example
- column  Made of
- row  product  a fixed sequence of values of other types  a case class, an enumeration case, a tuple, a singleton  *fields*
- row  sum  a single choice from a fixed set of disjoint types  an enumeration, a sealed trait  *variants*
+ column  Sorte
+ column  Un type qui est
+ column  Par exemple
+ column  Composé de
+ row  produit  une séquence fixe de valeurs d'autres types  une case class, un cas d'énumération, un tuple, un singleton  *champs*
+ row  somme  un choix unique parmi un ensemble fixe de types disjoints  une énumération, un sealed trait  *variantes*
```

- The fields of a product have fixed types, a canonical order and labels. Products and sums are
- category-theoretic duals, and so, correspondingly, are fields and variants.
+ Les champs d'un produit ont des types fixes, un ordre canonique et des étiquettes. Produits et
+ sommes sont duaux au sens de la théorie des catégories, et il en va de même, en conséquence,
+ des champs et des variantes.

```scala
sealed trait Temporal

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

case class Date(day: Int, month: Month, year: Int) extends Temporal
case class Time(hour: Int, minute: Int)
case class DateTime(date: Date, time: Time) extends Temporal
```

- Here `Temporal` is a sum whose variants are `Date` and `DateTime`. `Date`, `Time` and `DateTime`
- are products, and `day`, `month` and `year` are the fields of `Date`. `Month` is a sum whose
- twelve variants are themselves products: singletons, which are products with no fields.
+ Ici, `Temporal` est une somme dont les variantes sont `Date` et `DateTime`. `Date`, `Time` et
+ `DateTime` sont des produits, et `day`, `month` et `year` sont les champs de `Date`. `Month`
+ est une somme dont les douze variantes sont elles-mêmes des produits : des singletons, qui sont
+ des produits sans champ.

- Typeclasses divide along a related line. Where the type parameter appears in the *return* type
- of the abstract method, the typeclass is a *producer*. It makes new instances of that type, and
- may be covariant. Where the parameter appears in the method's *parameters*, the typeclass is a
- *consumer*. Existing instances are handed to it, and it may be contravariant. Nothing is used
- up. The word only says which way the value travels:
+ Les classes de types se divisent selon une ligne voisine. Lorsque le paramètre de type apparaît
+ dans le type de *retour* de la méthode abstraite, la classe de types est un *producteur*. Elle
+ crée de nouvelles instances de ce type, et peut être covariante. Lorsque le paramètre apparaît
+ dans les *paramètres* de la méthode, la classe de types est un *consommateur*. Des instances
+ existantes lui sont remises, et elle peut être contravariante. Rien n'est épuisé. Le mot dit
+ seulement dans quel sens circule la valeur :

```scala
trait Presenting[value]:
  def present(value: value): Text

trait Producing[+value]:
  def produce(): value
```

- The distinction matters because the two derive differently. A consumer folds over the fields of
- a value it was given. A producer has no value to fold over, and must build one.
+ La distinction compte, car les deux se dérivent différemment. Un consommateur effectue un pli
+ sur les champs d'une valeur qu'on lui a donnée. Un producteur n'a aucune valeur sur laquelle
+ plier, et doit en construire une.

- ### Deriving instances of a typeclass
+ ### Dériver les instances d'une classe de types

- A typeclass gains derivation by extending `Derivation`, or `ProductDerivation` where only case
- classes make sense, and implementing `conjunction` for products and `disjunction` for
- coproducts. Here is a Show-like typeclass in full, with the extension method which makes it
- convenient to call:
+ Une classe de types acquiert la dérivation en étendant `Derivation`, ou `ProductDerivation` là
+ où seules les case classes ont un sens, et en implémentant `conjunction` pour les produits et
+ `disjunction` pour les coproduits. Voici une classe de types semblable à Show, en entier, avec
+ la méthode d'extension qui la rend pratique à appeler :

```scala
trait Presentation[value]:
  def present(value: value): Text

object Presentation extends Derivation[Presentation]:
  given Presentation[Text] = identity(_)
  given Presentation[Int] = _.toString.tt

  inline def conjunction[derivation <: Product: ProductReflection]: Presentation[derivation] =
    value =>
      fields(value):
        [field] => field => t"$label=${contextual.present(field)}"
      . join(t"${typeName[derivation]}(", t", ", t")")

  inline def disjunction[derivation: SumReflection]: Presentation[derivation] =
    value =>
      variant(value):
        [variant <: derivation] => variant => contextual.present(variant)

extension [value](value: value)
  def present(using presentation: Presentation[value]): Text = presentation.present(value)
```

- `fields` folds over the fields of a product, and gives each one its `label`, its position, and
- the `contextual` instance of the typeclass for its type. `variant` dispatches on the actual case
- of a coproduct. That is the whole cost of derivation for a typeclass.
+ `fields` effectue un pli sur les champs d'un produit, et donne à chacun son `label`, sa
+ position, et l'instance `contextual` de la classe de types pour son type. `variant` effectue
+ une répartition selon le cas effectif d'un coproduit. C'est là tout le coût de la dérivation
+ pour une classe de types.

- `fields` and `variant` are two of a small set of primitives, which the sections below introduce
- in turn:
+ `fields` et `variant` font partie d'un petit ensemble de primitives, que les sections
+ ci-dessous présentent tour à tour :

```tabular
- column  Primitive
- column  Used in
- column  Does
- row  `fields(value)`  `conjunction`  folds over a product's fields, with their values
- row  `contexts[derivation]()`  `conjunction`  folds over a product's fields, with no value in hand
- row  `build[derivation]`  `conjunction`  constructs a product from a value for each field
- row  `construct`  `conjunction`  constructs a product inside a type constructor, given its `pure` and `bind`
- row  `dereference`  a `contexts` lambda  extracts the current field from a product
- row  `variant(value)`  `disjunction`  dispatches on the variant of a value
- row  `delegate[derivation](label)`  `disjunction`  dispatches to the variant a label names
- row  `complement(other)`  a `variant` lambda  narrows another value to the same variant, as an `Optional`
- row  `choice[derivation]`  `disjunction`  says, as the code compiles, whether every variant is a singleton
- row  `choices[derivation]`  `disjunction`  folds over every variant, with no value in hand
- row  `singleton[derivation](label)`  `disjunction`  yields the singleton a label names
- row  `variantLabels[derivation]`  `disjunction`  lists the labels of the variants
+ column  Primitive
+ column  Utilisée dans
+ column  Effet
+ row  `fields(value)`  `conjunction`  plie sur les champs d'un produit, avec leurs valeurs
+ row  `contexts[derivation]()`  `conjunction`  plie sur les champs d'un produit, sans valeur en main
+ row  `build[derivation]`  `conjunction`  construit un produit à partir d'une valeur pour chaque champ
+ row  `construct`  `conjunction`  construit un produit dans un constructeur de type, étant donnés son `pure` et son `bind`
+ row  `dereference`  une lambda de `contexts`  extrait le champ courant d'un produit
+ row  `variant(value)`  `disjunction`  répartit selon la variante d'une valeur
+ row  `delegate[derivation](label)`  `disjunction`  répartit vers la variante que nomme une étiquette
+ row  `complement(other)`  une lambda de `variant`  restreint une autre valeur à la même variante, sous forme d'`Optional`
+ row  `choice[derivation]`  `disjunction`  indique, pendant la compilation, si chaque variante est un singleton
+ row  `choices[derivation]`  `disjunction`  plie sur chaque variante, sans valeur en main
+ row  `singleton[derivation](label)`  `disjunction`  produit le singleton que nomme une étiquette
+ row  `variantLabels[derivation]`  `disjunction`  liste les étiquettes des variantes
```

- The `[field] => field => …` syntax is a *polymorphic lambda*, and it is worth a moment, because it
- is the one unfamiliar piece. It stands to a polymorphic method exactly as an ordinary lambda
- stands to an ordinary one. Where the lambda for `def transform(field: Field): Text` is
- `Field => Text`, the lambda for `def transform[field](field: field): Text` is
- `[field] => field => Text`.
+ La syntaxe `[field] => field => …` est une *lambda polymorphe*, et elle mérite qu'on s'y
+ arrête, car c'est la seule pièce peu familière. Elle est à une méthode polymorphe exactement ce
+ qu'une lambda ordinaire est à une méthode ordinaire. Là où la lambda de
+ `def transform(field: Field): Text` est `Field => Text`, la lambda de
+ `def transform[field](field: field): Text` est `[field] => field => Text`.

- It is needed because each field has a different type, and the body knows nothing at all about
- those types, except the one thing which matters: that two occurrences of `field` name the *same*
- type. That is enough, because it means the `contextual` instance supplied beside the field is a
- `Presentation[field]` for precisely this field's type, and can therefore be applied to it. The
- body of a derivation has no other way to do anything with a value whose type it cannot see.
- That is also why parametricity makes these bodies nearly write themselves.
+ Elle est nécessaire car chaque champ a un type différent, et le corps ne sait absolument rien
+ de ces types, sauf la seule chose qui compte : deux occurrences de `field` désignent le *même*
+ type. Cela suffit, car cela signifie que l'instance `contextual` fournie à côté du champ est un
+ `Presentation[field]` pour précisément le type de ce champ, et qu'elle peut donc lui être
+ appliquée. Le corps d'une dérivation n'a pas d'autre moyen de faire quoi que ce soit d'une
+ valeur dont il ne voit pas le type. C'est aussi pourquoi la paramétricité fait que ces corps
+ s'écrivent presque d'eux-mêmes.

- `contexts` is the same fold without a value to fold over, and gives the typeclass instance of
- each field alone. `typeName`, `tuple` and `singleton` describe the product being derived for,
- so a derivation can drop the type name for a tuple, or the parentheses for a singleton.
+ `contexts` est le même pli sans valeur sur laquelle plier, et donne seulement l'instance de la
+ classe de types de chaque champ. `typeName`, `tuple` et `singleton` décrivent le produit pour
+ lequel on dérive, si bien qu'une dérivation peut omettre le nom du type pour un tuple, ou les
+ parenthèses pour un singleton.

- ### Using a derived instance
+ ### Utiliser une instance dérivée

- With the object above in scope, any case class or enumeration has a `Presentation`. It is
- resolved automatically, or requested explicitly with a `derives` clause:
+ Avec l'objet ci-dessus dans la portée, toute case class ou énumération a une `Presentation`.
+ Elle est résolue automatiquement, ou demandée explicitement avec une clause `derives` :

```scala fuel
parse
##
case class Person(name: Text, age: Int) derives Presentation

val presented: Text = Person(t"Hamlet", 30).present
```

- Here `presented` is `t"Person(name=Hamlet, age=30)"`. Nested structures derive recursively. A
- case class of case classes needs nothing more.
+ Ici, `presented` vaut `t"Person(name=Hamlet, age=30)"`. Les structures imbriquées se dérivent
+ récursivement. Une case class de case classes n'a besoin de rien de plus.

- ### What a derivation can see
+ ### Ce qu'une dérivation peut voir

- Inside `conjunction`, each field carries more than its value, and the type being derived for can
- be described too:
+ Dans `conjunction`, chaque champ porte plus que sa valeur, et le type pour lequel on dérive
+ peut lui aussi être décrit :

```tabular
- column  Value
- column  Available in
- column  Gives
- row  `label`  the lambda of any primitive that visits fields or variants  the field's or variant's name as written
- row  `index`  the lambda of any primitive that visits fields or variants  its position
- row  `contextual`  the lambda of any primitive that visits fields or variants  the instance of the typeclass for its type
- row  `default`  the lambdas of `fields`, `contexts`, `build` and `construct`  the field's declared default, or `Unset`
- row  `typeName[derivation]`  a derivation  the name of the type being derived for
- row  `tuple[derivation]`  a derivation  whether that type is a tuple
- row  `singleton[derivation]`  a derivation  whether that type is a singleton
+ column  Valeur
+ column  Disponible dans
+ column  Donne
+ row  `label`  la lambda de toute primitive qui visite des champs ou des variantes  le nom du champ ou de la variante tel qu'écrit
+ row  `index`  la lambda de toute primitive qui visite des champs ou des variantes  sa position
+ row  `contextual`  la lambda de toute primitive qui visite des champs ou des variantes  l'instance de la classe de types pour son type
+ row  `default`  les lambdas de `fields`, `contexts`, `build` et `construct`  la valeur par défaut déclarée du champ, ou `Unset`
+ row  `typeName[derivation]`  une dérivation  le nom du type pour lequel on dérive
+ row  `tuple[derivation]`  une dérivation  si ce type est un tuple
+ row  `singleton[derivation]`  une dérivation  si ce type est un singleton
```

- A derivation can therefore produce something which mentions the structure, and not only
- something which folds over it. Here it produces the labels of a product's fields, with no value
- in hand, through `contexts`. Since derivation composes the instances of a product's fields,
- every field type needs one, even where it is trivial, as it is for the labels of a `Text` or an
- `Int`, which have none:
+ Une dérivation peut donc produire quelque chose qui mentionne la structure, et pas seulement
+ quelque chose qui la plie. Ici, elle produit les étiquettes des champs d'un produit, sans
+ valeur en main, via `contexts`. Puisque la dérivation compose les instances des champs d'un
+ produit, chaque type de champ en a besoin d'une, même lorsqu'elle est triviale, comme c'est le
+ cas pour les étiquettes d'un `Text` ou d'un `Int`, qui n'en ont pas :

```scala fuel
parse
##
trait Labels[value]:
  def labels: List[Text]

object Labels extends ProductDerivation[Labels]:
  given Labels[Text] = new Labels[Text] { def labels: List[Text] = Nil }
  given Labels[Int] = new Labels[Int] { def labels: List[Text] = Nil }

  inline def conjunction[derivation <: Product: ProductReflection]: Labels[derivation] =
    val fieldLabels = contexts[derivation]() { [field] => context => label }
    new Labels[derivation]:
      def labels: List[Text] = fieldLabels.to[List]

case class Empty()

Labels.derived[Person].labels
Labels.derived[Empty].labels
```

- `Labels.derived[Person].labels` is `List(t"name", t"age")`, and `Labels.derived[Empty].labels`
- is the empty list. An empty product is a product with no fields, and a single-field product is
- not a special case. Both derive from the same rule, which is where hand-written instances
- usually go wrong.
+ `Labels.derived[Person].labels` vaut `List(t"name", t"age")`, et
+ `Labels.derived[Empty].labels` est la liste vide. Un produit vide est un produit sans champ, et
+ un produit à un seul champ n'est pas un cas particulier. Tous deux se dérivent de la même
+ règle, et c'est là que les instances écrites à la main se trompent généralement.

- Inside `disjunction`, `variant` narrows the value to its actual case, so the body is typed at
- that case rather than at the sum, and the case's own label is available for a discriminator.
+ Dans `disjunction`, `variant` restreint la valeur à son cas effectif, si bien que le corps est
+ typé selon ce cas plutôt que selon la somme, et l'étiquette propre du cas est disponible pour
+ un discriminant.

- ### Producing a value rather than consuming one
+ ### Produire une valeur plutôt que la consommer

- A producer has no instance to fold over, so instead of `fields` it uses `build`, which constructs
- a new instance of the product. Its lambda receives the typeclass instance for each field, which
- is the only thing capable of making a value of a type the body cannot see, and returns the value
- of that field. Here is a parser of comma-separated fields, in full:
+ Un producteur n'a aucune instance sur laquelle plier ; au lieu de `fields`, il utilise donc
+ `build`, qui construit une nouvelle instance du produit. Sa lambda reçoit l'instance de la
+ classe de types pour chaque champ, la seule chose capable de créer une valeur d'un type que le
+ corps ne voit pas, et renvoie la valeur de ce champ. Voici un analyseur de champs séparés par
+ des virgules, en entier :

```scala fuel
parse
##
trait Parsing[value]:
  def parse(text: Text): value

object Parsing extends ProductDerivation[Parsing]:
  given Parsing[Text] = identity(_)
  given Parsing[Int] = _.s.toInt

  inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
    text =>
      val columns = text.cut(t",")
      build[derivation]:
        [field] => parsing => parsing.parse(columns(Ordinal.zerary(index)).or(t""))

Parsing.derived[Person].parse(t"Hamlet,30")
```

- `Parsing.derived[Person].parse(t"Hamlet,30")` is `Person(t"Hamlet", 30)`. `Parsing` is a
- typeclass of its own, and not the `Readable` which `read` looks for, so
- `t"Hamlet,30".read[Person]` does not compile. Its instances are reached through `derived`.
+ `Parsing.derived[Person].parse(t"Hamlet,30")` vaut `Person(t"Hamlet", 30)`. `Parsing` est
+ une classe de types à part entière, et non le `Readable` que cherche `read`, si bien que
+ `t"Hamlet,30".read[Person]` ne compile pas. On atteint ses instances via `derived`.

- The counterpart for sums is `delegate`, which takes the label of the variant to build, and
- dispatches to the instance of that variant. `variant` can read the answer off a value it was
- given. `delegate` is told which variant to produce, so the label usually comes out of the input,
- here a `Date:` or `DateTime:` prefix. A label which names no variant raises a `Variant.Error`:
+ L'équivalent pour les sommes est `delegate`, qui prend l'étiquette de la variante à
+ construire et répartit vers l'instance de cette variante. `variant` peut lire la réponse sur
+ une valeur qu'on lui a donnée. `delegate`, lui, se voit indiquer quelle variante produire ;
+ l'étiquette provient donc généralement de l'entrée, ici un préfixe `Date:` ou `DateTime:`. Une
+ étiquette qui ne nomme aucune variante lève une `Variant.Error` :

```scala fuel
parse
##
object ParsingSums extends Derivation[Parsing]:
  inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
    Parsing.conjunction[derivation]

  inline def disjunction[derivation: SumReflection]: Parsing[derivation] =
    text =>
      text.cut(t":") match
        case List(prefix: Text, rest: Text) =>
          delegate[derivation](prefix):
            [variant <: derivation] => parsing => parsing.parse(rest)
```

- Where the produced value is wrapped in a type constructor, as a parser which returns an
- `Optional` or a decoder which returns an `Attempt` is, `construct` takes the polymorphic `pure`
- and `bind` operations for that constructor, and threads the fields through it.
+ Lorsque la valeur produite est enveloppée dans un constructeur de type, comme c'est le cas d'un
+ analyseur qui renvoie un `Optional` ou d'un décodeur qui renvoie un `Attempt`, `construct`
+ prend les opérations polymorphes `pure` et `bind` de ce constructeur, et y fait passer les
+ champs.

- ### Typeclasses over two values
+ ### Classes de types sur deux valeurs

- Some typeclasses take two values of the same type. An equivalence is the obvious one. Folding
- over the fields of the left value is easy. The corresponding field of the right value is the
- problem. Doing it by hand would mean building parallel arrays of left fields, right fields and
- instances, and the moment those values are separated from the lambda which typed them, their
- types erase, and only casts put them back together.
+ Certaines classes de types prennent deux valeurs du même type. Une équivalence en est
+ l'exemple évident. Plier sur les champs de la valeur de gauche est facile. Le champ
+ correspondant de la valeur de droite est le problème. Le faire à la main impliquerait de
+ construire des tableaux parallèles de champs de gauche, de champs de droite et d'instances, et
+ dès que ces valeurs sont séparées de la lambda qui les typait, leurs types sont effacés, et
+ seuls des transtypages permettent de les réassembler.

- `dereference` and `complement` avoid that. Inside `contexts`, `dereference` is a function from
- the product to the current field, typed with that field's type, so the same field can be pulled
- out of *both* values and handed to the one `contextual` instance. For sums, `complement` does
- the corresponding job inside `variant`. It retrieves the other value, narrowed to the same case.
- It returns an [`Optional`](optional-values.md), since the two values need not be the same
- variant, and if they are not, there is no meaningfully typed value to return:
+ `dereference` et `complement` évitent cela. Dans `contexts`, `dereference` est une fonction du
+ produit vers le champ courant, typée avec le type de ce champ, si bien que le même champ peut
+ être extrait des *deux* valeurs et remis à l'unique instance `contextual`. Pour les sommes,
+ `complement` joue le rôle correspondant dans `variant`. Elle récupère l'autre valeur,
+ restreinte au même cas. Elle renvoie un [`Optional`](optional-values.md), puisque les deux
+ valeurs ne sont pas forcément de la même variante, et que si elles ne le sont pas, il n'existe
+ aucune valeur typée de manière sensée à renvoyer :

```scala fuel
parse
##
trait Equivalence[value]:
  def equal(left: value, right: value): Boolean

object Equivalence extends Derivation[Equivalence]:
  given Equivalence[Int] = _ == _
  given Equivalence[Text] = _.lower == _.lower

  inline def conjunction[derivation <: Product: ProductReflection]: Equivalence[derivation] =
    (left, right) =>
      contexts[derivation]():
        [field] => equivalence =>
          val extract: derivation => field = dereference
          equivalence.equal(extract(left), extract(right))
      . all { boolean => boolean }

  inline def disjunction[derivation: SumReflection]: Equivalence[derivation] =
    (left, right) =>
      variant(left):
        [variant <: derivation] => leftValue =>
          complement(right).lay(false)(contextual.equal(leftValue, _))

Equivalence.derived[Person].equal(Person(t"Hamlet", 30), Person(t"HAMLET", 30))
```

- `Equivalence.derived[Person].equal(Person(t"Hamlet", 30), Person(t"HAMLET", 30))` is `true`,
- since the instance for `Text` ignores case. Different variants compare unequal. The same
- variant compares through the instance which both share.
+ `Equivalence.derived[Person].equal(Person(t"Hamlet", 30), Person(t"HAMLET", 30))` vaut
+ `true`, puisque l'instance pour `Text` ignore la casse. Des variantes différentes sont
+ inégales. Une même variante se compare via l'instance que les deux partagent.

- ### Enumerations of singletons
+ ### Énumérations de singletons

- An enumeration whose cases are all singletons is a straight choice between names, with no data,
- and it supports things which a general sum does not. `choice` reports, at [compiletime],
- whether that is the case, so a derivation can restrict itself to such enumerations, and reject
- the rest with a message of its own:
+ Une énumération dont tous les cas sont des singletons est un simple choix entre des noms, sans
+ données, et elle permet des choses que ne permet pas une somme générale. `choice` indique,
+ [à la compilation][compilation], si c'est le cas, si bien qu'une dérivation peut se limiter à
+ de telles énumérations, et rejeter les autres avec un message de son cru :

```scala
trait Naming[value]:
  def name(value: value): Text

object Naming extends Derivation[Naming]:
  inline def conjunction[derivation <: Product: ProductReflection]: Naming[derivation] =
    value => typeName[derivation]

  inline def disjunction[derivation: SumReflection]: Naming[derivation] = value =>
    inline if choice[derivation] then
      variant(value):
        [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.name(arm)}"
    else scala.compiletime.error("cannot derive Naming for a sum whose variants carry data")

Naming.derived[Month].name(Month.Mar)
```

- `Naming.derived[Month].name(Month.Mar)` is `t"Month.Mar"`. The `inline if` matters. It forces
- `choice` to be evaluated as the code compiles, so the `scala.compiletime.error` branch is either
- eliminated or reached, and reaching it fails the build.
+ `Naming.derived[Month].name(Month.Mar)` vaut `t"Month.Mar"`. Le `inline if` compte. Il force
+ l'évaluation de `choice` pendant la compilation, si bien que la branche
+ `scala.compiletime.error` est soit éliminée, soit atteinte, et l'atteindre fait échouer le
+ build.

- For such an enumeration, `singleton` turns a label back into the value it names,
- `variantLabels` lists the labels, and `choices` folds over every variant without needing a value
- to dispatch on. That is what a derivation which produces a *schema*, rather than an instance,
- needs.
+ Pour une telle énumération, `singleton` reconvertit une étiquette en la valeur qu'elle nomme,
+ `variantLabels` liste les étiquettes, et `choices` plie sur chaque variante sans avoir besoin
+ d'une valeur sur laquelle répartir. C'est ce dont a besoin une dérivation qui produit un
+ *schéma*, plutôt qu'une instance.

- ### Default field values
+ ### Valeurs par défaut des champs

- A case class may give its fields default values, and a decoder usually wants them. A field
- absent from the input should take the default which the author wrote, and not fail. Within the
- lambdas of `fields`, `contexts` and `build`, a contextual `Default[Optional[field]]` is
- available, and calling `default` gives an `Optional[field]`: the declared default, or `Unset`
- where the field has none. Here is a parser which tolerates missing trailing columns:
+ Une case class peut donner des valeurs par défaut à ses champs, et un décodeur les veut
+ généralement. Un champ absent de l'entrée doit prendre la valeur par défaut que l'auteur a
+ écrite, et ne pas échouer. Dans les lambdas de `fields`, `contexts` et `build`, un
+ `Default[Optional[field]]` contextuel est disponible, et appeler `default` donne un
+ `Optional[field]` : la valeur par défaut déclarée, ou `Unset` si le champ n'en a pas. Voici un
+ analyseur qui tolère l'absence de colonnes finales :

```scala fuel
parse
frame
  label Every column must be present
  code
      object StrictParsing extends ProductDerivation[Parsing]:
        given Parsing[Text] = identity(_)
        given Parsing[Int] = _.s.toInt

        inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
          text =>
            val columns = text.cut(t",")
            build[derivation]:
              [field] => parsing => parsing.parse(columns(Ordinal.zerary(index)).or(t""))
frame
  label A missing column takes the field's default
  code
      object LenientParsing extends ProductDerivation[Parsing]:
        given Parsing[Text] = identity(_)
        given Parsing[Int] = _.s.toInt

        inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
          text =>
            val columns = text.cut(t",")
            build[derivation]:
              [field] => parsing =>
                columns(Ordinal.zerary(index)).let(parsing.parse(_))
                . or(default.or(panic(m"no column and no default for $label")))

      case class Settings(name: Text, retries: Int = 3)

      LenientParsing.derived[Settings].parse(t"primary")
##
```

- `LenientParsing.derived[Settings].parse(t"primary")` is `Settings(t"primary", 3)`.
+ `LenientParsing.derived[Settings].parse(t"primary")` vaut `Settings(t"primary", 3)`.

- ### Deriving beyond codecs
+ ### Dériver au-delà des codecs

- Nothing about the engine is specific to encoding. Any typeclass whose instances compose
- structurally derives the same way, including the arithmetic ones, which combine two values of a
- product field by field:
+ Rien dans le moteur n'est propre à l'encodage. Toute classe de types dont les instances se
+ composent structurellement se dérive de la même manière, y compris les classes arithmétiques,
+ qui combinent deux valeurs d'un produit champ par champ :

```scala
import arithmetic.addable

case class Pair(label: Text, count: Int)

val sum: Pair = Pair(t"Rosen", 10) + Pair(t"crantz", 15)
```

- Here `sum` is `Pair(t"Rosencrantz", 25)`. Addition on `Pair` is addition on each field, so
- `Text` concatenates while `Int` sums, and no instance is written for `Pair` at all.
- `Subtractable`, `Multiplicable` and their siblings follow the same rule.
+ Ici, `sum` vaut `Pair(t"Rosencrantz", 25)`. L'addition sur `Pair` est l'addition sur chaque
+ champ, si bien que `Text` concatène tandis que `Int` additionne, et qu'aucune instance n'est
+ écrite pour `Pair`. `Subtractable`, `Multiplicable` et leurs semblables suivent la même règle.

- ### When derivation fails
+ ### Quand la dérivation échoue

- A typeclass may sensibly derive for products but not for coproducts, or the reverse. Extending
- `ProductDerivation` rather than `Derivation` says so, and applying it to a sum type is then a
- compile error which names the mismatch, and not a surprise at [runtime]. A type with no `Mirror`
- at all, such as an ordinary trait or a class which is not a case class, likewise fails at the
- point of derivation, where the message can say what is missing.
+ Une classe de types peut raisonnablement se dériver pour les produits mais pas pour les
+ coproduits, ou l'inverse. Étendre `ProductDerivation` plutôt que `Derivation` le dit, et
+ l'appliquer à un type somme est alors une erreur de compilation qui nomme l'incompatibilité,
+ et non une surprise [à l'exécution][exécution]. Un type sans aucun `Mirror`, comme un trait
+ ordinaire ou une classe qui n'est pas une case class, échoue de même au point de dérivation,
+ où le message peut dire ce qui manque.

- Three failures are common enough to name:
+ Trois échecs sont assez courants pour être nommés :

```tabular
- column  Symptom
- column  Usual cause
- column  Fix
- row  a long message ending "given instance derived in trait Derivation does not match type…"  a polymorphic lambda's type variable has no upper bound  write `[variant <: derivation]`, not `[variant]`
- row  a type mismatch against `Product`  `conjunction`'s type parameter lacks its bound  add `<: Product`
- row  no instance found for a type whose fields look fine  one of *those* fields lacks an instance  compile an explicit call to `derived`, which says which
+ column  Symptôme
+ column  Cause habituelle
+ column  Correctif
+ row  un long message se terminant par "given instance derived in trait Derivation does not match type…"  la variable de type d'une lambda polymorphe n'a pas de borne supérieure  écrire `[variant <: derivation]`, et non `[variant]`
+ row  une incompatibilité de type avec `Product`  il manque sa borne au paramètre de type de `conjunction`  ajouter `<: Product`
+ row  aucune instance trouvée pour un type dont les champs semblent corrects  l'un de *ces* champs n'a pas d'instance  compiler un appel explicite à `derived`, qui indique lequel
```

- A recursive type is a special case. It cannot be derived in place, because the derivation would
- have to summon itself part of the way through. Defining the instance on the type's companion,
- with a `derives` clause, breaks the cycle:
+ Un type récursif est un cas particulier. Il ne peut pas être dérivé sur place, car la
+ dérivation devrait s'invoquer elle-même en cours de route. Définir l'instance sur le compagnon
+ du type, avec une clause `derives`, rompt le cycle :

```scala fuel
parse
##
enum Tree derives Presentation:
  case Leaf
  case Branch(left: Tree, value: Int, right: Tree)

val tree: Text = Tree.Branch(Tree.Leaf, 1, Tree.Leaf).present
```

- Here `tree` is `t"Branch(left=Leaf, value=1, right=Leaf)"`.
+ Ici, `tree` vaut `t"Branch(left=Leaf, value=1, right=Leaf)"`.

- ### Choosing between candidate instances
+ ### Choisir entre des instances candidates

- A derived instance comes from a `given` inherited from `Derivation`, so it sits at lower
- priority than any `given` written in the body of the companion. That is usually what is wanted,
- and occasionally not. Where a derived instance and a hand-written one are truly ambiguous,
- adjusting priorities tends to resolve one ambiguity by creating another elsewhere.
+ Une instance dérivée provient d'un `given` hérité de `Derivation` ; elle a donc une priorité
+ plus basse que tout `given` écrit dans le corps du compagnon. C'est généralement ce que l'on
+ veut, et parfois non. Lorsqu'une instance dérivée et une instance écrite à la main sont
+ réellement ambiguës, ajuster les priorités tend à résoudre une ambiguïté en en créant une autre
+ ailleurs.

- The reliable fix is to make the choice explicit, and not to rank it. Turn the competing `given`s
- into ordinary methods, and write a single `derived` which selects among them in order, with
- `scala.compiletime.summonFrom`:
+ Le correctif fiable consiste à rendre le choix explicite, et non à le hiérarchiser.
+ Transformez les `given` concurrents en méthodes ordinaires, et écrivez un unique `derived` qui
+ choisit parmi elles dans l'ordre, avec `scala.compiletime.summonFrom` :

```scala
trait Rendering[value]:
  def render(value: value): Text

object Rendering:
  inline given derived[Value]: Rendering[Value] = item =>
    scala.compiletime.summonFrom:
      case presentation: Presentation[Value] => presentation.present(item)
      case given (Value is Showable)         => item.show
      case _                                 => item.toString.tt

extension [value](value: value)
  def render(using rendering: Rendering[value]): Text = rendering.render(value)
```

```scala fuel
parse
##
val person: Text = Person(t"Hamlet", 30).render
val number: Text = 3.14.render
```

- The first goes through `Presentation`, which `Person` derives. The second goes through
- `Showable`, which `Double` has. Read plainly: use a `Presentation` if one exists; otherwise a
- `Showable`, brought into scope for the right-hand side; otherwise fall back. The order in the
- source *is* the priority, which is why this is easier to reason about than a lattice of implicit
- scopes. The type parameter is capitalized here for a reason. In a `summonFrom` pattern, a
- lowercase type name would be read as a fresh pattern variable, and not as the given's own
- parameter.
+ Le premier passe par `Presentation`, que `Person` dérive. Le second passe par `Showable`, que
+ `Double` possède. Lu simplement : utiliser une `Presentation` s'il en existe une ; sinon un
+ `Showable`, mis dans la portée pour le membre de droite ; sinon se replier. L'ordre dans le
+ code source *est* la priorité, et c'est pourquoi il est plus facile de raisonner sur cette
+ forme que sur un treillis de portées implicites. Le paramètre de type prend ici une majuscule
+ pour une raison. Dans un motif de `summonFrom`, un nom de type en minuscules serait lu comme
+ une nouvelle variable de motif, et non comme le paramètre propre du given.

- The same technique gives derivation without exposing it. Define `conjunction` and `disjunction`
- on an ordinary object rather than the companion, and call `derived` on it explicitly wherever an
- instance is wanted, as `ParsingSums` and `LenientParsing` did above. Nothing then reaches
- implicit search unless it is asked for.
+ La même technique fournit la dérivation sans l'exposer. Définissez `conjunction` et
+ `disjunction` sur un objet ordinaire plutôt que sur le compagnon, et appelez `derived` dessus
+ explicitement partout où une instance est voulue, comme l'ont fait `ParsingSums` et
+ `LenientParsing` plus haut. Rien n'atteint alors la recherche implicite sans avoir été demandé.

- ### Per-field instances
+ ### Instances par champ

- Occasionally one field of one type needs a different instance from the default: a special codec
- for one column, or an override for one path. `specifically` builds such a map of overrides by
- naming the paths, which are checked against the actual structure of the type:
+ Il arrive qu'un champ d'un type ait besoin d'une instance différente de celle par défaut : un
+ codec spécial pour une colonne, ou une redéfinition pour un chemin. `specifically` construit
+ une telle map de redéfinitions en nommant les chemins, qui sont vérifiés par rapport à la
+ structure réelle du type :

```scala fuel
parse
##
case class Play(hero: Person, villain: Person)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

given playSpecific: (Play is Specific over Json.Encodable) =
  specifically:
    case root.villain.name() => shout

import formatting.compactJsonFormatting

val encoded: Text = Play(Person(t"Othello", 40), Person(t"Iago", 28)).in[Json].show
```

- Here `encoded` is `{"hero":{"name":"Othello","age":40},"villain":{"name":"IAGO","age":28}}`. A
- misspelled path, or an instance of the wrong type for its field, is a compile error. A
- derivation which supports overrides consults the map at each field it visits, as
- [JSON](json.md) and [catalogs](catalogs.md) do.
+ Ici, `encoded` vaut
+ `{"hero":{"name":"Othello","age":40},"villain":{"name":"IAGO","age":28}}`. Un chemin mal
+ orthographié, ou une instance du mauvais type pour son champ, est une erreur de compilation.
+ Une dérivation qui prend en charge les redéfinitions consulte la map à chaque champ qu'elle
+ visite, comme le font [JSON](json.md) et les [catalogues](catalogs.md).

- ### Related
+ ### Voir aussi

- - [JSON](json.md), [CSV](csv.md), [XML](xml.md) and the other formats derive their codecs this
-   way.
- - [Annotations](annotations.md) describes the `@name` annotation which a derivation reads to
-   rename a field.
- - [Catalogs](catalogs.md) and [optics](optics.md) describe `Specific` and the field paths it is
-   keyed by.
- - [Metaprogramming](metaprogramming.md) describes the compiletime tools beneath the engine.
- - [Generic data](generic-data.md) describes a derived codec over structures of any shape.
- - [Small APIs](../../philosophy/small-apis.md) explains why two rules are better than an instance
-   for each type.
- - [Display](display.md) and [numbers](numbers.md) derive their instances for case classes, and [errors](errors.md) describes the diagnosis when a derivation fails.
+ - [JSON](json.md), [CSV](csv.md), [XML](xml.md) et les autres formats dérivent leurs codecs de
+   cette manière.
+ - [Annotations](annotations.md) décrit l'annotation `@name` qu'une dérivation lit pour renommer
+   un champ.
+ - [Catalogues](catalogs.md) et [optiques](optics.md) décrivent `Specific` et les chemins de
+   champ qui lui servent de clés.
+ - [Métaprogrammation](metaprogramming.md) décrit les outils de compilation sous-jacents au
+   moteur.
+ - [Données génériques](generic-data.md) décrit un codec dérivé sur des structures de forme
+   quelconque.
+ - [Petites API](../../philosophy/small-apis.md) explique pourquoi deux règles valent mieux
+   qu'une instance pour chaque type.
+ - [Affichage](display.md) et [nombres](numbers.md) dérivent leurs instances pour les case
+   classes, et [erreurs](errors.md) décrit le diagnostic lorsqu'une dérivation échoue.
