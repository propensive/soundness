- ## Interpolation
+ ## Interpolation

- ### About
+ ### Présentation

- Checked literals are used throughout Soundness: a `url"…"` which must be a URL, an `sh"…"`
- whose quoting must balance, a `j"…"` which must be JSON. All of them are built on one piece of
- machinery for defining custom
- [string interpolators](https://docs.scala-lang.org/scala3/book/string-interpolation.html).
+ Les littéraux vérifiés sont utilisés dans tout Soundness : un `url"…"` qui doit être une
+ URL, un `sh"…"` dont les guillemets doivent s'équilibrer, un `j"…"` qui doit être du JSON.
+ Tous reposent sur une même mécanique pour définir des
+ [interpolateurs de chaînes](https://docs.scala-lang.org/scala3/book/string-interpolation.html)
+ personnalisés.

- An [interpolator] defined with it does two things at [compiletime]. It validates the literal's
- content, and reports an error at the exact character at fault. And it accepts substitutions
- only of the types it declares, each checked for the position it occupies. This is the principal
- instrument of [safety by construction](../../philosophy/safety-by-construction.md).
+ Un [interpolateur] défini avec elle fait deux choses [à la compilation][compilation]. Il
+ valide le contenu du littéral, et signale une erreur au caractère exact qui est en faute.
+ Et il n'accepte que des substitutions des types qu'il déclare, chacune vérifiée pour la
+ position qu'elle occupe. C'est le principal instrument de la
+ [sûreté par construction](../../philosophy/safety-by-construction.md).

- Defining an interpolator does not mean writing a macro by hand. The work of capturing the
- literal's parts, and their positions in the source file, is done once. The author of an
- interpolator supplies ordinary [typeclass] instances, which say how the parts are parsed and
- what may be substituted where.
+ Définir un interpolateur ne signifie pas écrire une macro à la main. Le travail de capture
+ des parties du littéral, et de leurs positions dans le fichier source, est fait une fois
+ pour toutes. L'auteur d'un interpolateur fournit des instances ordinaires de
+ [classes de types][classe de types], qui indiquent comment les parties sont analysées et ce
+ qui peut être substitué à quel endroit.

- ### On interpolators
+ ### À propos des interpolateurs

- Scala's built-in `s"…"` interpolation glues strings together without types. Any value is
- converted with `toString`, and the literal parts mean nothing to the compiler. Yet a string
- literal very often *is* something: a URL, a path, a regular expression, a fragment of SQL. It
- has a syntax which could be checked the moment it is written, and positions where only certain
- values make sense.
+ L'interpolation `s"…"` intégrée à Scala colle des chaînes ensemble sans types. Toute valeur
+ est convertie avec `toString`, et les parties littérales ne signifient rien pour le
+ compilateur. Pourtant, un littéral de chaîne *est* très souvent quelque chose : une URL, un
+ chemin, une expression régulière, un fragment de SQL. Il a une syntaxe qui pourrait être
+ vérifiée au moment même où il est écrit, et des positions où seules certaines valeurs ont du
+ sens.

- Here an interpolated string is treated as a small program to be compiled:
+ Ici, une chaîne interpolée est traitée comme un petit programme à compiler :

- - The literal parts are parsed at compiletime, by the interpolator's own logic, so a malformed
-   literal never reaches a running program.
- - Substitutions are resolved through typeclasses. What may be substituted, and how it is made
-   safe for its position, is decided by instances in scope, not by `toString`.
- - The source positions of the parts are captured, so an error message points at the offending
-   character, not at the whole expression.
+ - Les parties littérales sont analysées à la compilation, par la logique propre à
+   l'interpolateur ; un littéral mal formé n'atteint donc jamais un programme en cours
+   d'exécution.
+ - Les substitutions sont résolues au moyen de classes de types. Ce qui peut être substitué,
+   et la manière de le rendre sûr pour sa position, est décidé par les instances dans la
+   portée, et non par `toString`.
+ - Les positions des parties dans le code source sont capturées ; un message d'erreur pointe
+   donc sur le caractère fautif, et non sur l'expression entière.

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*
```

- A [media type](media-types.md) literal shows the result. A well-formed one is a `MediaType`,
- and a misspelled one does not compile:
+ Un littéral de [type de média](media-types.md) montre le résultat. Un littéral bien formé
+ est un `MediaType`, et un littéral mal orthographié ne compile pas :

```scala
val plain: MediaType = media"text/plain"
```

```scala fuel
compile
errors show
##
media"text/plian"
```

- ### Anatomy of an interpolator
+ ### Anatomie d'un interpolateur

- An interpolator is made of three pieces. The first is a prefix method on `StringContext`, which
- names the interpolator and hands its content to the machinery:
+ Un interpolateur se compose de trois éléments. Le premier est une méthode de préfixe sur
+ `StringContext`, qui nomme l'interpolateur et transmet son contenu à la mécanique :

```scala
extension (inline context: StringContext)
  transparent inline def media: Interpolation = interpolation[MediaType](context)
```

- The second is an `Interpolable` instance for the target type, which does the parsing and the
- construction. It receives the literal parts with their source positions, and produces the
- value, reporting any fault against the character which caused it.
+ Le deuxième est une instance d'`Interpolable` pour le type cible, qui effectue l'analyse et
+ la construction. Elle reçoit les parties littérales avec leurs positions dans le code
+ source, et produit la valeur, en signalant toute faute au caractère qui l'a causée.

- The third is a set of `Insertion` instances, which declare the types that may be substituted,
- and how each becomes part of the result.
+ Le troisième est un ensemble d'instances d'`Insertion`, qui déclarent les types pouvant
+ être substitués, et comment chacun devient une partie du résultat.

- ### Substitutions
+ ### Substitutions

- An `Insertion` maps a substituted value into the interpolator's own input type, so the
- interpolator controls how foreign values enter. The insertions of the
- [shell interpolator](processes.md) show the idea. Text becomes one argument, and a list becomes
- several:
+ Une `Insertion` convertit une valeur substituée vers le type d'entrée propre à
+ l'interpolateur ; l'interpolateur contrôle donc la manière dont les valeurs étrangères
+ entrent. Les insertions de l'[interpolateur shell](processes.md) illustrent l'idée. Un texte
+ devient un argument, et une liste en devient plusieurs :

```scala
given Insertion[Sh.Parameters, Text] = value => Sh.Parameters(value)
given Insertion[Sh.Parameters, List[Text]] = xs => Sh.Parameters(xs*)
```

- Insertion is a typeclass, so a new type becomes substitutable by declaring one instance,
- without touching the interpolator. A type with no instance cannot be substituted at all, which
- is how an interpolator forbids what it cannot make safe.
+ Insertion est une classe de types ; un nouveau type devient donc substituable en déclarant
+ une instance, sans toucher à l'interpolateur. Un type sans instance ne peut pas être
+ substitué du tout, et c'est ainsi qu'un interpolateur interdit ce qu'il ne peut pas rendre
+ sûr.

- Sometimes different positions accept different things. A URL's port takes a number, and its
- path takes text. A `Substitution` is an `Insertion` with one more type parameter, a singleton
- `Label`, which tells the positions apart.
+ Parfois, différentes positions acceptent différentes choses. Le port d'une URL prend un
+ nombre, et son chemin prend du texte. Une `Substitution` est une `Insertion` avec un
+ paramètre de type de plus, un `Label` singleton, qui distingue les positions.

- At [runtime] the two behave identically. The difference is at compiletime. An insertion tells
- the interpolator's parser only that *something* was substituted. A substitution passes the
- parser its label, and the parser proceeds as though the position held a representative token of
- the substituted type: `"0"` for a number, `""""` for a string.
+ [À l'exécution][exécution], les deux se comportent de manière identique. La différence se
+ situe à la compilation. Une insertion indique seulement à l'analyseur de l'interpolateur
+ que *quelque chose* a été substitué. Une substitution transmet son étiquette à l'analyseur,
+ et celui-ci procède comme si la position contenait un jeton représentatif du type
+ substitué : `"0"` pour un nombre, `""""` pour une chaîne.

```scala
enum SqlInput:
  case IntInput(value: Int)
  case StrInput(value: Text)

given Substitution[SqlInput, Int, "0"] = SqlInput.IntInput(_)
given Substitution[SqlInput, Text, "\"\""] = SqlInput.StrInput(_)
```

- The interpolator's compiletime reading of the literal can then depend on what was substituted.
- It can check the *shape* of a literal which contains substitutions, instead of giving up
- wherever one appears.
+ La lecture du littéral par l'interpolateur à la compilation peut alors dépendre de ce qui a
+ été substitué. Il peut vérifier la *forme* d'un littéral qui contient des substitutions, au
+ lieu d'abandonner partout où l'une d'elles apparaît.

- ### Patterns
+ ### Motifs

- The same machinery runs in reverse. An `Extrapolable` instance lets an interpolated literal
- serve as a pattern in a `match`, taking a value apart and binding its substitutions. This is
- how the patterns of [`j"…"`](json.md#matching-json) and [`html"…"`](html.md) work. One
- definition gives both the construction and the match.
+ La même mécanique fonctionne en sens inverse. Une instance d'`Extrapolable` permet à un
+ littéral interpolé de servir de motif dans un `match`, en décomposant une valeur et en
+ liant ses substitutions. C'est ainsi que fonctionnent les motifs de
+ [`j"…"`](json.md#matching-json) et de [`html"…"`](html.md). Une seule définition donne à la
+ fois la construction et le filtrage.

- The four typeclasses, side by side:
+ Les quatre classes de types, côte à côte :

```tabular
- column  Typeclass
- column  Given for
- column  Says
- row  `Interpolable`  the result type  how the literal parts are parsed into a value
- row  `Insertion[input, value]`  a substitutable type  how a value enters the interpolator's input
- row  `Substitution[input, value, label]`  a substitutable type  the same, and which token the parser sees in its place
- row  `Extrapolable`  the matched type  how a value is taken apart by a literal pattern
+ column  Classe de types
+ column  Given pour
+ column  Indique
+ row  `Interpolable`  le type du résultat  comment les parties littérales sont analysées en une valeur
+ row  `Insertion[input, value]`  un type substituable  comment une valeur entre dans l'entrée de l'interpolateur
+ row  `Substitution[input, value, label]`  un type substituable  la même chose, et quel jeton l'analyseur voit à sa place
+ row  `Extrapolable`  le type filtré  comment une valeur est décomposée par un motif littéral
```

- ### Compiletime and runtime
+ ### Compilation et exécution

- An interpolator's parsing logic is ordinary code, and it runs in both worlds. At compiletime it
- checks literals, and gives compile errors. At runtime it parses input which is built
- dynamically, and gives typed [errors](errors.md).
+ La logique d'analyse d'un interpolateur est du code ordinaire, et elle s'exécute dans les
+ deux mondes. À la compilation, elle vérifie les littéraux, et produit des erreurs de
+ compilation. À l'exécution, elle analyse des entrées construites dynamiquement, et produit
+ des [erreurs](errors.md) typées.

- The two cannot drift apart, because they are the same code. A hand-written macro with a
- separate runtime parser can make no such guarantee.
+ Les deux ne peuvent pas diverger, car ce sont le même code. Une macro écrite à la main avec
+ un analyseur distinct pour l'exécution ne peut offrir aucune garantie de ce genre.

- ### Parts and origins
+ ### Parties et origines

- `interpolate` receives two type parameters, which carry what the compiler knows about the
- literal:
+ `interpolate` reçoit deux paramètres de type, qui portent ce que le compilateur sait du
+ littéral :

```tabular
- column  Type parameter
- column  Carries
- row  `parts`  a tuple of the literal's fragments, as singleton string types, so the interpolator sees the text of each
- row  `origins`  where each fragment began in the source file, so an error against a character resolves to that character's position
+ column  Paramètre de type
+ column  Porte
+ row  `parts`  un tuple des fragments du littéral, sous forme de types chaîne singletons, afin que l'interpolateur voie le texte de chacun
+ row  `origins`  l'endroit où chaque fragment commençait dans le fichier source, afin qu'une erreur sur un caractère se résolve en la position de ce caractère
```

- The media-type interpolator's instance, in outline, hands both to a macro:
+ L'instance de l'interpolateur de types de média, dans les grandes lignes, transmet les deux à
+ une macro :

```scala fuel
parse
##
import gesticulate.internal

inline given interpolable: MediaType is Interpolable:
  transparent inline def interpolate[parts <: Tuple, origins <: Tuple](inline insertions: Any*)
  :   MediaType =

    ${internal.mediaInterpolator[parts]('insertions)}
```

- That is what makes the compile error for `media"text/plian"` point at the misspelled subtype,
- not at the whole literal. [Metaprogramming](metaprogramming.md) describes the macros
- themselves.
+ C'est ce qui fait que l'erreur de compilation pour `media"text/plian"` pointe sur le
+ sous-type mal orthographié, et non sur le littéral entier.
+ [Métaprogrammation](metaprogramming.md) décrit les macros elles-mêmes.

- ### Returning a precise type
+ ### Renvoyer un type précis

- The prefix method is `transparent inline`, so an interpolator may return something more
- specific than its declared type. A literal can thereby carry what it *is* into the type system,
- beyond merely being valid:
+ La méthode de préfixe est `transparent inline` ; un interpolateur peut donc renvoyer quelque
+ chose de plus spécifique que son type déclaré. Un littéral peut ainsi faire entrer ce qu'il
+ *est* dans le système de types, au-delà du simple fait d'être valide :

- - A [path](paths.md) literal returns a path on a particular platform.
- - A name literal returns a name which is proved valid for its kind.
- - A document literal checked against a schema returns a document typed by that schema.
+ - Un littéral de [chemin](paths.md) renvoie un chemin sur une plateforme particulière.
+ - Un littéral de nom renvoie un nom dont la validité pour son genre est prouvée.
+ - Un littéral de document vérifié par rapport à un schéma renvoie un document typé par ce
+   schéma.

- The cost of that precision is that the result type depends on the literal's content, so the
- literal must be known at compiletime. Where it is not, the corresponding runtime parser, which
- is the same code, returns the general type and a typed error.
+ Le prix de cette précision est que le type du résultat dépend du contenu du littéral, qui
+ doit donc être connu à la compilation. Lorsqu'il ne l'est pas, l'analyseur correspondant à
+ l'exécution, qui est le même code, renvoie le type général et une erreur typée.

- ### Choosing between compiletime and runtime
+ ### Choisir entre compilation et exécution

- An interpolator is the right tool where a literal has a syntax, and the syntax can be checked.
- It is the wrong tool where the content is truly dynamic. There is then nothing to check as the
- code compiles, and the interpolator's machinery buys nothing over the runtime parser it
- delegates to.
+ Un interpolateur est le bon outil lorsqu'un littéral a une syntaxe, et que cette syntaxe
+ peut être vérifiée. C'est le mauvais outil lorsque le contenu est réellement dynamique. Il
+ n'y a alors rien à vérifier pendant la compilation du code, et la mécanique de
+ l'interpolateur n'apporte rien de plus que l'analyseur à l'exécution auquel elle délègue.

- The convention throughout is therefore to offer both: `url"…"` for a literal, and
- [`as[Url]`](decoding.md#decoding-a-value) for text obtained at runtime. The same code is behind
- each, so a program can move a value from one to the other without any change in behavior.
+ La convention suivie partout est donc d'offrir les deux : `url"…"` pour un littéral, et
+ [`as[Url]`](decoding.md#decoding-a-value) pour un texte obtenu à l'exécution. Le même code
+ se trouve derrière chacun ; un programme peut donc faire passer une valeur de l'un à l'autre
+ sans aucun changement de comportement.

- ### Related
+ ### Voir aussi

- Interpolators defined this way include those for [text](text.md#writing-text),
- [JSON](json.md#building-json), [XML](xml.md) and [HTML](html.md),
- [URLs and other network addresses](network-addresses.md), [paths](paths.md),
- [regular expressions](patterns.md), [shell commands](processes.md),
- [media types](media-types.md) and [error messages](errors.md#defining-an-error).
- - [Names](names.md) checks an identifier with an interpolator.
+ Parmi les interpolateurs définis de cette manière figurent ceux pour le
+ [texte](text.md#writing-text), [JSON](json.md#building-json), [XML](xml.md) et
+ [HTML](html.md), les [URL et autres adresses réseau](network-addresses.md), les
+ [chemins](paths.md), les [expressions régulières](patterns.md), les
+ [commandes shell](processes.md), les [types de média](media-types.md) et les
+ [messages d'erreur](errors.md#defining-an-error).
+ - [Noms](names.md) vérifie un identifiant au moyen d'un interpolateur.
