- ## Foreign Interoperability
+ ## Interopérabilité étrangère

- ### About
+ ### Présentation

- Other languages have type systems too. Their definitions, such as a TypeScript declaration file,
- a C header, a [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) interface or a WebAssembly
- [WIT](https://component-model.bytecodealliance.org/design/wit.html) description, say precisely
- which members exist and what they accept.
+ Les autres langages ont eux aussi des systèmes de types. Leurs définitions, comme un fichier
+ de déclarations TypeScript, un en-tête C, une interface
+ [WebIDL](https://en.wikipedia.org/wiki/Web_IDL) ou une description
+ [WIT](https://component-model.bytecodealliance.org/design/wit.html) WebAssembly, indiquent
+ précisément quels membres existent et ce qu'ils acceptent.

- Those definitions are read as the code compiles, and *foreign* expressions are checked against
- them. Navigating a foreign type, selecting a member and applying a function are each verified
- against the foreign language's own declarations.
+ Ces définitions sont lues pendant la compilation du code, et les expressions *étrangères*
+ sont vérifiées par rapport à elles. Parcourir un type étranger, sélectionner un membre et
+ appliquer une fonction sont chacun vérifiés par rapport aux propres déclarations du langage
+ étranger.

- An `Ecosystem` names a foreign world and the grammar its definitions are written in. An
- `Interface` loads a definition file for it. A `Foreign` value navigates the declared types, and
- produces an expression to hand to whatever executes in that world. This is the mechanism beneath
- the [typed DOM scripting](web-automation.md) of the browser, and it is open to any ecosystem with
- a readable interface language.
+ Un `Ecosystem` nomme un monde étranger et la grammaire dans laquelle ses définitions sont
+ écrites. Une `Interface` charge pour lui un fichier de définitions. Une valeur `Foreign`
+ parcourt les types déclarés, et produit une expression à transmettre à ce qui s'exécute dans
+ ce monde. C'est le mécanisme qui sous-tend le
+ [scripting DOM typé](web-automation.md) du navigateur, et il est ouvert à tout écosystème
+ doté d'un langage d'interface lisible.

- ### On foreign types
+ ### À propos des types étrangers

- Interoperation usually discards the foreign type system at the border. A JavaScript call from
- Scala is a string. A C function is a binding declared by hand. The authoritative declarations,
- the `*.d.ts`, the header or the IDL, sit unread while the programmer transcribes them, and the
- mistakes are exactly the ones which the foreign compiler would have caught.
+ L'interopérabilité abandonne habituellement le système de types étranger à la frontière. Un
+ appel JavaScript depuis Scala est une chaîne. Une fonction C est une liaison déclarée à la
+ main. Les déclarations qui font autorité, le `*.d.ts`, l'en-tête ou l'IDL, restent non lues
+ pendant que le programmeur les retranscrit, et les erreurs sont exactement celles que le
+ compilateur étranger aurait détectées.

- Here the declarations are read instead. The definition file is parsed at [compiletime], each
- step of a navigation is resolved against it, and a member which does not exist, or an argument of
- the wrong foreign type, is a Scala compile error. Checking a foreign call against the foreign
- world's own definitions is
- [safety by construction](../../philosophy/safety-by-construction.md) across a language boundary.
+ Ici, les déclarations sont lues à la place. Le fichier de définitions est analysé
+ [à la compilation][compilation], chaque étape d'un parcours est résolue par rapport à lui, et
+ un membre qui n'existe pas, ou un argument du mauvais type étranger, est une erreur de
+ compilation Scala. Vérifier un appel étranger par rapport aux propres définitions du monde
+ étranger relève de la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) par-delà une frontière
+ entre langages.

- Everything comes from the `soundness` package:
+ Tout provient du package `soundness` :

```scala
import soundness.*
```

- ### Loading an interface
+ ### Charger une interface

- An `Interface` binds an ecosystem to a definition file on the [classpath](classpath.md). The
- file is read as the code compiles, so it must be on the compiler's classpath. Each supported
- grammar is a `Dialect` which the ecosystem names:
+ Une `Interface` lie un écosystème à un fichier de définitions présent dans le
+ [classpath](classpath.md). Le fichier est lu pendant la compilation du code ; il doit donc
+ se trouver dans le classpath du compilateur. Chaque grammaire prise en charge est un
+ `Dialect` que nomme l'écosystème :

```tabular
- column  Definitions
- column  Ecosystem
- column  Dialect
- row  TypeScript declarations  `Typescript`  `TypescriptDialect`
- row  C headers  `Native`  `CHeader.Dialect`
- row  WebIDL  `WebIdl`  `WebIdlDialect`
- row  WIT  `Wit`  `WitDialect`
- row  Kotlin `@Metadata` in classfiles  `Kotlin`  `KotlinDialect`
+ column  Définitions
+ column  Écosystème
+ column  Dialecte
+ row  déclarations TypeScript  `Typescript`  `TypescriptDialect`
+ row  en-têtes C  `Native`  `CHeader.Dialect`
+ row  WebIDL  `WebIdl`  `WebIdlDialect`
+ row  WIT  `Wit`  `WitDialect`
+ row  `@Metadata` Kotlin dans les fichiers de classe  `Kotlin`  `KotlinDialect`
```

- A TypeScript interface, for example:
+ Une interface TypeScript, par exemple :

```scala fuel
typecheck
##
given (Interface in Typescript at "/xenophile/definitions.ts") =
  Interface[Typescript](cp"/xenophile/definitions.ts")
```

- ### Navigating foreign types
+ ### Parcourir les types étrangers

- A `Foreign` value stands for a value of a foreign type. Its members navigate as though the
- foreign declarations were Scala. The type of each step comes from the declarations, so the
- refinement tracks exactly what the foreign type system says. Here `Play` declares a member
- `scene` of type `Scene`, and a method `greet` which takes a string:
+ Une valeur `Foreign` représente une valeur d'un type étranger. Ses membres se parcourent
+ comme si les déclarations étrangères étaient du Scala. Le type de chaque étape provient des
+ déclarations ; le raffinement suit donc exactement ce que dit le système de types étranger.
+ Ici, `Play` déclare un membre `scene` de type `Scene`, et une méthode `greet` qui prend une
+ chaîne :

```scala fuel
typecheck
##
val play: Foreign of "Play" from Typescript = Foreign["Play", Typescript]

val scene: Foreign of "Scene" from Typescript = play.scene
val greeting = play.greet(t"hello")
```

- `Play` declares no member called `missing`, and `greet` does not take a number, so neither of
- these compiles:
+ `Play` ne déclare aucun membre appelé `missing`, et `greet` ne prend pas de nombre ; aucune
+ de ces deux lignes ne compile donc :

```scala fuel
compile
errors show
##
play.missing
```

```scala fuel
compile
errors show
##
play.greet(42)
```

- Scala values pass as arguments where an `Interoperable` instance maps them onto the foreign
- type: text to a string, and the sized numbers to their foreign widths. The boundary is typed in
- both directions.
+ Les valeurs Scala passent comme arguments là où une instance d'`Interoperable` les fait
+ correspondre au type étranger : le texte à une chaîne, et les nombres à taille fixe à leurs
+ largeurs étrangères. La frontière est typée dans les deux sens.

- ### Kotlin and Java facades
+ ### Façades Kotlin et Java

- A JVM library written in Kotlin is on the same classpath, so its declarations need no separate
- file. They are in the classfiles, in the `@Metadata` which a Kotlin compiler writes. Those
- declarations are read at compiletime, and a *facade* wraps a live value so that its members are
- reached as though they were Scala's. Each access becomes a direct, statically-typed JVM call,
- with no reflection and no wrapper object. With the Kotlin standard library on the classpath:
+ Une bibliothèque JVM écrite en Kotlin se trouve dans le même classpath ; ses déclarations
+ n'ont donc besoin d'aucun fichier séparé. Elles sont dans les fichiers de classe, dans les
+ `@Metadata` qu'écrit un compilateur Kotlin. Ces déclarations sont lues à la compilation, et
+ une *façade* enveloppe une valeur vivante de sorte que ses membres soient atteints comme
+ s'ils étaient ceux de Scala. Chaque accès devient un appel JVM direct et typé statiquement,
+ sans réflexion ni objet enveloppe. Avec la bibliothèque standard Kotlin dans le classpath :

```scala fuel
typecheck
##
val pair = make[kotlin.Pair[Text, Text]](t"a", t"b")
val first: Text = pair.first

val regex = make[kotlin.text.Regex](t"[0-9]+")
val matched: Boolean = regex.matches(t"123")
```

- Three methods enter a Kotlin type:
+ Trois méthodes permettent d'entrer dans un type Kotlin :

```tabular
- column  Method
- column  Reaches
- row  `make[T](…)`  a new instance of `T`, through its constructor
- row  `companion[T]`  the members of `T`'s companion object
- row  `singleton[T]`  the members of the object `T`
+ column  Méthode
+ column  Atteint
+ row  `make[T](…)`  une nouvelle instance de `T`, au moyen de son constructeur
+ row  `companion[T]`  les membres de l'objet compagnon de `T`
+ row  `singleton[T]`  les membres de l'objet `T`
```

- Types substitute through, so `pair.first` is a `Text` and not an `Any`, and a `Text` passes
- wherever a `CharSequence` is expected. Kotlin's nullability is honored. A result which may be
- null is an [`Optional`](optional-values.md), absent where Kotlin would return null.
+ Les types se substituent de bout en bout ; `pair.first` est donc un `Text` et non un `Any`,
+ et un `Text` passe partout où une `CharSequence` est attendue. La nullabilité de Kotlin est
+ respectée. Un résultat qui peut être nul est un [`Optional`](optional-values.md), absent là
+ où Kotlin renverrait null.

- Properties read as members, and `var` properties accept assignment, through the getters and
- setters which Kotlin generated. Assigning to a `val` does not compile. A Kotlin parameter of
- function type takes an ordinary Scala lambda. The parameter of the lambda is itself a facade
- over the Kotlin type, so the value inside it navigates too:
+ Les propriétés se lisent comme des membres, et les propriétés `var` acceptent l'affectation,
+ au moyen des getters et setters qu'a générés Kotlin. Affecter une `val` ne compile pas. Un
+ paramètre Kotlin de type fonction prend une lambda Scala ordinaire. Le paramètre de la lambda
+ est lui-même une façade sur le type Kotlin ; la valeur à l'intérieur se parcourt donc aussi :

```scala fuel
typecheck
##
val replaced = regex.replace(t"a1b2", (m: Facade over kotlin.text.MatchResult) => t"<${m.value}>")
```

- Here `replaced` is `t"a<1>b<2>"`. A member which the class does not declare, or an argument of
- the wrong type, is a compile error. Where the name is close to a real one, the error suggests
- it, spelled as Kotlin spells it.
+ Ici, `replaced` vaut `t"a<1>b<2>"`. Un membre que la classe ne déclare pas, ou un argument du
+ mauvais type, est une erreur de compilation. Lorsque le nom est proche d'un nom réel,
+ l'erreur le suggère, orthographié comme l'orthographie Kotlin.

- ### Expressions, not effects
+ ### Des expressions, pas des effets

- Navigation builds an *expression*, a typed tree of references, selections and applications, and
- performs nothing itself. Running the expression is the business of the ecosystem. The browser
- ecosystem renders it to JavaScript for an event handler, and a WebAssembly ecosystem lowers WIT
- calls to imports. The checking is the point. By the time an expression leaves Scala, every step
- of it has been verified against the foreign world's own definitions.
+ Le parcours construit une *expression*, un arbre typé de références, de sélections et
+ d'applications, et n'effectue rien par lui-même. Exécuter l'expression est l'affaire de
+ l'écosystème. L'écosystème du navigateur la rend en JavaScript pour un gestionnaire
+ d'événement, et un écosystème WebAssembly abaisse les appels WIT en imports. La
+ vérification est l'essentiel. Au moment où une expression quitte Scala, chacune de ses
+ étapes a été vérifiée par rapport aux propres définitions du monde étranger.

- ### Java's own types
+ ### Les types propres à Java

- The standard library of the JVM has types for things which these libraries have their own types
- for: paths, URLs, instants, durations. Code which meets both worlds needs to cross between them
- without copying values by hand. Rather than converting, each Java type is made a
- *representation* of the concept. Any operation which asks for its result "as a path" or "as an
- instant" can then produce the Java type directly. Which representation is meant is a choice,
- made by import:
+ La bibliothèque standard de la JVM a des types pour des choses pour lesquelles ces
+ bibliothèques ont leurs propres types : chemins, URL, instants, durées. Le code qui côtoie
+ les deux mondes doit passer de l'un à l'autre sans copier des valeurs à la main. Plutôt que
+ de convertir, on fait de chaque type Java une *représentation* du concept. Toute opération
+ qui demande son résultat « sous forme de chemin » ou « sous forme d'instant » peut alors
+ produire directement le type Java. La représentation voulue est un choix, fait par import :

```tabular
- column  Java type
- column  Stands as
- column  Import
- row  `java.nio.file.Path`  a path  `pathInterfaces.javaNioPath`
- row  `java.io.File`  a path  `pathInterfaces.javaIoFile`
- row  `java.net.URL`  a URL  `urlInterfaces.javaNetUrl`
- row  `java.time.Instant`  an instant  `instantInterfaces.javaTimeInstant`
- row  `java.util.Date`  an instant  `instantInterfaces.javaUtilDate`
- row  `Long`, milliseconds since the epoch  an instant  `instantInterfaces.javaLongInstant`
- row  `Long`, nanoseconds  a duration  `durationInterfaces.javaLongDuration`
+ column  Type Java
+ column  Tient lieu de
+ column  Import
+ row  `java.nio.file.Path`  un chemin  `pathInterfaces.javaNioPath`
+ row  `java.io.File`  un chemin  `pathInterfaces.javaIoFile`
+ row  `java.net.URL`  une URL  `urlInterfaces.javaNetUrl`
+ row  `java.time.Instant`  un instant  `instantInterfaces.javaTimeInstant`
+ row  `java.util.Date`  un instant  `instantInterfaces.javaUtilDate`
+ row  `Long`, millisecondes depuis l'epoch  un instant  `instantInterfaces.javaLongInstant`
+ row  `Long`, nanosecondes  une durée  `durationInterfaces.javaLongDuration`
```

- For example:
+ Par exemple :

```scala
import instantInterfaces.javaTimeInstant
import durationInterfaces.javaLongDuration
```

- With those in scope, a [time](time.md) read from a file or a clock arrives as a
- `java.time.Instant`, and a duration as a `Long` of nanoseconds, ready to hand to a Java API
- which expects them. The rest of the program keeps its own types.
+ Avec ceux-ci dans la portée, une [heure](time.md) lue dans un fichier ou sur une horloge
+ arrive sous forme de `java.time.Instant`, et une durée sous forme de `Long` de nanosecondes,
+ prêtes à être transmises à une API Java qui les attend. Le reste du programme conserve ses
+ propres types.

- ### Related
+ ### Voir aussi

- - [Web automation](web-automation.md) scripts the browser's DOM through foreign expressions.
- - [Classpath](classpath.md) describes the classpath which an interface file is read from.
- - [Compiler](compiler.md) compiles Kotlin sources into the classfiles which a facade reads.
- - [Time](time.md) and [paths](paths.md) describe the types which the Java representations stand
-   for.
- - [Browser DOM](dom.md) and [syntax highlighting](syntax-highlighting.md) build on foreign interface definitions.
+ - [Automatisation web](web-automation.md) scripte le DOM du navigateur au moyen
+   d'expressions étrangères.
+ - [Classpath](classpath.md) décrit le classpath à partir duquel un fichier d'interface est
+   lu.
+ - [Compilateur](compiler.md) compile des sources Kotlin en fichiers de classe que lit une
+   façade.
+ - [Temps](time.md) et [chemins](paths.md) décrivent les types dont tiennent lieu les
+   représentations Java.
+ - [DOM du navigateur](dom.md) et [coloration syntaxique](syntax-highlighting.md) s'appuient
+   sur des définitions d'interfaces étrangères.
