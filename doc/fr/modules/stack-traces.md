- ## Stack Traces
+ ## Traces d'appels

- ### About
+ ### Présentation

- A JVM stack trace describes compiled classes, not the Scala which produced them. A lambda
- appears as `$anonfun`, an extension method gains `$extension`, and an operator is spelled
- `$plus`.
+ Une trace d'appels de la JVM décrit des classes compilées, et non le Scala qui les a
+ produites. Une lambda apparaît sous la forme `$anonfun`, une méthode d'extension se voit
+ ajouter `$extension`, et un opérateur s'écrit `$plus`.

- A `StackTrace` captures a trace as an [immutable] value, with its frames, positions and chain
- of causes. It then *demangles* the trace, so the names a developer reads are closer to the names
- they wrote.
+ Une `StackTrace` capture une trace sous forme de valeur [immuable], avec ses trames, ses
+ positions et sa chaîne de causes. Elle *démêle* ensuite la trace, si bien que les noms que lit
+ un développeur sont plus proches des noms qu'il a écrits.

- Two small related tools sit beside it. A `Codepoint` is the source position of a call site,
- captured automatically. An `Fqcn` is a validated fully-qualified class name.
+ Deux petits outils apparentés se trouvent à côté. Un `Codepoint` est la position dans le code
+ source d'un site d'appel, capturée automatiquement. Un `Fqcn` est un nom de classe pleinement
+ qualifié validé.

- ### On stack traces
+ ### À propos des traces d'appels

- The stack trace of a `Throwable` is mutable, printable state. The standard idiom writes it to a
- stream. Inspecting it in a program means picking through `StackTraceElement`s, whose names are
- the compiler's and not the programmer's.
+ La trace d'appels d'un `Throwable` est un état mutable et affichable. L'idiome standard
+ l'écrit dans un flux. L'inspecter dans un programme revient à fouiller parmi des
+ `StackTraceElement`, dont les noms sont ceux du compilateur et non ceux du programmeur.

- [Errors](errors.md) here are values. Their traces should be values too: comparable,
- transformable, and rendered where and how the program chooses. A trace which can be trimmed,
- resolved and rendered as a value follows [immutability](../../philosophy/immutability.md) into
- diagnostics.
+ Les [erreurs](errors.md) sont ici des valeurs. Leurs traces devraient en être aussi :
+ comparables, transformables, et rendues où et comme le programme le choisit. Une trace qui
+ peut être élaguée, résolue et rendue en tant que valeur prolonge
+ l'[immuabilité](../../philosophy/immutability.md) jusque dans les diagnostics.

- Everything comes from the `soundness` package, and the second import lets the samples print
- their results:
+ Tout provient du package `soundness`, et le second import permet aux exemples d'afficher
+ leurs résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Capturing a trace
+ ### Capturer une trace

- Any `Throwable` converts to a `StackTrace`. The value holds the class of the exception, its
- message, its frames, and its cause as another `StackTrace`:
+ Tout `Throwable` se convertit en `StackTrace`. La valeur contient la classe de l'exception, son
+ message, ses trames, et sa cause sous forme d'une autre `StackTrace` :

```scala
val exception = Exception("boom", Exception("the cause"))
val trace = exception.stackTrace
```

```scala fuel
run
##
Out.println(trace.className)
Out.println(trace.message.text)
Out.println(trace.cause.let(_.message.text).inspect)
```

- Each frame names its class and method, already demangled. The frames of a trace captured here
- belong to the machinery which runs these samples, so they are not shown. `crop` and `drop` trim
- frames, such as the machinery below the entry point of a test framework, so a rendered trace
- shows only the frames which matter.
+ Chaque trame nomme sa classe et sa méthode, déjà démêlées. Les trames d'une trace capturée ici
+ appartiennent à la mécanique qui exécute ces exemples, elles ne sont donc pas montrées. `crop`
+ et `drop` élaguent des trames, comme la mécanique située sous le point d'entrée d'un framework
+ de test, si bien qu'une trace rendue ne montre que les trames qui comptent.

- ### Demangling
+ ### Démêlage

- The names of frames are rewritten from the JVM's encoding toward Scala's. `StackTrace.rewrite`
- is the function which does it, and it can be tried on any compiled name. An operator becomes its
- symbol:
+ Les noms des trames sont réécrits de l'encodage de la JVM vers celui de Scala.
+ `StackTrace.rewrite` est la fonction qui s'en charge, et on peut l'essayer sur n'importe quel
+ nom compilé. Un opérateur devient son symbole :

```scala fuel
run
##
Out.println(StackTrace.rewrite("$plus$plus"))
```

- The compiler's synthetic markers become compact glyphs. A number after a `$` becomes a
- subscript, and the `$$` which joins a private member to the class it was expanded into becomes
- one separator:
+ Les marqueurs synthétiques du compilateur deviennent des glyphes compacts. Un nombre après un
+ `$` devient un indice, et le `$$` qui relie un membre privé à la classe dans laquelle il a été
+ expansé devient un unique séparateur :

```scala fuel
run
##
Out.println(StackTrace.rewrite("com.example.Main$"))
Out.println(StackTrace.rewrite("$anonfun$3", method = true))
Out.println(StackTrace.rewrite("greeting$extension", method = true))
Out.println(StackTrace.rewrite("com.example.Main$$anon$1"))
```

- A `$uXXXX` escape becomes the character it encodes. These are the glyphs:
+ Un échappement `$uXXXX` devient le caractère qu'il encode. Voici les glyphes :

```tabular
- column  Glyph
- column  JVM marker
- column  Meaning
- row  `λ`  `$anonfun`, `$$Lambda`  lambda
- row  `Λ`  `$adapted`  adapted lambda: the bridge a lambda's method is called through
- row  `α`  `$anon`  anonymous class
- row  `ⲛ`  `<init>`  constructor
- row  `ι`  `<clinit>`, `$init$`  initializer, of a class's statics or of a trait
- row  `ℓ`  `$lzyINIT`, `$lzy`, `$lzyHandle`  lazy value: its initializer, field or handle
- row  `↑`  `super$`  super accessor
- row  `⇢`  `protected$`, `inline$`, `$accessor`  accessor: protected, inline or parameter
- row  `≔`  `$_setter_$`  trait setter
- row  `⊢`  `$extension`  extension method
- row  `∂`  `$direct`  direct method, the erased form of a context function
- row  `δ`  `$default`  default argument
- row  `⊡`  `$retainedBody`  retained inline body
- row  `⊙`  `$outer`  outer reference
- row  `⁅⁆`  `$package`  package file
- row  `Ξ`  trailing `$`  object
- row  `Σ`  `$mc…$sp`  specialized method, with its signature
+ column  Glyphe
+ column  Marqueur JVM
+ column  Signification
+ row  `λ`  `$anonfun`, `$$Lambda`  lambda
+ row  `Λ`  `$adapted`  lambda adaptée : le pont par lequel la méthode d'une lambda est appelée
+ row  `α`  `$anon`  classe anonyme
+ row  `ⲛ`  `<init>`  constructeur
+ row  `ι`  `<clinit>`, `$init$`  initialiseur, des membres statiques d'une classe ou d'un trait
+ row  `ℓ`  `$lzyINIT`, `$lzy`, `$lzyHandle`  valeur paresseuse : son initialiseur, son champ ou sa poignée
+ row  `↑`  `super$`  accesseur super
+ row  `⇢`  `protected$`, `inline$`, `$accessor`  accesseur : protégé, inline ou de paramètre
+ row  `≔`  `$_setter_$`  mutateur de trait
+ row  `⊢`  `$extension`  méthode d'extension
+ row  `∂`  `$direct`  méthode directe, la forme effacée d'une fonction contextuelle
+ row  `δ`  `$default`  argument par défaut
+ row  `⊡`  `$retainedBody`  corps inline conservé
+ row  `⊙`  `$outer`  référence externe
+ row  `⁅⁆`  `$package`  fichier de package
+ row  `Ξ`  `$` final  objet
+ row  `Σ`  `$mc…$sp`  méthode spécialisée, avec sa signature
```

- The markers are the ones which the Scala compiler's `NameKinds` defines. They are held in
- `StackTrace.Marker.all`, and `StackTrace.legend` is derived from them, for rendering beside a
- trace:
+ Les marqueurs sont ceux que définit le `NameKinds` du compilateur Scala. Ils sont contenus dans
+ `StackTrace.Marker.all`, et `StackTrace.legend` en est dérivée, pour un rendu à côté d'une
+ trace :

```scala fuel
run
##
Out.println(StackTrace.legend.at(t"λ").inspect)
Out.println(StackTrace.legend.at(t"⊢").inspect)
```

- The result is a trace which reads as the program was written, and not as it was compiled.
+ Le résultat est une trace qui se lit comme le programme a été écrit, et non comme il a été
+ compilé.

- Whether an error captures a trace at all is the `Diagnostics` choice which
- [errors](errors.md) describes. A trace suits debugging, and is omitted where errors are
- expected and handled.
+ Qu'une erreur capture ou non une trace relève du choix `Diagnostics` que décrit la page
+ [erreurs](errors.md). Une trace convient au débogage, et est omise là où les erreurs sont
+ attendues et traitées.

- ### Resolving frames to their source
+ ### Résoudre les trames vers leur code source

- Demangling can only tidy a compiled name. `$anonfun$3` becomes `λ₃`, which says that a lambda
- ran, but not *which* lambda. The names which most need explaining are exactly the ones which the
- compiler mints after its output is pickled: anonymous functions, initializers, bridges,
- specializations. No rewriting recovers them, because the information is not in the name.
+ Le démêlage ne peut que mettre de l'ordre dans un nom compilé. `$anonfun$3` devient `λ₃`, ce
+ qui dit qu'une lambda s'est exécutée, mais pas *laquelle*. Les noms qui ont le plus besoin
+ d'explication sont exactement ceux que le compilateur forge après la sérialisation de sa
+ sortie : fonctions anonymes, initialiseurs, ponts, spécialisations. Aucune réécriture ne les
+ retrouve, car l'information n'est pas dans le nom.

- Position does what the name cannot. The compiler records the extent of every definition, so the
- line which a frame already carries is enough to find the definition it came from. Importing a
- resolver makes every trace captured in its scope carry that information:
+ La position fait ce que le nom ne peut pas faire. Le compilateur enregistre l'étendue de
+ chaque définition, si bien que la ligne que porte déjà une trame suffit à trouver la
+ définition dont elle provient. Importer un résolveur fait porter cette information à chaque
+ trace capturée dans sa portée :

```scala
import stackResolutions.tastyStackResolution
```

- A resolved `Frame` gains a `Source`: the path of the file, the chain of enclosing definitions,
- the definition's own name in the source, and the kind of definition it is. `displayClass` and
- `displayMethod` give the resolved names where they are known, and fall back to the demangled
- names where they are not, so a renderer needs no branch of its own:
+ Une `Frame` résolue gagne une `Source` : le chemin du fichier, la chaîne des définitions
+ englobantes, le nom propre de la définition dans le code source, et le genre de définition
+ dont il s'agit. `displayClass` et `displayMethod` donnent les noms résolus là où ils sont
+ connus, et se rabattent sur les noms démêlés là où ils ne le sont pas, si bien qu'un moteur
+ de rendu n'a besoin d'aucune branche propre :

```scala
trace.frames.head.let: frame =>
  frame.displayClass
  frame.displayMethod
  frame.source.let(_.kind)
```

- The `Kind` of a definition says what it is. `plumbing` says which kinds are pure plumbing which
- the compiler synthesized, so a renderer can dim or drop them:
+ Le `Kind` d'une définition dit ce qu'elle est. `plumbing` dit quels genres relèvent de la
+ pure tuyauterie synthétisée par le compilateur, si bien qu'un moteur de rendu peut les
+ atténuer ou les supprimer :

```tabular
- column  Kind
- column  Plumbing
- row  `Method`  no
- row  `Lambda`  no
- row  `Value`  no
- row  `Class`  no
- row  `Constructor`  no
- row  `Extension`  no
- row  `Default`  no
- row  `Bridge`  yes
- row  `Forwarder`  yes
- row  `Initializer`  yes
- row  `Specialized`  yes
- row  `Synthetic`  yes
+ column  Genre
+ column  Tuyauterie
+ row  `Method`  non
+ row  `Lambda`  non
+ row  `Value`  non
+ row  `Class`  non
+ row  `Constructor`  non
+ row  `Extension`  non
+ row  `Default`  non
+ row  `Bridge`  oui
+ row  `Forwarder`  oui
+ row  `Initializer`  oui
+ row  `Specialized`  oui
+ row  `Synthetic`  oui
```

- Resolution costs one file read for each top-level class named in the trace, so it is chosen
- rather than automatic. A trace already in hand is resolved explicitly, through whichever
- [classloader](classpath.md#choosing-a-classloader) can find the compiled classes:
+ La résolution coûte une lecture de fichier pour chaque classe de premier niveau nommée dans
+ la trace ; elle est donc choisie plutôt qu'automatique. Une trace déjà disponible se résout
+ explicitement, via le [chargeur de classes](classpath.md#choosing-a-classloader), quel qu'il
+ soit, qui peut trouver les classes compilées :

```scala
import classloaders.threadContextClassloader
import workingDirectories.javaBaseWorkingDirectory

val resolved = trace.resolved
```

- The [capability] to read those files is one which this layer deliberately lacks. The default
- resolver does nothing, and an implementation comes from a module which has a classpath. That is
- what keeps stack traces working unchanged where no such capability exists: in a browser, in a
- native binary, or inside a WebAssembly component.
+ La [capacité] de lire ces fichiers est une capacité dont cette couche est délibérément
+ dépourvue. Le résolveur par défaut ne fait rien, et une implémentation provient d'un module qui
+ dispose d'un classpath. C'est ce qui permet aux traces d'appels de fonctionner sans
+ changement là où une telle capacité n'existe pas : dans un navigateur, dans un binaire natif,
+ ou à l'intérieur d'un composant WebAssembly.

- ### Codepoints
+ ### Codepoints

- A `Codepoint` is the source file and line of a call site. It is captured automatically wherever
- a method asks for one as a [given]:
+ Un `Codepoint` est le fichier source et la ligne d'un site d'appel. Il est capturé
+ automatiquement partout où une méthode en demande un comme [given] :

```scala
def note(message: Text)(using codepoint: Codepoint): Text =
  t"${codepoint.text}: $message"
```

- A call such as `note(t"checkpoint")` gives a text such as `app.scala:42: checkpoint`, with the
- position of the call and not of the definition. This is how [logs](logging.md),
- [tests](testing.md) and [caches](caching.md) know where they were called from, and no caller
- passes a location by hand.
+ Un appel tel que `note(t"checkpoint")` donne un texte tel que `app.scala:42: checkpoint`, avec
+ la position de l'appel et non celle de la définition. C'est ainsi que la
+ [journalisation](logging.md), les [tests](testing.md) et les [caches](caching.md) savent
+ d'où ils ont été appelés, sans qu'aucun appelant ne passe d'emplacement à la main.

- ### Class names
+ ### Noms de classes

- An `Fqcn` is a fully-qualified class name which satisfies the JVM's rules: every segment is
- valid, and none is a keyword. The `fqcn"…"` [interpolator] checks it at [compiletime]:
+ Un `Fqcn` est un nom de classe pleinement qualifié qui satisfait les règles de la JVM : chaque
+ segment est valide, et aucun n'est un mot-clé. L'[interpolateur] `fqcn"…"` le vérifie
+ [à la compilation][compilation] :

```scala
val main = fqcn"com.example.Main"
```

- A Java keyword is not a valid segment:
+ Un mot-clé Java n'est pas un segment valide :

```scala fuel
compile
errors show
##
fqcn"com.example.class"
```

- At runtime, a class name is checked by parsing it. Wherever a class name travels, such as the
- `Main-Class` of a manifest or a reflective lookup, an `Fqcn` carries the guarantee that it could
- at least name a class.
+ À l'exécution, un nom de classe se vérifie en l'analysant. Partout où circule un nom de
+ classe, comme la `Main-Class` d'un manifeste ou une recherche par réflexion, un `Fqcn` porte la
+ garantie qu'il pourrait au moins nommer une classe.

- ### Related
+ ### Voir aussi

- - [Errors](errors.md) describes the error values whose traces these are, and the `Diagnostics`
-   choice.
- - [Classpath](classpath.md) describes the classloaders which resolve a frame to its source.
- - [Debugging](debugging.md) renders traces and values for inspection.
- - [Logging](logging.md) and [testing](testing.md) use codepoints to say where a message or a
-   check came from.
- - [Concurrency](concurrency.md) and [compiler](compiler.md) report their failures with traces as values.
+ - [Erreurs](errors.md) décrit les valeurs d'erreur dont ce sont les traces, et le choix
+   `Diagnostics`.
+ - [Classpath](classpath.md) décrit les chargeurs de classes qui résolvent une trame vers son
+   code source.
+ - [Débogage](debugging.md) rend les traces et les valeurs pour l'inspection.
+ - [Journalisation](logging.md) et [tests](testing.md) utilisent les codepoints pour dire d'où
+   provient un message ou une vérification.
+ - [Concurrence](concurrency.md) et [compilation](compiler.md) signalent leurs échecs avec des
+   traces sous forme de valeurs.
