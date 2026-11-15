- ## Testing
+ ## Tests

- ### About
+ ### Présentation

- A test is a value. It names what it verifies, and produces a value, and an assertion states
- what must hold of that value. Around that core sit two capabilities which most frameworks
- lack.
+ Un test est une valeur. Il nomme ce qu'il vérifie et produit une valeur, et une assertion
+ énonce ce qui doit être vrai de cette valeur. Autour de ce noyau se trouvent deux capacités
+ qui manquent à la plupart des frameworks.

- When an assertion of equality fails, the report is not two `toString`s. It is a structural
- *contrast*: a diff of the expected value and the observed one, aligned field by field and
- element by element, with the actual differences highlighted.
+ Lorsqu'une assertion d'égalité échoue, le rapport n'est pas fait de deux `toString`. C'est
+ un *contraste* structurel : une comparaison de la valeur attendue et de la valeur observée,
+ alignées champ par champ et élément par élément, avec les différences réelles mises en
+ évidence.

- And tests can assert that code *does not compile*. So many checks are made at [compiletime]
- that this matters: a block which should be rejected is captured, and its compile errors
- become values to inspect. Testing that something is impossible is what a library built on
- [correctness](../../philosophy/correctness.md) most needs to do.
+ Et les tests peuvent affirmer qu'un code *ne compile pas*. Tant de vérifications sont faites
+ [à la compilation][compilation] que cela compte : un bloc qui devrait être rejeté est
+ capturé, et ses erreurs de compilation deviennent des valeurs à examiner. Tester qu'une
+ chose est impossible est ce dont une bibliothèque fondée sur la
+ [correction](../../philosophy/correctness.md) a le plus besoin.

- ### On testing
+ ### À propos des tests

- The real product of a test framework is its output on failure. "Expected X but found Y",
- printed as two long strings, leaves the reader to find the difference by eye.
+ Le véritable produit d'un framework de test est ce qu'il affiche en cas d'échec. « Expected
+ X but found Y », affiché sous forme de deux longues chaînes, laisse le lecteur trouver la
+ différence à l'œil.

- And in a codebase whose invariants live in the type system, half of what needs to be
- verified is precisely that certain code cannot be written. An ordinary test cannot express
- that at all, since it would have to compile first.
+ Et dans un code dont les invariants vivent dans le système de types, la moitié de ce qu'il
+ faut vérifier est précisément qu'un certain code ne peut pas être écrit. Un test ordinaire ne
+ peut pas du tout l'exprimer, puisqu'il devrait d'abord compiler.

- Both are addressed here. Failures are shown as structured comparisons, and compile errors
- are subjects for tests in their own right. Everything comes from the `soundness` package:
+ Les deux problèmes sont traités ici. Les échecs sont montrés sous forme de comparaisons
+ structurées, et les erreurs de compilation sont des sujets de test à part entière. Tout
+ provient du package `soundness` :

```scala
import soundness.*
import environments.javaBaseEnvironment
import strategies.throwUnsafely
```

- The samples on this page declare tests, which a test runner would run, so they are compiled
- but not run here.
+ Les exemples de cette page déclarent des tests, qu'un lanceur de tests exécuterait : ils
+ sont donc compilés, mais pas exécutés ici.

- ### Suites and tests
+ ### Suites et tests

- A suite implements `run()`. A test wraps the expression under scrutiny, and asserts on its
- value, and `suite` groups related tests:
+ Une suite implémente `run()`. Un test enveloppe l'expression examinée et fait une assertion
+ sur sa valeur, et `suite` regroupe des tests apparentés :

```scala
object Tests extends Suite(m"Parser tests"):
  def run(): Unit =
    suite(m"Numbers"):
      test(m"a decimal integer parses"):
        t"42".as[Int]
      . assert(_ == 42)
```

- A test ends in one of three assertions:
+ Un test se termine par l'une de trois assertions :

```tabular
- column  Assertion
- column  Asserts
- column  Returns
- row  `assert`  a predicate holds of the value  nothing
- row  `check`  a predicate holds of the value  the value, for chaining further work on it
- row  `matches`  the value fits a pattern  nothing
+ column  Assertion
+ column  Affirme
+ column  Renvoie
+ row  `assert`  un prédicat est vrai de la valeur  rien
+ row  `check`  un prédicat est vrai de la valeur  la valeur, pour enchaîner d'autres traitements
+ row  `matches`  la valeur correspond à un motif  rien
```

- A test which *should* pass, but is known not to yet, is written like any other, inside an
- `aspirationally` block. Every assertion within it is recorded as an aspiration. It neither
- fails the build, nor is forgotten, and it is reported distinctly once it starts to pass.
+ Un test qui *devrait* réussir, mais dont on sait qu'il ne réussit pas encore, s'écrit comme
+ n'importe quel autre, dans un bloc `aspirationally`. Chaque assertion qu'il contient est
+ enregistrée comme une aspiration. Elle ne fait pas échouer le build, n'est pas oubliée, et
+ elle est signalée distinctement dès qu'elle commence à réussir.

```scala
aspirationally:
  test(m"a hexadecimal integer parses"):
    t"0x2a".as[Int]
  . assert(_ == 42)
```

- Approximate comparison is built in, for the numeric cases where exact equality is wrong:
+ La comparaison approchée est intégrée, pour les cas numériques où l'égalité exacte est
+ inappropriée :

```scala
import dysasymptotics.linearSize

val results = List(0.01, -0.02, 0.015)

test(m"the mean converges"):
  results.total/results.size
. assert(_ === 0.0 +/- 0.02)
```

- A test may also have a *moniker*: an identifier, checked at compiletime, which names it
- stably. A selection, or a chart, can then address the test even after its description has
- been reworded:
+ Un test peut aussi avoir un *moniker* : un identifiant, vérifié à la compilation, qui le
+ nomme de manière stable. Une sélection, ou un graphique, peut alors désigner le test même
+ après que sa description a été reformulée :

```scala
import soundness.nominative

test(n"square", m"square a number")(3*3).assert(_ == 9)
```

- ### Where a test belongs
+ ### La place d'un test

- A suite has a title and an *id*. The id is what a selection, and a path, know the suite by.
- It is either given, as a string literal before the title, or derived from the title, as its
- letters and digits in lower case, with a `-` for each run of anything else. The first of
- these suites is run with `fume run parser`, and the second with `fume run parser-tests`:
+ Une suite a un titre et un *id*. L'id est ce par quoi une sélection, et un chemin,
+ connaissent la suite. Il est soit donné, sous forme de littéral de chaîne avant le titre,
+ soit dérivé du titre, à partir de ses lettres et chiffres en minuscules, avec un `-` pour
+ chaque suite d'autres caractères. La première de ces suites est exécutée avec
+ `fume run parser`, et la seconde avec `fume run parser-tests` :

```scala fuel
typecheck
##
object Tests extends Suite("parser", m"Parser tests")
object ParserTests extends Suite(m"Parser tests")
```

- Soundness's own suites always give an id, which names what the library is for, and not the
- library itself, as [the testing standard](../../standards/testing.md) sets out.
+ Les suites propres à Soundness donnent toujours un id, qui nomme ce à quoi sert la
+ bibliothèque, et non la bibliothèque elle-même, comme l'expose
+ [le standard de test](../../standards/testing.md).

- An id which is given is also a *type*. Within the body of
- `Suite("parser", m"Parser tests")`, the [contextual value] which `test` and `suite` take is
- a `Testable of "parser"`, and each `suite` block passes the same type on. The compiler
- therefore knows, of every test which it compiles, which suite it is declared for, and it
- refuses a `test` which is declared for none.
+ Un id donné est aussi un *type*. Dans le corps de `Suite("parser", m"Parser tests")`, la
+ [valeur contextuelle] que prennent `test` et `suite` est un `Testable of "parser"`, et
+ chaque bloc `suite` transmet ce même type. Le compilateur sait donc, pour chaque test qu'il
+ compile, pour quelle suite il est déclaré, et il refuse un `test` qui n'est déclaré pour
+ aucune.

- Tests which are written in a method of their own name the suite, by its id, in the method's
- signature, as the second version shows:
+ Les tests écrits dans une méthode à part nomment la suite, par son id, dans la signature de
+ la méthode, comme le montre la seconde version :

```scala fuel
alternative Inline
  code
      object Tests extends Suite(m"Parser tests"):
        def run(): Unit =
          suite(m"Numbers"):
            test(m"a decimal integer parses"):
              t"42".as[Int]
            . assert(_ == 42)
alternative In a method
  code
      object Tests extends Suite("parser", m"Parser tests"):
        def run(): Unit =
          suite(m"Numbers")(numbers())

        def numbers()(using Testable of "parser"): Unit =
          test(m"a decimal integer parses"):
            t"42".as[Int]
          . assert(_ == 42)
##
```

- A suite whose id is derived has no literal to name it by. Its tests are written in its
- body, or in a method which is generic in the suite, with
- `[topic <: Label](using Testable of topic)`.
+ Une suite dont l'id est dérivé n'a pas de littéral par lequel la nommer. Ses tests sont
+ écrits dans son corps, ou dans une méthode générique en la suite, avec
+ `[topic <: Label](using Testable of topic)`.

- A compiler plugin reads this from the typed code, and writes an index of the tests beside
- the index of suites, in `META-INF/probably/tests`: every test's place in the hierarchy, its
- kind, its moniker, its tags and where it is declared. A tool can therefore list the tests on
- a classpath, complete their names, and arrange them into suites, without running anything. A
- listing which is made by running a suite, by contrast, executes every statement around its
- tests.
+ Un plugin de compilateur lit cela dans le code typé, et écrit un index des tests à côté de
+ l'index des suites, dans `META-INF/probably/tests` : la place de chaque test dans la
+ hiérarchie, sa nature, son moniker, ses étiquettes et l'endroit où il est déclaré. Un outil
+ peut donc lister les tests d'un classpath, compléter leurs noms et les organiser en suites,
+ sans rien exécuter. Une liste obtenue en exécutant une suite, au contraire, exécute chaque
+ instruction autour de ses tests.

- Some tests cannot be known in that way: one for each file of a corpus, say, or tests which
- are run on a `Runner` of the program's own, and belong to no suite at all. They are declared
- inside an `impromptu` block. It admits `test` and `suite` as a `Suite` does, at the position
- of the surrounding suite, if there is one, but leaves them out of the index. A tool learns
- of them as they run.
+ Certains tests ne peuvent pas être connus de cette manière : un par fichier d'un corpus,
+ par exemple, ou des tests exécutés sur un `Runner` propre au programme, qui n'appartiennent
+ à aucune suite. Ils sont déclarés dans un bloc `impromptu`. Celui-ci admet `test` et `suite`
+ comme le fait une `Suite`, à la position de la suite englobante s'il y en a une, mais les
+ laisse hors de l'index. Un outil en prend connaissance à mesure qu'ils s'exécutent.

```scala
impromptu:
  List(t"1", t"22", t"333").each: digits =>
    test(m"$digits parses")(digits.as[Int]).assert(_ > 0)
```

- A test with a computed name which is *not* in an `impromptu` block is still indexed, by the
- fixed parts of its name.
+ Un test au nom calculé qui ne se trouve *pas* dans un bloc `impromptu` est tout de même
+ indexé, par les parties fixes de son nom.

- ### Spreading a test over axes
+ ### Déployer un test sur des axes

- A test is a name, and zero or more *axes*. Given an axis, the body runs once for each value,
- and each verdict is recorded at that coordinate. One test therefore covers a family of
- cases, without becoming a loop whose failures all report the same name:
+ Un test est un nom, et zéro ou plusieurs *axes*. Avec un axe, le corps s'exécute une fois
+ pour chaque valeur, et chaque verdict est enregistré à cette coordonnée. Un seul test couvre
+ donc une famille de cas, sans devenir une boucle dont les échecs signalent tous le même nom :

```scala
test(m"double every value").over(Axis(t"n")(1, 2, 3, 4)): n =>
  n*2
. assert((n, result) => result == n*2)
```

- The companion of an [enumeration] is an axis in its own right, and needs no list of values:
+ Le compagnon d'une [énumération] est un axe à part entière, et n'a besoin d'aucune liste de
+ valeurs :

```scala
enum Codec:
  case Binary, Textual, Compressed

test(m"enum companions form axes").over(Codec): codec =>
  codec.ordinal
. assert((codec, ordinal) => ordinal >= 0 && ordinal < 3)
```

- Two axes spread a test over a grid, which is shown as a table with one axis along each
- side. A body which is partial leaves gaps, which appear as empty positions, not as failures.
- That is an honest way to say that a combination does not apply:
+ Deux axes déploient un test sur une grille, présentée sous forme de tableau avec un axe le
+ long de chaque côté. Un corps partiel laisse des trous, qui apparaissent comme des positions
+ vides, et non comme des échecs. C'est une manière honnête de dire qu'une combinaison ne
+ s'applique pas :

```scala
test(m"biaxial spread with a gap").over(Axis(t"x")(1, 2, 3), Axis(t"y")(10, 20)):
  case (x, y) if x + y != 23 => x*y
. assert((x, y, result) => result == x*y)
```

- The assertion sees the values of the axes beside the result, so a predicate can depend on
- where it is in the grid.
+ L'assertion voit les valeurs des axes à côté du résultat : un prédicat peut ainsi dépendre
+ de sa position dans la grille.

- ### Failure as a contrast
+ ### L'échec comme contraste

- When an assertion of equality fails, the two values are *decomposed*, with case classes
- broken into fields, and collections into aligned elements, and are set side by side. The
- report shows the tree, with the differing leaves marked: an age which differs by two, a list
- with one element substituted, a text with the changed characters highlighted. Sequences are
- aligned through the same [diffing](diffing.md) which is used elsewhere, so an insertion
- shows as an insertion, and not as every later element "differing".
+ Lorsqu'une assertion d'égalité échoue, les deux valeurs sont *décomposées*, les case classes
+ en champs et les collections en éléments alignés, puis placées côte à côte. Le rapport montre
+ l'arbre, avec les feuilles divergentes marquées : un âge qui diffère de deux, une liste dont
+ un élément a été remplacé, un texte dont les caractères modifiés sont mis en évidence. Les
+ séquences sont alignées par le même [calcul de différences](diffing.md) qu'ailleurs : une
+ insertion apparaît ainsi comme une insertion, et non comme une « différence » de chaque
+ élément suivant.

- Comparison alone can say only *same* or *different*. A raw diff of two sequences therefore
- reports a changed element as a deletion beside an unrelated insertion, and stops there. The
- report goes further. Within each region of differences, it asks whether an element which was
- removed and an element which was added are *similar*: a board member with the same role but
- a different name, or a record with the same identifier and a changed field. Where they are,
- the two are shown as one substitution, broken down to the field which changed, and not as
- two unrelated entries which the reader must pair up.
+ La comparaison seule ne peut dire que *identique* ou *différent*. Une comparaison brute de
+ deux séquences signale donc un élément modifié comme une suppression à côté d'une insertion
+ sans rapport, et s'arrête là. Le rapport va plus loin. Dans chaque zone de différences, il se
+ demande si un élément supprimé et un élément ajouté sont *similaires* : un membre du conseil
+ ayant le même rôle mais un nom différent, ou un enregistrement ayant le même identifiant mais
+ un champ modifié. Lorsque c'est le cas, les deux sont montrés comme une seule substitution,
+ détaillée jusqu'au champ modifié, et non comme deux entrées sans rapport que le lecteur
+ devrait apparier.

- ### Diagnosing a failure
+ ### Diagnostiquer un échec

- Nothing about how a failure will be shown is decided when a test is compiled. Most
- assertions pass, so working out how to render each one's value, on every compilation, would
- be paid for thousands of times to serve the few which fail. An assertion records only where
- its predicate is written.
+ Rien de la manière dont un échec sera montré n'est décidé à la compilation d'un test. La
+ plupart des assertions réussissent : déterminer comment rendre la valeur de chacune, à chaque
+ compilation, serait payé des milliers de fois pour servir les quelques-unes qui échouent.
+ Une assertion n'enregistre que l'endroit où son prédicat est écrit.

- When a predicate fails, the runner reads it back from the compiled suite, and evaluates it
- once more on the value which failed, recording each intermediate value on the way. The
- test's body is not run again. For a predicate such as `x => x*2 < 9 && x*y > 4`, with `x`
- and `y` both 2, the report is the tree of its subexpressions:
+ Lorsqu'un prédicat échoue, le lanceur le relit dans la suite compilée, et l'évalue une fois
+ de plus sur la valeur qui a échoué, en enregistrant chaque valeur intermédiaire au passage.
+ Le corps du test n'est pas réexécuté. Pour un prédicat tel que `x => x*2 < 9 && x*y > 4`,
+ avec `x` et `y` valant tous deux 2, le rapport est l'arbre de ses sous-expressions :

```
x*2 < 9 && x*y > 4   false
x*2 < 9              true
x*2                  4
x                    2
           x*y > 4   false
           x*y       4
           x         2
             y       2
```

- This is what makes a failing property legible: the counterexample is shown together with
- everything that was computed from it. A predicate which is an equality is also shown as a
- contrast of its two sides.
+ C'est ce qui rend lisible une propriété en échec : le contre-exemple est montré avec tout ce
+ qui en a été calculé. Un prédicat qui est une égalité est également montré comme un
+ contraste entre ses deux membres.

- Each value is rendered by whichever instance is found for its type at that moment, an
- `Inspectable` or a `Showable`. A suite supplies one in the type's companion, by an import
- above the assertion, or as a given of the object which the suite is written in, and need
- not know whether a runner will ask for it. A suite is asked for a diagnosis with the
- selection term `--diagnose=dissect,contrast,inspect`.
+ Chaque valeur est rendue par l'instance trouvée pour son type à ce moment-là, un
+ `Inspectable` ou un `Showable`. Une suite en fournit une dans le compagnon du type, par un
+ import au-dessus de l'assertion, ou comme given de l'objet dans lequel la suite est écrite,
+ et n'a pas besoin de savoir si un lanceur la demandera. On demande un diagnostic à une suite
+ avec le terme de sélection `--diagnose=dissect,contrast,inspect`.

- ### Asserting that code does not compile
+ ### Affirmer qu'un code ne compile pas

- `demilitarize` compiles the code which it encloses and, instead of failing the build,
- delivers the compile errors as values:
+ `demilitarize` compile le code qu'il enveloppe et, au lieu de faire échouer le build, livre
+ les erreurs de compilation sous forme de valeurs :

```scala fuel
typecheck
##
import classloaders.threadContextClassloader

test(m"a malformed version literal is rejected"):
  demilitarize:
    v"1.2"
  . map(_.message)
. assert(!_.nil)
```

- This is how Soundness's own guarantees are tested: that adding amounts in the wrong
- [currency](money.md#arithmetic), an invalid literal, or an undeclared relation, really does
- fail to compile. Any project which builds invariants into types needs the same. The
- *impossibility* is the feature, and this makes it testable.
+ C'est ainsi que sont testées les garanties propres à Soundness : qu'additionner des montants
+ dans la mauvaise [devise](money.md#arithmetic), un littéral invalide ou une relation non
+ déclarée échoue réellement à la compilation. Tout projet qui inscrit des invariants dans les
+ types en a besoin. L'*impossibilité* est la fonctionnalité, et ceci la rend testable.

- It works through a compiler plugin, and knowing roughly how is useful when it misbehaves.
- The plugin runs after parsing, but before typechecking. When it finds a `demilitarize` block
- in the untyped code, it starts a *separate* compilation of the same file, on the same
- classpath, but without the plugin, and expects it to fail. The errors which fall inside the
- block are captured. In the main compilation, they are replaced by code which constructs the
- corresponding `CompileError` values. Typechecking then sees only those constructions, and
- never the erroneous code, so the build succeeds, so long as nothing outside a `demilitarize`
- block is wrong.
+ Cela fonctionne au moyen d'un plugin de compilateur, et savoir à peu près comment est utile
+ lorsqu'il se comporte mal. Le plugin s'exécute après l'analyse syntaxique, mais avant la
+ vérification des types. Lorsqu'il trouve un bloc `demilitarize` dans le code non typé, il
+ lance une compilation *séparée* du même fichier, sur le même classpath, mais sans le plugin,
+ et s'attend à ce qu'elle échoue. Les erreurs qui tombent à l'intérieur du bloc sont
+ capturées. Dans la compilation principale, elles sont remplacées par du code qui construit
+ les valeurs `CompileError` correspondantes. La vérification des types ne voit alors que ces
+ constructions, et jamais le code erroné : le build réussit donc, tant que rien n'est faux en
+ dehors d'un bloc `demilitarize`.

- One consequence is worth knowing. An error in an early phase of the compiler can stop later
- phases from running, so errors which would have come from those phases go uncaptured on the
- first pass. The plugin runs the compilation again as many times as are needed, removing more
- of the offending code each time, until the later phases are reached. That is why a file with
- several `demilitarize` blocks compiles more slowly than its size suggests.
+ Une conséquence mérite d'être connue. Une erreur dans une phase précoce du compilateur peut
+ empêcher l'exécution des phases suivantes : les erreurs qui seraient venues de ces phases ne
+ sont donc pas capturées lors de la première passe. Le plugin relance la compilation autant de
+ fois que nécessaire, en retirant chaque fois davantage du code fautif, jusqu'à atteindre les
+ phases suivantes. C'est pourquoi un fichier comportant plusieurs blocs `demilitarize` compile
+ plus lentement que sa taille ne le laisse supposer.

- ### Measuring, not just asserting
+ ### Mesurer, pas seulement affirmer

- A test need not end in a verdict. Three further kinds record measurements:
+ Un test n'a pas besoin de se terminer par un verdict. Trois autres sortes enregistrent des
+ mesures :

```tabular
- column  Kind
- column  Records
- row  `test`  a verdict
- row  `Bench`  speed
- row  `Stress`  allocation per operation, peak heap, and the live set under sustained concurrency
- row  `Profile`  the hottest methods by self time, from JFR execution samples
+ column  Sorte
+ column  Enregistre
+ row  `test`  un verdict
+ row  `Bench`  la vitesse
+ row  `Stress`  allocation par opération, pic de tas, ensemble vivant sous concurrence soutenue
+ row  `Profile`  les méthodes les plus chaudes en temps propre, d'après les échantillons JFR
```

- All four land in the same report: a name, its axes, and a sparse map of cells, whose figures
- come from one closed set. Every kind is therefore displayed in the same way, and charted in
- the same way.
+ Les quatre aboutissent dans le même rapport : un nom, ses axes, et une map creuse de
+ cellules, dont les chiffres proviennent d'un même ensemble fermé. Chaque sorte est donc
+ affichée de la même manière, et représentée graphiquement de la même manière.

- Every measurement is compiled and run on a `BenchmarkDevice`, which is the local machine
- here. It needs a system, a temporary directory to work in, and an embedding for the values
- which it carries, all in scope. A `Decimalizer` formats its sizes and rates:
+ Chaque mesure est compilée et exécutée sur un `BenchmarkDevice`, qui est ici la machine
+ locale. Il a besoin d'un système, d'un répertoire temporaire où travailler, et d'un
+ encodage pour les valeurs qu'il transporte, tous dans la portée. Un `Decimalizer` formate
+ ses tailles et ses débits :

```scala
import embeddings.automaticEmbedding
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory

given device: BenchmarkDevice = LocalhostDevice
given decimalizer: Decimalizer = Decimalizer(2)
```

- A `Bench` measures speed. Its body is a quoted expression, which is [staged](staging.md),
- and dispatched in a JVM of its own, so that the measurement is not distorted by the rest of
- the suite:
+ Un `Bench` mesure la vitesse. Son corps est une expression quotée, qui est
+ [mise en étapes](staging.md) et répartie dans une JVM à part, afin que la mesure ne soit pas
+ faussée par le reste de la suite :

```scala fuel
typecheck
##
val bench = Bench()

bench(m"decode directly")(target = 1*Second, operationSize = 1000*Byte):
  '{ Benchmarks.decodeUsersDirect() }
```

- A `Stress` measures what a figure for throughput hides. It runs for a target of elapsed
- time, at a stated concurrency, and may be given a constrained heap, or a limit on CPU, so a
- design can be shown to hold up where it matters:
+ Un `Stress` mesure ce que masque un chiffre de débit. Il s'exécute pendant une durée cible,
+ à une concurrence donnée, et peut recevoir un tas contraint ou une limite de CPU, de sorte
+ qu'on peut montrer qu'une conception tient là où cela compte :

```scala fuel
typecheck
##
import threads.virtualThreads

val stress = Stress()
val constrained = Stress(heap = t"128m")

stress(m"cross-thread hand-off")(target = 2*Second, concurrency = 16):
  '{ Benchmarks.handOff() }
```

- A `Profile` answers where the time went. It gives a histogram of the hottest methods,
- colored by package:
+ Un `Profile` répond à la question de savoir où le temps est passé. Il donne un histogramme
+ des méthodes les plus chaudes, coloré par package :

```scala fuel
typecheck
##
val profile = Profile()

profile(m"pipeline hotspots")(target = 5*Second):
  '{ Benchmarks.pipeline() }
```

- Benchmarks spread over axes as tests do. One program is staged for each distinct
- implementation, and each cell is dispatched in a JVM of its own. A baseline anchors one
- value of an axis, and the rest are shown relative to it. A stress *sweep* records its steps
- as coordinates on an axis of a single test, not as a family of tests with different names.
- [Charts](charts.md) draws the results.
+ Les benchmarks se déploient sur des axes comme les tests. Un programme est mis en étapes
+ pour chaque implémentation distincte, et chaque cellule est répartie dans une JVM à part.
+ Une référence ancre une valeur d'un axe, et les autres sont montrées par rapport à elle. Un
+ *balayage* de stress enregistre ses étapes comme coordonnées sur un axe d'un seul test, et
+ non comme une famille de tests aux noms différents. [Les graphiques](charts.md) dessinent
+ les résultats.

- ### Running and selecting
+ ### Exécuter et sélectionner

- A `Suite` is not a main class. A compiler plugin records every `Suite` object in the index
- `META-INF/services/probably.Suite` of the jar which it compiles. The runner,
- [fume](https://github.com/propensive/fume), discovers suites from that index on a built
- classpath, runs each one in its own process, and displays the stream of `TestEvent`s which a
- suite emits: live progress on a terminal, plain output in CI, each failure with its
- contrast, and a nonzero exit status if any test failed. That is all that a build needs to
- gate on.
+ Une `Suite` n'est pas une classe principale. Un plugin de compilateur enregistre chaque
+ objet `Suite` dans l'index `META-INF/services/probably.Suite` du jar qu'il compile. Le
+ lanceur, [fume](https://github.com/propensive/fume), découvre les suites à partir de cet
+ index sur un classpath construit, exécute chacune dans son propre processus, et affiche le
+ flux de `TestEvent` qu'émet une suite : la progression en direct sur un terminal, une
+ sortie simple en CI, chaque échec avec son contraste, et un code de sortie non nul si un
+ test a échoué. C'est tout ce dont un build a besoin pour décider.

- A project names its classpath once, in `.pyrocosm/fume/config.tel`, so a bare `fume run`
- runs everything. `fume list`, and the completion of test names in a shell, read the index of
- tests, and so run nothing.
+ Un projet nomme son classpath une seule fois, dans `.pyrocosm/fume/config.tel` : un simple
+ `fume run` exécute donc tout. `fume list`, ainsi que la complétion des noms de tests dans un
+ shell, lisent l'index des tests, et n'exécutent donc rien.

- fume also accepts terms of selection, so a subset can be run without editing the code:
+ fume accepte aussi des termes de sélection, de sorte qu'un sous-ensemble peut être exécuté
+ sans modifier le code :

```tabular
- column  Term
- column  Selects
- column  Combines by
- row  a hash  the test with that hash  union
- row  a moniker, such as `square`  the test with that moniker  union
- row  a name glob, such as `'parser*'`  the tests whose names match  union
- row  `--test`, `--bench`, `--stress`, `--profile`  the tests of that kind  intersection
- row  an axis constraint, such as `parser=jacinta` or `'N<32'`  the cells matching it  intersection
+ column  Terme
+ column  Sélectionne
+ column  Se combine par
+ row  un hachage  le test ayant ce hachage  union
+ row  un moniker, tel que `square`  le test ayant ce moniker  union
+ row  un motif de nom, tel que `'parser*'`  les tests dont le nom correspond  union
+ row  `--test`, `--bench`, `--stress`, `--profile`  les tests de cette sorte  intersection
+ row  contrainte d'axe, comme `parser=jacinta` ou `'N<32'`  les cellules conformes  intersection
```

- Terms which identify tests are united with each other, and the rest are intersected with
- that union:
+ Les termes qui identifient des tests sont réunis entre eux, et les autres sont intersectés
+ avec cette union :

```sh
fume run square 'parser*'
fume run --bench
fume run parser=jacinta 'N<32'
fume list
```

- The first selects by moniker and then by a glob on names, the second runs only the
- benchmarks, the third runs only the cells which match the constraints on the axes, and the
- last lists what matches, and runs nothing. Assertions which are not selected never execute,
- and benchmark cells which are not selected are skipped, so a selection costs only what it
- runs.
+ Le premier sélectionne par moniker puis par un motif sur les noms, le deuxième n'exécute que
+ les benchmarks, le troisième n'exécute que les cellules qui satisfont les contraintes sur
+ les axes, et le dernier liste ce qui correspond, sans rien exécuter. Les assertions non
+ sélectionnées ne s'exécutent jamais, et les cellules de benchmark non sélectionnées sont
+ sautées : une sélection ne coûte donc que ce qu'elle exécute.

- A suite is not a main class, but an entry point for plain `java`, for CI, or for a host
- which is too old to read the jar's schema of events, is one line.
- `Suite.main(arguments, Tests)` runs the suites named, prints a line for each completed test,
- its details and a summary, and exits with their status:
+ Une suite n'est pas une classe principale, mais un point d'entrée pour un simple `java`,
+ pour la CI, ou pour un hôte trop ancien pour lire le schéma d'événements du jar, tient en
+ une ligne. `Suite.main(arguments, Tests)` exécute les suites nommées, affiche une ligne pour
+ chaque test terminé, ses détails et un résumé, et sort avec leur code :

```tabular
- column  Status
- column  Means
- row  `0`  every test passed
- row  `1`  a test failed
- row  `2`  a suite threw
+ column  Code
+ column  Signification
+ row  `0`  tous les tests ont réussi
+ row  `1`  un test a échoué
+ row  `2`  une suite a levé une exception
```

- `Suite.run` is the same, without exiting, for a host of its own.
+ `Suite.run` fait de même, sans sortir, pour un hôte qui a le sien propre.

- <!-- doccheck: skip -->
+ <!-- doccheck: skip -->
```scala
object Tests extends Suite("parser", m"Parser tests"):
  def main(arguments: scala.Array[String]): Unit = Suite.main(arguments, Tests)
  def run(): Unit = ()
```

- ### Related
+ ### Voir aussi

- - [Errors](errors.md#inspecting-a-failure-directly) describes `capture`, which a test uses to
-   assert that code fails in the right way.
- - [Diffing](diffing.md) computes the differences which a contrast shows.
- - [Randomness](randomness.md#reproducing-a-run) generates reproducible test data.
- - [Terminal emulation](terminal-emulator.md) tests what a terminal program displays.
- - [Environment](environment.md#overriding-for-a-block) gives a test a fixed environment.
- - [Browser DOM](dom.md), [web automation](web-automation.md), [time](time.md) and [stack traces](stack-traces.md) describe what tests drive, control and report.
+ - [Erreurs](errors.md#inspecting-a-failure-directly) décrit `capture`, qu'un test utilise
+   pour affirmer qu'un code échoue de la bonne manière.
+ - [Calcul de différences](diffing.md) calcule les différences que montre un contraste.
+ - [Aléatoire](randomness.md#reproducing-a-run) génère des données de test reproductibles.
+ - [Émulation de terminal](terminal-emulator.md) teste ce qu'affiche un programme de
+   terminal.
+ - [Environnement](environment.md#overriding-for-a-block) donne à un test un environnement
+   fixe.
+ - [DOM du navigateur](dom.md), [automatisation web](web-automation.md), [temps](time.md) et
+   [traces de pile](stack-traces.md) décrivent ce que les tests pilotent, contrôlent et
+   signalent.
