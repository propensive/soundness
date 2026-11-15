- ## Randomness
+ ## Aléatoire

- ### About
+ ### Présentation

- Random values of any type, not only numbers, come from the `Randomizable` [typeclass]. An
- instance is [derived][derivation] for a case class or an enumeration from its structure, so a
- whole tree of test data is one call.
+ Les valeurs aléatoires de n'importe quel type, pas seulement des nombres, proviennent de la
+ [classe de types] `Randomizable`. Une instance est [dérivée][dérivation] pour une case class ou
+ une énumération à partir de sa structure : tout un arbre de données de test tient donc en un
+ appel.

- The *source* of the randomness and the *shape* of the numbers are separate choices, and both
- are explicit. The source is a [given] in scope, and is either seeded and reproducible, or
- cryptographically secure. The distribution which a `Double` is drawn from is another given:
- uniform, Gaussian or gamma.
+ La *source* de l'aléatoire et la *forme* des nombres sont des choix distincts, et tous deux
+ sont explicites. La source est un [given] dans la portée, et elle est soit initialisée par une
+ graine et reproductible, soit sûre au sens cryptographique. La distribution selon laquelle un
+ `Double` est tiré est un autre given : uniforme, gaussienne ou gamma.

- ### On randomness
+ ### L'aléatoire

- Randomness hides assumptions:
+ L'aléatoire cache des hypothèses :

- - `math.random()` answers only one narrow question, a uniform `Double`, and everything else is
-   assembled by hand.
- - The seed is global and implicit, so a failing test which depended on chance cannot be
-   replayed.
- - Whether the generator is predictable is nowhere visible in the code. That is harmless in a
-   simulation, and fatal in a generator of tokens.
+ - `math.random()` ne répond qu'à une question étroite, un `Double` uniforme, et tout le reste
+   s'assemble à la main.
+ - La graine est globale et implicite : un test en échec qui dépendait du hasard ne peut donc
+   pas être rejoué.
+ - Que le générateur soit prévisible ou non n'est visible nulle part dans le code. C'est
+   inoffensif dans une simulation, et fatal dans un générateur de jetons.

- Here the concerns are separate, and each has a name:
+ Ici, les préoccupations sont séparées, et chacune a un nom :

```tabular
- column  Concern
- column  Stated by
- row  where randomness is used  a `stochastic` block
- row  which generator supplies it  an import from `randomization`
- row  what distribution shapes a number  a `Distribution` given
- row  how a value of some type is generated  a `Randomizable` instance, usually derived
+ column  Préoccupation
+ column  Énoncée par
+ row  où l'aléatoire est utilisé  un bloc `stochastic`
+ row  quel générateur le fournit  un import depuis `randomization`
+ row  quelle distribution donne sa forme à un nombre  un given `Distribution`
+ row  comment se génère une valeur d'un certain type  une instance de `Randomizable`, en général dérivée
```

- A source of randomness which is a [capability], not a global, is
- [declarative context](../../philosophy/declarative-context.md): reproducibility follows from the
- scope, not from discipline.
+ Une source d'aléatoire qui est une [capacité], et non une globale, relève du
+ [contexte déclaratif](../../philosophy/declarative-context.md) : la reproductibilité découle de
+ la portée, et non de la discipline.

- Everything comes from the `soundness` package. A seeded source makes every run produce the same
- values, and the last import lets the samples print their results:
+ Tout provient du package `soundness`. Une source initialisée par une graine fait produire les
+ mêmes valeurs à chaque exécution, et le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import randomization.seededRandomization
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given seed: Seed = Seed(1L)
```

- ### Generating values
+ ### Générer des valeurs

- `stochastic` opens a scope which has a source of randomness. Within it, `arbitrary` produces a
- value of any type with a `Randomizable` instance. The primitive types have instances, and a
- case class or an enumeration derives one from its members:
+ `stochastic` ouvre une portée qui dispose d'une source d'aléatoire. À l'intérieur, `arbitrary`
+ produit une valeur de n'importe quel type doté d'une instance de `Randomizable`. Les types
+ primitifs ont des instances, et une case class ou une énumération en dérive une de ses membres :

```scala
case class Point(x: Int, y: Int)
```

```scala fuel
run
##
stochastic:
  Out.println(arbitrary[Int]().show)
  Out.println(arbitrary[Point]().inspect)
```

- The source is the seeded one, so running the code again with the same seed produces the same
- values. That is what replaying a failing property test requires.
+ La source est celle à graine : réexécuter le code avec la même graine produit donc les mêmes
+ valeurs. C'est ce qu'exige le rejeu d'un test de propriété en échec.

- Outside a `stochastic` block there is no source, and no random value can be asked for:
+ Hors d'un bloc `stochastic`, il n'y a pas de source, et on ne peut demander aucune valeur
+ aléatoire :

```scala fuel
compile
errors show
##
arbitrary[Int]()
```

- ### Distributions
+ ### Distributions

- A random `Double` needs a shape, which a `Distribution` in scope supplies. Until the code says
- which kind of `Double` it means, one cannot be generated, which prevents the classic mistake of
- treating a uniform draw as though it were normal.
+ Un `Double` aléatoire a besoin d'une forme, que fournit une `Distribution` dans la portée. Tant
+ que le code ne dit pas de quel type de `Double` il s'agit, on ne peut pas en générer, ce qui
+ évite l'erreur classique de traiter un tirage uniforme comme s'il était normal.

```tabular
- column  Distribution
- column  Parameters
- column  Draws from
- row  `Gaussian`  `mean`, defaulting to `0.0`, and `std`, to `1.0`  a [normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
- row  `UniformDistribution`  `start` and `end`  a flat range
- row  `Gamma`  `shape` and `scale`  a [gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
+ column  Distribution
+ column  Paramètres
+ column  Tire selon
+ row  `Gaussian`  `mean`, par défaut `0.0`, et `std`, par défaut `1.0`  une [loi normale](https://en.wikipedia.org/wiki/Normal_distribution)
+ row  `UniformDistribution`  `start` et `end`  un intervalle uniforme
+ row  `Gamma`  `shape` et `scale`  une [loi gamma](https://en.wikipedia.org/wiki/Gamma_distribution)
```

- Common choices are also givens, to import from the package `randomDistributions`:
+ Les choix courants sont aussi des givens, à importer depuis le package `randomDistributions` :

```tabular
- column  Given
- column  Draws from
- row  `gaussianDistribution`  the standard normal distribution
- row  `uniformUnitIntervalDistribution`  a flat range from 0 to 1
- row  `uniformSymmetricUnitIntervalDistribution`  a flat range from -1 to 1
- row  `binaryDistribution`  a random `Long`, converted to a `Double`
+ column  Given
+ column  Tire selon
+ row  `gaussianDistribution`  la loi normale centrée réduite
+ row  `uniformUnitIntervalDistribution`  un intervalle uniforme de 0 à 1
+ row  `uniformSymmetricUnitIntervalDistribution`  un intervalle uniforme de -1 à 1
+ row  `binaryDistribution`  un `Long` aléatoire, converti en `Double`
```

- Ten thousand draws from a Gaussian with a standard deviation of 2 have a mean close to 0:
+ Dix mille tirages d'une gaussienne d'écart-type 2 ont une moyenne proche de 0 :

```scala
val draws = stochastic:
  given distribution: Distribution = Gaussian(0.0, 2.0)
  List.fill(10000)(arbitrary[Double]())
```

```scala fuel
run
##
Out.println((draws.fold(0.0)(_ + _)/10000).inspect)
```

- [Mathematics](mathematics.md) has the statistics to describe such a sample.
+ [Mathématiques](mathematics.md) dispose des statistiques pour décrire un tel échantillon.

- ### Sources
+ ### Sources

- The generator behind a scope is chosen by an import from the package `randomization`. The
- secure generators draw from the JDK's
- [cryptographically secure](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
- source, for values which an adversary must not predict:
+ Le générateur derrière une portée se choisit par un import depuis le package `randomization`. Les
+ générateurs sûrs puisent dans la source [sûre au sens
+ cryptographique](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
+ du JDK, pour les valeurs qu'un adversaire ne doit pas pouvoir prédire :

```tabular
- column  Import
- column  Generator
- column  Reproducible
- row  `seededRandomization`  ordinary, from the `Seed` in scope  yes
- row  `unseededRandomization`  ordinary  no, it varies run to run
- row  `secureUnseededRandomization`  secure  no
- row  `stronglySecureRandomization`  the platform's strongest secure source  no
- row
-   cell  `secureSeededRandomization`
-   cell  secure, seeded from the `Seed` in scope
-   cell
+ column  Import
+ column  Générateur
+ column  Reproductible
+ row  `seededRandomization`  ordinaire, à partir de la `Seed` dans la portée  oui
+ row  `unseededRandomization`  ordinaire  non, il varie d'une exécution à l'autre
+ row  `secureUnseededRandomization`  sûr  non
+ row  `stronglySecureRandomization`  la source sûre la plus forte de la plateforme  non
+ row
+   cell  `secureSeededRandomization`
+   cell  sûr, initialisé à partir de la `Seed` dans la portée
+   cell
```

- Simulation and the generation of secrets therefore cannot be confused: the difference is
- written at the import. [Cryptography](cryptography.md) generates keys from the secure sources.
+ La simulation et la génération de secrets ne peuvent donc pas être confondues : la différence
+ est écrite dans l'import. La [cryptographie](cryptography.md) génère des clés à partir des
+ sources sûres.

- ### Collections and custom types
+ ### Collections et types personnalisés

- A random collection needs a size, which is chosen by a `Random.Size` given.
- `randomSizes.uniformSizeUpto100` bounds it at a hundred elements.
+ Une collection aléatoire a besoin d'une taille, choisie par un given `Random.Size`.
+ `randomSizes.uniformSizeUpto100` la limite à cent éléments.

- A type whose values need more care than generating each field defines an instance of its own,
- or maps an existing one:
+ Un type dont les valeurs demandent plus de soin que la génération de chaque champ définit sa
+ propre instance, ou transforme une instance existante :

```scala
import randomDistributions.uniformUnitIntervalDistribution

case class Latitude(degrees: Double)

given latitudeRandomizable: (Latitude is Randomizable) =
  summon[Double is Randomizable].map(d => Latitude(d*180 - 90))
```

```scala fuel
run
##
stochastic:
  Out.println(arbitrary[Latitude]().inspect)
```

- Where generating each field *is* right, the instance is derived from the type. A case class of
- randomizable fields is randomizable, and a sealed hierarchy picks among its variants. That is
- what property-based [testing](testing.md) needs, to generate values of an arbitrary type.
+ Là où générer chaque champ *est* la bonne approche, l'instance se dérive du type. Une case
+ class de champs aléatoires est elle-même aléatoire, et une hiérarchie scellée choisit parmi ses
+ variantes. C'est ce dont ont besoin les [tests](testing.md) fondés sur les propriétés, pour
+ générer des valeurs d'un type arbitraire.

- ### Reproducing a run
+ ### Reproduire une exécution

- A failing randomized test is worth nothing if it cannot be run again. Under
- `seededRandomization`, the whole scope is a function of its `Seed`. Recording the seed of a
- failing run, and supplying it again, reproduces exactly the same values. That includes the
- sizes of generated collections, which are drawn from the same generator:
+ Un test aléatoire en échec ne vaut rien s'il ne peut pas être réexécuté. Sous
+ `seededRandomization`, toute la portée est une fonction de sa `Seed`. Enregistrer la graine
+ d'une exécution en échec, et la fournir de nouveau, reproduit exactement les mêmes valeurs.
+ Cela inclut les tailles des collections générées, qui sont tirées du même générateur :

```scala
import randomSizes.uniformSizeUpto100

def sample(): List[Int] =
  given seed: Seed = Seed(42L)

  stochastic:
    arbitrary[List[Int]]().excerpt(0, 3)
```

```scala fuel
run
##
Out.println(sample().inspect)
Out.println(sample().inspect)
```

- Nesting is deterministic too. A scope's generator is derived from that of the scope enclosing
- it, so restoring the outer seed restores the inner sequence.
+ L'imbrication est déterministe elle aussi. Le générateur d'une portée est dérivé de celui de
+ la portée qui l'englobe : restaurer la graine extérieure restaure donc la séquence intérieure.

- What is determined is the *sequence of draws*, and that is only as reproducible as the code
- which makes them. Single-threaded code draws in a fixed order, and reproduces exactly.
- [Concurrent](concurrency.md) code which draws from one scope does not. Its threads interleave
- differently on each run, so the same seed hands different values to different threads. Where a
- concurrent computation must be reproducible, each strand needs a scope of its own, seeded
- deterministically.
+ Ce qui est déterminé, c'est la *séquence des tirages*, et elle n'est reproductible que dans la
+ mesure où le code qui les effectue l'est. Un code monothread tire dans un ordre fixe, et se
+ reproduit exactement. Un code [concurrent](concurrency.md) qui tire depuis une seule portée ne
+ le fait pas. Ses threads s'entrelacent différemment à chaque exécution : la même graine donne
+ donc des valeurs différentes à des threads différents. Lorsqu'un calcul concurrent doit être
+ reproductible, chaque fil a besoin de sa propre portée, initialisée de façon déterministe.

- ### Shuffling and coin-tosses
+ ### Mélange et pile ou face

- A `Random` in scope also shuffles a sequence, and answers a fair coin-toss. These are small
- conveniences which otherwise get written by hand, badly:
+ Un `Random` dans la portée mélange aussi une séquence, et répond à un pile ou face équitable.
+ Ce sont de petites commodités qu'on écrit sinon à la main, et mal :

```scala fuel
run
##
stochastic: (random: Random) ?=>
  Out.println(random.shuffle(List(1, 2, 3, 4, 5)).inspect)
  Out.println(toss().inspect)
```

- ### Related
+ ### Voir aussi

- - [UUIDs](uuid.md) and [label placement](label-placement.md) draw from a seeded source of randomness.
+ - Les [UUID](uuid.md) et le [placement d'étiquettes](label-placement.md) puisent dans une
+   source d'aléatoire à graine.
