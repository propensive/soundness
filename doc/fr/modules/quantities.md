- ## Quantities
+ ## Quantités

- ### About
+ ### Présentation

- Physical quantities, such as a distance, a mass or a speed, are values which carry their
- [units](https://en.wikipedia.org/wiki/Units_of_measurement) in their type:
+ Les grandeurs physiques, comme une distance, une masse ou une vitesse, sont des valeurs qui
+ portent leurs [unités](https://en.wikipedia.org/wiki/Units_of_measurement) dans leur type :

- - Adding a distance to a time is a compile error.
- - Multiplying a distance by a force gives a quantity whose units are worked out
-   automatically.
- - A quantity costs nothing at [runtime] beyond the `Double` it wraps.
+ - Ajouter une distance à une durée est une erreur de compilation.
+ - Multiplier une distance par une force donne une quantité dont les unités sont calculées
+   automatiquement.
+ - Une quantité ne coûte rien [à l'exécution][exécution] au-delà du `Double` qu'elle enveloppe.

- That last property is a constraint on the design, not an optimization, as
- [zero cost](../../philosophy/zero-cost.md) explains.
+ Cette dernière propriété est une contrainte de conception, et non une optimisation, comme
+ l'explique [coût nul](../../philosophy/zero-cost.md).

- ### On quantities
+ ### À propos des quantités

- A bare number has no units. The figure `5` says nothing about whether it counts meters, feet,
- seconds or kilograms. Code which loses track of the answer has caused real disasters. The
- [loss of a Mars orbiter](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure)
- is one of them: one component worked in newtons, and another in pound-force. The usual
- defenses are naming conventions, comments, and a wrapper class for each unit. They rely on
- discipline, and catch nothing automatically.
+ Un nombre seul n'a pas d'unités. Le chiffre `5` ne dit rien de ce qu'il compte : des mètres,
+ des pieds, des secondes ou des kilogrammes. Du code qui perd le fil de la réponse a causé de
+ vraies catastrophes. La
+ [perte d'un orbiteur martien](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter#Cause_of_failure)
+ en est une : un composant travaillait en newtons, et un autre en livres-force. Les défenses
+ habituelles sont les conventions de nommage, les commentaires et une classe enveloppe pour
+ chaque unité. Elles reposent sur la discipline, et ne détectent rien automatiquement.

- Here the units of every quantity are recorded in its type, and
- [dimensional analysis](https://en.wikipedia.org/wiki/Dimensional_analysis) is performed at
- [compiletime]. A `Quantity` has a type parameter for its units, so a length and a duration are
- different types, and the compiler refuses to add them. Arithmetic combines units correctly:
- dividing a length by a duration gives a speed. Quantities of compatible units convert into
- one another automatically.
+ Ici, les unités de chaque quantité sont enregistrées dans son type, et
+ l'[analyse dimensionnelle](https://en.wikipedia.org/wiki/Dimensional_analysis) est effectuée
+ [à la compilation][compilation]. Une `Quantity` a un paramètre de type pour ses unités, de
+ sorte qu'une longueur et une durée sont des types différents, et que le compilateur refuse
+ de les additionner. L'arithmétique combine correctement les unités : diviser une longueur
+ par une durée donne une vitesse. Les quantités d'unités compatibles se convertissent
+ automatiquement les unes dans les autres.

- None of this exists at runtime. A `Quantity` is an [opaque type] over a `Double`, and its
- operations are inlined, so the safety is paid for entirely at compiletime.
+ Rien de tout cela n'existe à l'exécution. Une `Quantity` est un [type opaque] sur un
+ `Double`, et ses opérations sont inlinées ; la sûreté est donc entièrement payée à la
+ compilation.

- The units, the prefixes and the operations on quantities all come from the `soundness`
- package. Arithmetic between a plain number and a quantity relies on the compiler's *into*
- conversions, so that language feature is enabled too. A `Decimalizer` in scope fixes how many
- significant figures a quantity is shown with, and the last import lets the samples print
- their results:
+ Les unités, les préfixes et les opérations sur les quantités viennent tous du package
+ `soundness`. L'arithmétique entre un nombre simple et une quantité repose sur les
+ conversions *into* du compilateur ; cette fonctionnalité du langage est donc également
+ activée. Un `Decimalizer` dans la portée fixe le nombre de chiffres significatifs avec
+ lequel une quantité est affichée, et le dernier import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import scala.language.experimental.into
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)
```

- ### Quantities and units
+ ### Quantités et unités

- A quantity is a number multiplied by a unit. The SI base units are values: `Metre`, `Second`,
- `Gram`, `Kelvin`, `Ampere`, `Candela` and `Mole`. So are many other units.
+ Une quantité est un nombre multiplié par une unité. Les unités de base du SI sont des
+ valeurs : `Metre`, `Second`, `Gram`, `Kelvin`, `Ampere`, `Candela` et `Mole`. Bien d'autres
+ unités le sont aussi.

```scala
val distance = 5*Metre
val duration = 2*Second
```

- `show` writes a quantity with its unit:
+ `show` écrit une quantité avec son unité :

```scala fuel
run
##
Out.println(distance.show)
Out.println(duration.show)
```

- The unit lives in the type. `5*Metre` is a `Quantity[Metres[1]]`, in which the `1` is the
- power to which the unit is raised. An area would be `Metres[2]`, and a frequency
- `Seconds[-1]`. These types are inferred, so they rarely need to be written, but they are what
- the compiler checks.
+ L'unité réside dans le type. `5*Metre` est une `Quantity[Metres[1]]`, dans laquelle le `1`
+ est la puissance à laquelle l'unité est élevée. Une aire serait `Metres[2]`, et une
+ fréquence `Seconds[-1]`. Ces types sont inférés ; il est donc rarement nécessaire de les
+ écrire, mais c'est eux que le compilateur vérifie.

- ### Arithmetic and dimensions
+ ### Arithmétique et dimensions

- Quantities of the same unit add and subtract. Multiplication and division combine units, and
- give a quantity whose type records the new dimensions:
+ Les quantités de même unité s'additionnent et se soustraient. La multiplication et la
+ division combinent les unités, et donnent une quantité dont le type enregistre les
+ nouvelles dimensions :

```scala fuel
run
##
Out.println((Metre + Metre*2.0).show)
Out.println((2*Second * 3*Metre).show)
Out.println((distance/duration).show)
Out.println((2*Metre/Second).invert.show)
```

- Adding quantities whose dimensions disagree is a compile error. The message names the
- mismatch in plain words, not in terms of types:
+ Additionner des quantités dont les dimensions ne concordent pas est une erreur de
+ compilation. Le message nomme la discordance en termes simples, et non en termes de types :

```scala fuel
compile
errors show
##
Metre + 2*Second
```

- The check is by dimension, not merely by unit, so a length cannot be added to an energy:
+ La vérification porte sur la dimension, et pas seulement sur l'unité ; une longueur ne peut
+ donc pas être ajoutée à une énergie :

```scala fuel
compile
errors show
##
2*Metre + 2*Joule
```

- Even two powers of the same dimension stay apart. A length is not an area:
+ Même deux puissances d'une même dimension restent distinctes. Une longueur n'est pas une
+ aire :

```scala fuel
compile
errors show
##
7*Metre >= 2*Metre*Metre
```

- ### Mixing units
+ ### Mélanger les unités

- Different units of the *same* dimension combine freely, and are converted automatically. The
- result takes the unit of the principal operand, which here is the SI one:
+ Des unités différentes de la *même* dimension se combinent librement, et sont converties
+ automatiquement. Le résultat prend l'unité de l'opérande principal, qui est ici celle du
+ SI :

```scala fuel
run
##
Out.println((2*Metre + 2*Foot).show)
Out.println((2*Foot + 2*Metre).show)
```

- Conversion applies to multiplication and division too. An expression may be written in
- whatever units are natural, and still give a coherent result:
+ La conversion s'applique aussi à la multiplication et à la division. Une expression peut
+ être écrite dans les unités qui sont naturelles, et donner tout de même un résultat
+ cohérent :

```scala
val width = 2*Metre
val depth = 3*Foot
```

```scala fuel
run
##
Out.println((width*depth).show)
```

- ### Converting
+ ### Conversion

- `convert` converts a quantity explicitly to another compatible unit, naming the family of the
- target unit:
+ `convert` convertit explicitement une quantité dans une autre unité compatible, en nommant
+ la famille de l'unité cible :

```scala fuel
run
##
Out.println((3*Foot).convert[Metres].show)
Out.println((3*Metre).convert[Feet].show)
```

- `normalize` does the same, with the unit type written with its power. An hour is not one of
- the predefined unit values, but a unit is only a quantity of one, so it is defined in one
- line:
+ `normalize` fait de même, avec le type d'unité écrit avec sa puissance. Une heure ne fait
+ pas partie des valeurs d'unités prédéfinies, mais une unité n'est qu'une quantité valant un ;
+ elle se définit donc en une ligne :

```scala
val Hour: Quantity[Hours[1]] = Quantity(1.0)
```

```scala fuel
run
##
Out.println((2*Hour).normalize[Seconds[1]].show)
Out.println((1*Inch).normalize[Metres[1]].show)
```

- `normalize` acts only on the dimension of the unit which it names. Components which are
- measured in that dimension are converted to the target, and components of every other
- dimension are left exactly as they were. Normalizing a speed to seconds therefore rescales
- its time, and leaves its distance untouched. It follows that converting to a unit of a
- dimension which the quantity does not have is not an error. It simply does nothing, because
- there is no matching component to rescale.
+ `normalize` n'agit que sur la dimension de l'unité qu'elle nomme. Les composantes mesurées
+ dans cette dimension sont converties vers la cible, et les composantes de toute autre
+ dimension sont laissées exactement telles qu'elles étaient. Normaliser une vitesse en
+ secondes met donc à l'échelle son temps, et laisse sa distance intacte. Il s'ensuit que
+ convertir vers une unité d'une dimension que la quantité ne possède pas n'est pas une
+ erreur. Cela ne fait simplement rien, car il n'y a aucune composante correspondante à
+ mettre à l'échelle.

- ### Metric prefixes
+ ### Préfixes métriques

- A [metric prefix](https://en.wikipedia.org/wiki/Metric_prefix) scales a unit. `Kilo`, `Mega`,
- `Giga`, `Milli`, `Micro`, `Nano` and the rest apply to a unit, to give a larger or a smaller
- one. The binary prefixes, `Kibi`, `Mebi` and `Gibi`, scale by powers of two instead of ten,
- for quantities of information:
+ Un [préfixe métrique](https://en.wikipedia.org/wiki/Metric_prefix) met une unité à
+ l'échelle. `Kilo`, `Mega`, `Giga`, `Milli`, `Micro`, `Nano` et les autres s'appliquent à une
+ unité pour en donner une plus grande ou plus petite. Les préfixes binaires, `Kibi`, `Mebi`
+ et `Gibi`, mettent à l'échelle par puissances de deux plutôt que de dix, pour les quantités
+ d'information :

```scala fuel
run
##
Out.println((15*Kilo(Metre)).show)
Out.println((1.5*Milli(Metre)).show)
Out.println((10*Kibi(Metre)).show)
```

- The full set, decimal and binary:
+ L'ensemble complet, décimal et binaire :

```tabular
- column  Prefix
- column  Symbol
- column  Factor
- row  `Quetta`  Q  10³⁰
- row  `Ronna`  R  10²⁷
- row  `Yotta`  Y  10²⁴
- row  `Zetta`  Z  10²¹
- row  `Exa`  E  10¹⁸
- row  `Peta`  P  10¹⁵
- row  `Tera`  T  10¹²
- row  `Giga`  G  10⁹
- row  `Mega`  M  10⁶
- row  `Kilo`  k  10³
- row  `Hecto`  h  10²
- row  `Deka`  da  10¹
- row  `Deci`  d  10⁻¹
- row  `Centi`  c  10⁻²
- row  `Milli`  m  10⁻³
- row  `Micro`  µ  10⁻⁶
- row  `Nano`  n  10⁻⁹
- row  `Pico`  p  10⁻¹²
- row  `Femto`  f  10⁻¹⁵
- row  `Atto`  a  10⁻¹⁸
- row  `Zepto`  z  10⁻²¹
- row  `Yocto`  y  10⁻²⁴
- row  `Ronto`  r  10⁻²⁷
- row  `Quecto`  q  10⁻³⁰
- row  `Kibi`  Ki  2¹⁰
- row  `Mebi`  Mi  2²⁰
- row  `Gibi`  Gi  2³⁰
- row  `Tebi`  Ti  2⁴⁰
- row  `Pebi`  Pi  2⁵⁰
- row  `Exbi`  Ei  2⁶⁰
- row  `Zebi`  Zi  2⁷⁰
- row  `Yobi`  Yi  2⁸⁰
+ column  Préfixe
+ column  Symbole
+ column  Facteur
+ row  `Quetta`  Q  10³⁰
+ row  `Ronna`  R  10²⁷
+ row  `Yotta`  Y  10²⁴
+ row  `Zetta`  Z  10²¹
+ row  `Exa`  E  10¹⁸
+ row  `Peta`  P  10¹⁵
+ row  `Tera`  T  10¹²
+ row  `Giga`  G  10⁹
+ row  `Mega`  M  10⁶
+ row  `Kilo`  k  10³
+ row  `Hecto`  h  10²
+ row  `Deka`  da  10¹
+ row  `Deci`  d  10⁻¹
+ row  `Centi`  c  10⁻²
+ row  `Milli`  m  10⁻³
+ row  `Micro`  µ  10⁻⁶
+ row  `Nano`  n  10⁻⁹
+ row  `Pico`  p  10⁻¹²
+ row  `Femto`  f  10⁻¹⁵
+ row  `Atto`  a  10⁻¹⁸
+ row  `Zepto`  z  10⁻²¹
+ row  `Yocto`  y  10⁻²⁴
+ row  `Ronto`  r  10⁻²⁷
+ row  `Quecto`  q  10⁻³⁰
+ row  `Kibi`  Ki  2¹⁰
+ row  `Mebi`  Mi  2²⁰
+ row  `Gibi`  Gi  2³⁰
+ row  `Tebi`  Ti  2⁴⁰
+ row  `Pebi`  Pi  2⁵⁰
+ row  `Exbi`  Ei  2⁶⁰
+ row  `Zebi`  Zi  2⁷⁰
+ row  `Yobi`  Yi  2⁸⁰
```

- Mass is the one awkward case among the base units, because its SI base unit is itself a
- prefixed unit: the kilogram, not the gram. The unit is still named `Gram`, for consistency
- with every other unit, and the base unit is written as the prefixed `Kilo(Gram)`. But the
- dimension is `Kilograms`, and masses are always normalized and displayed in kilograms. So
- `Kilo(Gram)` is one whole `kg`, while a bare `Gram` is a thousandth of it:
+ La masse est le seul cas délicat parmi les unités de base, car son unité de base SI est
+ elle-même une unité préfixée : le kilogramme, et non le gramme. L'unité s'appelle tout de
+ même `Gram`, par cohérence avec toutes les autres unités, et l'unité de base s'écrit avec
+ le préfixe, `Kilo(Gram)`. Mais la dimension est `Kilograms`, et les masses sont toujours
+ normalisées et affichées en kilogrammes. Ainsi, `Kilo(Gram)` vaut un `kg` entier, tandis
+ qu'un simple `Gram` en vaut un millième :

```scala fuel
run
##
Out.println((1*Kilo(Gram)).show)
Out.println((1*Gram).show)
```

- ### Comparing
+ ### Comparaison

- Quantities of compatible units compare with the usual operators, and the comparison converts
- units as arithmetic does:
+ Les quantités d'unités compatibles se comparent avec les opérateurs habituels, et la
+ comparaison convertit les unités comme le fait l'arithmétique :

```scala fuel
run
##
Out.println((6*Foot < 2*Metre).inspect)
Out.println((9*Foot*Foot < Metre*Metre).inspect)
```

- Equality across different units of one dimension uses `===`, which asks whether two
- quantities measure the same amount:
+ L'égalité entre différentes unités d'une même dimension utilise `===`, qui demande si deux
+ quantités mesurent la même quantité :

```scala fuel
run
##
Out.println((Mile === 1760*Yard).inspect)
Out.println((Mile === Inch).inspect)
```

- As with arithmetic, comparing incompatible dimensions does not compile.
+ Comme pour l'arithmétique, comparer des dimensions incompatibles ne compile pas.

- Equality is exact, whether through `===` or `==`. Two quantities count as equal only when
- their underlying values agree to the last bit. A conversion which introduces a
- [rounding difference](https://en.wikipedia.org/wiki/Floating-point_arithmetic) can therefore
- leave quantities which are equal on paper comparing as unequal. For converted values, `<` and
- `>`, or an explicit tolerance, are usually the better test.
+ L'égalité est exacte, que ce soit avec `===` ou avec `==`. Deux quantités ne sont
+ considérées comme égales que lorsque leurs valeurs sous-jacentes concordent jusqu'au
+ dernier bit. Une conversion qui introduit une
+ [différence d'arrondi](https://en.wikipedia.org/wiki/Floating-point_arithmetic) peut donc
+ faire que des quantités égales sur le papier se comparent comme inégales. Pour des valeurs
+ converties, `<` et `>`, ou une tolérance explicite, constituent généralement un meilleur
+ test.

- Plain `==` is reserved for quantities which already share a unit type, where it compares
- those underlying values directly. Two quantities of different units have no `==` between
- them. Under strict equality, with `import language.strictEquality`, such a comparison does
- not compile, which is exactly the gap that `===` fills.
+ Le simple `==` est réservé aux quantités qui partagent déjà un type d'unité, où il compare
+ directement ces valeurs sous-jacentes. Deux quantités d'unités différentes n'ont pas de
+ `==` entre elles. Avec l'égalité stricte, `import language.strictEquality`, une telle
+ comparaison ne compile pas, et c'est exactement ce manque que comble `===`.

- ### Derived units
+ ### Unités dérivées

- The named SI units, such as `Newton`, `Joule`, `Watt`, `Pascal` and `Volt`, are defined in
- terms of the base units, so they work together with them. A force in newtons and a length in
- meters multiply to an energy. Every quantity can also name its own dimension, worked out
- from its units, and even an unusual combination resolves to its physical name:
+ Les unités SI nommées, telles que `Newton`, `Joule`, `Watt`, `Pascal` et `Volt`, sont
+ définies en fonction des unités de base ; elles fonctionnent donc avec elles. Une force en
+ newtons et une longueur en mètres se multiplient pour donner une énergie. Chaque quantité
+ peut aussi nommer sa propre dimension, calculée à partir de ses unités, et même une
+ combinaison inhabituelle se résout en son nom physique :

```scala fuel
run
##
Out.println(Metre.dimension)
Out.println((Metre/Second).dimension)
Out.println((Newton*Metre).dimension)
Out.println((Foot*Foot*Kilo(Gram)/(Second*Second*Mole)).dimension)
```

- ### The SI units
+ ### Les unités SI

- Seven base dimensions are defined, each with its type of units, and the value which names
- one of them:
+ Sept dimensions de base sont définies, chacune avec son type d'unités, et la valeur qui
+ nomme l'une d'elles :

```tabular
- column  Dimension
- column  Units
- column  Unit
- row  `Distance`  `Metres`  `Metre`
- row  `Mass`  `Kilograms`  `Gram`
- row  `Time`  `Seconds`  `Second`
- row  `Current`  `Amperes`  `Ampere`
- row  `Luminosity`  `Candelas`  `Candela`
- row  `AmountOfSubstance`  `Moles`  `Mole`
- row  `Heat`  `Kelvins`  `Kelvin`
+ column  Dimension
+ column  Unités
+ column  Unité
+ row  `Distance`  `Metres`  `Metre`
+ row  `Mass`  `Kilograms`  `Gram`
+ row  `Time`  `Seconds`  `Second`
+ row  `Current`  `Amperes`  `Ampere`
+ row  `Luminosity`  `Candelas`  `Candela`
+ row  `AmountOfSubstance`  `Moles`  `Mole`
+ row  `Heat`  `Kelvins`  `Kelvin`
```

- The named derived units are defined in terms of those, so each has its full dimensions, and
- is not a distinct kind of thing. The last column is the name which its `dimension` reports:
+ Les unités dérivées nommées sont définies en fonction de celles-ci ; chacune a donc ses
+ dimensions complètes, et n'est pas une sorte de chose distincte. La dernière colonne est
+ le nom que rapporte sa `dimension` :

```tabular
- column  Unit
- column  Definition
- column  Dimension
- row  `Hertz`  one per second  frequency
- row  `Newton`  meter-kilogram per second squared  force
- row  `Pascal`  newton per square meter  pressure
- row  `Joule`  newton-meter  energy
- row  `Watt`  joule per second  power
- row  `Coulomb`  second-ampere  electric charge
- row  `Volt`  watt per ampere  electric potential
- row  `Farad`  coulomb per volt  capacitance
- row  `Ohm`  volt per ampere  impedance
- row  `Siemens`  ampere per volt  electric conductance
- row  `Weber`  volt-second  magnetic flux
- row  `Tesla`  weber per square meter  magnetic flux density
- row  `Henry`  weber per ampere  inductance
- row  `Lux`  candela per square meter  illuminance
- row  `Becquerel`  one per second  frequency
- row  `Gray`  joule per kilogram  specific energy
- row  `Sievert`  joule per kilogram  specific energy
- row  `Katal`  mole per second
+ column  Unité
+ column  Définition
+ column  Dimension
+ row  `Hertz`  un par seconde  fréquence
+ row  `Newton`  mètre-kilogramme par seconde carrée  force
+ row  `Pascal`  newton par mètre carré  pression
+ row  `Joule`  newton-mètre  énergie
+ row  `Watt`  joule par seconde  puissance
+ row  `Coulomb`  seconde-ampère  charge électrique
+ row  `Volt`  watt par ampère  potentiel électrique
+ row  `Farad`  coulomb par volt  capacité
+ row  `Ohm`  volt par ampère  impédance
+ row  `Siemens`  ampère par volt  conductance électrique
+ row  `Weber`  volt-seconde  flux magnétique
+ row  `Tesla`  weber par mètre carré  densité de flux magnétique
+ row  `Henry`  weber par ampère  inductance
+ row  `Lux`  candela par mètre carré  éclairement
+ row  `Becquerel`  un par seconde  fréquence
+ row  `Gray`  joule par kilogramme  énergie massique
+ row  `Sievert`  joule par kilogramme  énergie massique
+ row  `Katal`  mole par seconde
```

- The CGS and imperial systems are provided beside them: `Dyne`, `Erg`, `Gauss`, `Poise`,
- `Foot`, `Furlong`, `Stone` and the rest.
+ Les systèmes CGS et impérial sont fournis à côté : `Dyne`, `Erg`, `Gauss`, `Poise`, `Foot`,
+ `Furlong`, `Stone` et les autres.

- ### Displaying
+ ### Affichage

- `show` writes a quantity out, and needs a `Decimalizer` in scope to fix the number of
- significant figures. Compound units are written with a middle dot and superscript powers,
- and very large or very small values switch to scientific notation:
+ `show` écrit une quantité, et requiert un `Decimalizer` dans la portée pour fixer le nombre
+ de chiffres significatifs. Les unités composées s'écrivent avec un point médian et des
+ puissances en exposant, et les valeurs très grandes ou très petites passent en notation
+ scientifique :

```scala fuel
run
##
Out.println((7.567*Metre).show)
Out.println((1.4*Metre*Metre).show)
Out.println((8.54*Metre/Second).show)
Out.println((8.54*Kilo(Metre)/Second).show)
Out.println(constants.SpeedOfLightInVacuum.show)
```

- `show` is the only correct way to write a quantity out. A `Quantity` is an opaque type over
- a `Double`, so its `toString` is the `Double`'s: the bare number, with no units at all. A
- quantity which is substituted into a plain string therefore silently loses the very thing
- which distinguishes it. [Text](text.md#rendering-numbers) describes the `Decimalizer`.
+ `show` est la seule façon correcte d'écrire une quantité. Une `Quantity` est un type opaque
+ sur un `Double` ; son `toString` est donc celui du `Double` : le nombre seul, sans aucune
+ unité. Une quantité substituée dans une chaîne ordinaire perd donc silencieusement ce qui
+ précisément la distingue. [Texte](text.md#rendering-numbers) décrit le `Decimalizer`.

- Instead of scientific notation, a quantity can scale itself to a metric prefix for display.
- A `Prefixes` instance lists the prefixes to choose from, and `show` picks the one which keeps
- the number readable:
+ Plutôt que la notation scientifique, une quantité peut se mettre à l'échelle d'un préfixe
+ métrique pour l'affichage. Une instance `Prefixes` liste les préfixes parmi lesquels
+ choisir, et `show` choisit celui qui garde le nombre lisible :

```scala
sealed trait Information extends Dimension
sealed trait Bits[Power <: Nat] extends Units[Power, Information]
val Bit: MetricUnit[Bits[1]] = MetricUnit(1.0)
given bitDesignation: Designation[Bits[1]] = () => t"bit"

given bitPrefixes: (Prefixes on Bits[1]) = Prefixes(List(Kilo, Mega, Giga, Tera))
```

```scala fuel
run
##
Out.println((5000*Bit).show)
Out.println((5_000_000*Bit).show)
```

- That sample also shows how a new dimension and unit are declared: a `Dimension`, a `Units`
- type indexed by its power, a unit value, and a `Designation` which gives its symbol.
+ Cet exemple montre aussi comment déclarer une nouvelle dimension et une nouvelle unité :
+ une `Dimension`, un type `Units` indexé par sa puissance, une valeur d'unité, et une
+ `Designation` qui donne son symbole.

- ### Defining a unit of your own
+ ### Définir sa propre unité

- A unit which measures something for which the SI has no dimension needs both a dimension and
- a type of units. Take FLOPS, which is a processor's floating-point instructions per second.
- The definition is built up in four steps:
+ Une unité qui mesure quelque chose pour lequel le SI n'a pas de dimension requiert à la
+ fois une dimension et un type d'unités. Prenons les FLOPS, le nombre d'instructions en
+ virgule flottante qu'un processeur exécute par seconde. La définition se construit en
+ quatre étapes :

```scala fuel
frame
  label A bare rate, in inverse seconds
  code
      val SimpleFlop = 1.0/Second
frame
  label A metric unit, so that the prefixes apply
  code
      val PrefixableFlop: MetricUnit[Seconds[-1]] = MetricUnit(1)

      Mega(PrefixableFlop)
frame
  label A dimension of its own
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      Mega(Flop)*Second
frame
  label With a symbol for display
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      given Designation[Flops[1]] = () => t"FLOPS"

      Mega(Flop)*Second
##
```

- Written as a bare value, `1.0/Second` can be used in arithmetic, but says nothing about what
- it counts. Wrapping it as a `MetricUnit` at least makes the metric prefixes apply, so
- `Mega(Flop)` can be written, but the result is still just a number in units of inverse
- seconds. Giving it a dimension of its own makes it distinct from every frequency. The
- `power` parameter is required, and is bounded by `Nat`, which is an alias for
- `Int & Singleton`. `Mega(Flop)*Second` then has the type
- `Quantity[Flops[1] & Seconds[1]]`, and a `Designation` gives it a symbol for display.
+ Écrit comme une simple valeur, `1.0/Second` peut servir dans l'arithmétique, mais ne dit
+ rien de ce qu'il compte. L'envelopper dans une `MetricUnit` permet au moins d'appliquer
+ les préfixes métriques, de sorte que l'on peut écrire `Mega(Flop)`, mais le résultat n'est
+ toujours qu'un nombre en secondes inverses. Lui donner sa propre dimension le distingue de
+ toute fréquence. Le paramètre `power` est obligatoire, et est borné par `Nat`, qui est un
+ alias de `Int & Singleton`. `Mega(Flop)*Second` a alors le type
+ `Quantity[Flops[1] & Seconds[1]]`, et une `Designation` lui donne un symbole pour
+ l'affichage.

- Both traits may be `sealed`, since nothing implements them at runtime.
+ Les deux traits peuvent être `sealed`, puisque rien ne les implémente à l'exécution.

- ### Conversion ratios
+ ### Rapports de conversion

- Converting between two units needs the ratio between them. That comes from a contextual
- `Ratio` for the pair, with one unit at the power `1` and the other at the power `-1`, and the
- rate as a singleton literal `Double`:
+ Convertir entre deux unités requiert le rapport entre elles. Celui-ci provient d'un
+ `Ratio` contextuel pour la paire, avec une unité à la puissance `1` et l'autre à la
+ puissance `-1`, et le taux sous forme de `Double` littéral de type singleton :

```scala
inline given ratio: Ratio[Metres[1] & Furlongs[-1], 201.168] = !!
```

- The rate is a *type*, not a value, and that is what makes a conversion cost nothing. Its
- value is known at compiletime, so arithmetic on it is done as the code compiles, and the
- given itself can be erased. A conversion between two units therefore contributes no work at
- runtime beyond the multiplication which it actually denotes.
+ Le taux est un *type*, et non une valeur, et c'est ce qui rend une conversion gratuite. Sa
+ valeur est connue à la compilation ; l'arithmétique qui le concerne est donc effectuée
+ pendant la compilation du code, et le given lui-même peut être effacé. Une conversion
+ entre deux unités n'ajoute donc aucun travail à l'exécution au-delà de la multiplication
+ qu'elle dénote effectivement.

- ### Mixed and non-decimal bases
+ ### Bases mixtes et non décimales

- Not every measurement is a single number in a single unit. Before the metric system,
- quantities were expressed as a *cascade* of units, in exact multiples which were not powers
- of ten: feet and inches, or stones, pounds and ounces. Some still are. Hours, minutes and
- seconds are exactly that, and so is a person's height in most of the English-speaking world.
+ Toute mesure n'est pas un nombre unique dans une unité unique. Avant le système métrique,
+ les quantités s'exprimaient comme une *cascade* d'unités, en multiples exacts qui
+ n'étaient pas des puissances de dix : pieds et pouces, ou stones, livres et onces.
+ Certaines le sont encore. Les heures, minutes et secondes sont exactement cela, tout comme
+ la taille d'une personne dans la majeure partie du monde anglophone.

- A `Quanta` represents one such value: a whole number of each unit in a descending cascade,
- with the carrying done automatically. The type states the cascade, with the smallest unit as
- the type parameter, and the larger ones after `in`:
+ Une `Quanta` représente une telle valeur : un nombre entier de chaque unité dans une
+ cascade descendante, avec les retenues effectuées automatiquement. Le type énonce la
+ cascade, avec la plus petite unité comme paramètre de type, et les plus grandes après
+ `in` :

```scala
type Weight = Quanta[Ounces[1]] in (Pounds[1], Stones[1])
type Height = Quanta[Inches[1]] in (Feet[1])
```

- A value is written with the largest unit first, as it is spoken, and each component is read
- back by naming its unit. This weight is 1 stone, 3 pounds and 2 ounces:
+ Une valeur s'écrit avec la plus grande unité en premier, comme on la prononce, et chaque
+ composante se relit en nommant son unité. Ce poids est de 1 stone, 3 livres et 2 onces :

```scala
val weight: Weight = Quanta(1, 3, 2)
```

```scala fuel
run
##
Out.println(weight[Stones].show)
Out.println(weight[Pounds].show)
Out.println(weight[Ounces].show)
```

- Arithmetic carries between the units as the ratios require. Adding fifteen ounces to 3 pounds
- 2 ounces gives 4 pounds 1 ounce, not 3 pounds 17 ounces:
+ L'arithmétique effectue les retenues entre les unités comme l'exigent les rapports.
+ Ajouter quinze onces à 3 livres 2 onces donne 4 livres 1 once, et non 3 livres 17 onces :

```scala
val heavier: Weight = weight + Quanta(15)
```

```scala fuel
run
##
Out.println(heavier[Pounds].show)
Out.println(heavier[Ounces].show)
```

- The cascade is checked as it is written. Every unit in it must have the same dimension, so
- mixing seconds into a cascade of lengths does not compile, and the base must be a unit, not
- an arbitrary type. `TimeSeconds` and `TimeMinutes` are provided, for the two cascades which
- everyone needs.
+ La cascade est vérifiée telle qu'elle est écrite. Chaque unité qui la compose doit avoir
+ la même dimension ; mélanger des secondes dans une cascade de longueurs ne compile donc
+ pas, et la base doit être une unité, et non un type arbitraire. `TimeSeconds` et
+ `TimeMinutes` sont fournis, pour les deux cascades dont tout le monde a besoin.

- The units are real units, so a `Quanta` converts to and from an ordinary `Quantity`, which
- is how a measurement crosses between the two worlds:
+ Les unités sont de vraies unités ; une `Quanta` se convertit donc vers et depuis une
+ `Quantity` ordinaire, et c'est ainsi qu'une mesure passe d'un monde à l'autre :

```scala
val length: Quantity[Metres[1]] = 5.9*Foot + 10.0*Inch
val height = length.quanta[Height]
```

```scala fuel
run
##
Out.println(height[Feet].show)
Out.println(height[Inches].show)
Out.println(weight.quantity.convert[Pounds].show)
```

- Showing a `Quanta` uses each unit's symbol, as in `1st 3lb 2oz`. A `UnitsNames` given
- overrides the symbols where a cascade is conventionally written differently, so that a
- height can be shown as `5' 9"`.
+ Montrer une `Quanta` utilise le symbole de chaque unité, comme dans `1st 3lb 2oz`. Un given
+ `UnitsNames` remplace les symboles là où une cascade s'écrit conventionnellement autrement,
+ de sorte qu'une taille peut être montrée sous la forme `5' 9"`.

- ### Temperature
+ ### Température

- Temperature is measured from an arbitrary zero, not from nothing, so it behaves unlike a
- length or a mass. Thirty degrees plus thirty degrees is not sixty degrees of anything
- meaningful. It is modeled as a separate type, `Temperature`, and the scales with an
- arbitrary zero are kept distinct from the absolute Kelvin quantity which is used for
- differences.
+ La température se mesure à partir d'un zéro arbitraire, et non à partir de rien ; elle se
+ comporte donc autrement qu'une longueur ou une masse. Trente degrés plus trente degrés ne
+ font pas soixante degrés de quoi que ce soit de significatif. Elle est modélisée par un
+ type distinct, `Temperature`, et les échelles à zéro arbitraire sont tenues à part de la
+ quantité absolue en Kelvin, qui sert pour les différences.

- Four scales appear, and they fall into two kinds. Kelvin and Rankine measure from absolute
- zero, so they are ordinary quantities as well as scales. Celsius and Fahrenheit place their
- zero elsewhere, at the freezing point of water and lower still, so a reading on either is
- meaningful only as a `Temperature`. Multiplying or adding two such readings directly would
- be meaningless, and the types do not allow it.
+ Quatre échelles apparaissent, et elles se répartissent en deux sortes. Kelvin et Rankine
+ mesurent à partir du zéro absolu ; ce sont donc des quantités ordinaires en même temps que
+ des échelles. Celsius et Fahrenheit placent leur zéro ailleurs, au point de congélation de
+ l'eau et plus bas encore ; une lecture sur l'une ou l'autre n'a donc de sens qu'en tant
+ que `Temperature`. Multiplier ou additionner directement deux telles lectures n'aurait
+ aucun sens, et les types ne le permettent pas.

- A temperature is created on a named scale, and displayed on whichever scale is in scope:
+ Une température se crée sur une échelle nommée, et s'affiche sur l'échelle qui est dans la
+ portée :

```tabular
- column  Scale
- column  Zero
- column  Reading
- column  As a quantity
- column  Display import
- row  Kelvin  absolute zero  `Kelvin(…)`  the unit `Kelvin`  `temperatureScales.kelvinScale`
- row  Rankine  absolute zero  `Rankine(…)`  the units `Rankines`  `temperatureScales.rankineScale`
- row  Celsius  freezing point of water  `Celsius(…)`  none  `temperatureScales.celsiusScale`
- row  Fahrenheit  below freezing  `Fahrenheit(…)`  none  `temperatureScales.fahrenheitScale`
+ column  Échelle
+ column  Zéro
+ column  Lecture
+ column  En tant que quantité
+ column  Import d'affichage
+ row  Kelvin  zéro absolu  `Kelvin(…)`  l'unité `Kelvin`  `temperatureScales.kelvinScale`
+ row  Rankine  zéro absolu  `Rankine(…)`  unités `Rankines`  `temperatureScales.rankineScale`
+ row  Celsius  gel de l'eau  `Celsius(…)`  aucune  `temperatureScales.celsiusScale`
+ row  Fahrenheit  sous le gel  `Fahrenheit(…)`  aucune  `temperatureScales.fahrenheitScale`
```

```scala fuel
run
##
locally:
  import temperatureScales.celsiusScale
  Out.println(Celsius(30).show)
  Out.println(Fahrenheit(100).show)

locally:
  import temperatureScales.fahrenheitScale
  Out.println(Celsius(30).show)
  Out.println(Fahrenheit(100).show)
```

- Reading the same temperature on a different scale is thus a matter of importing a different
- one, with no conversion written by hand.
+ Lire la même température sur une autre échelle revient donc à en importer une autre, sans
+ écrire de conversion à la main.

- A `Temperature` adds and subtracts Kelvin or Rankine *quantities*, to give another
- temperature. The difference of two temperatures is a quantity, which converts like any
- other:
+ Une `Temperature` additionne et soustrait des *quantités* en Kelvin ou en Rankine, pour
+ donner une autre température. La différence de deux températures est une quantité, qui se
+ convertit comme n'importe quelle autre :

```scala fuel
run
##
locally:
  import temperatureScales.celsiusScale

  Out.println((zero[Temperature] + 300*Kelvin).show)
  Out.println((Fahrenheit(100) - zero[Temperature]).convert[Rankines].show)
```

- ### Aggregating
+ ### Agrégation

- A collection of quantities which share a unit reduces to a quantity, and each reduction
- understands units:
+ Une collection de quantités qui partagent une unité se réduit à une quantité, et chaque
+ réduction comprend les unités :

```tabular
- column  Method
- column  Result
- column  Of an empty collection
- row  `total`  the sum, in the same unit  zero
- row  `mean`  the mean, in the same unit  `Unset`
- row  `variance`  the variance, in the unit squared  `Unset`
- row  `std`  the standard deviation, in the same unit  `Unset`
+ column  Méthode
+ column  Résultat
+ column  Pour une collection vide
+ row  `total`  la somme, dans la même unité  zéro
+ row  `mean`  la moyenne, dans la même unité  `Unset`
+ row  `variance`  la variance, dans l'unité au carré  `Unset`
+ row  `std`  l'écart type, dans la même unité  `Unset`
```

- So the variance of a list of seconds is in seconds squared, and the standard deviation is
- back in seconds:
+ Ainsi, la variance d'une liste de secondes est en secondes carrées, et l'écart type est de
+ nouveau en secondes :

```scala
val samples = List(1*Second, 2*Second, 3*Second)
```

```scala fuel
run
##
Out.println(samples.total.let(_.show).or(t"none"))
Out.println(samples.mean.let(_.show).or(t"none"))
Out.println(samples.std.let(_.show).or(t"none"))
```

- [Mathematics](mathematics.md#elements-that-are-not-numbers) builds vectors and matrices of
- quantities, whose products carry units too.
+ [Mathématiques](mathematics.md#elements-that-are-not-numbers) construit des vecteurs et des
+ matrices de quantités, dont les produits portent aussi des unités.

- ### Quantifying your own types
+ ### Quantifier ses propres types

- A type which stands for a measurement can be taught to give a quantity, through a
- `Quantifiable` instance. It then takes part in the same dimensional arithmetic. A
- typographic point, which is one seventy-second of an inch, becomes a real length:
+ Un type qui représente une mesure peut apprendre à donner une quantité, au moyen d'une
+ instance `Quantifiable`. Il prend alors part à la même arithmétique dimensionnelle. Un
+ point typographique, qui vaut un soixante-douzième de pouce, devient une vraie longueur :

```scala
case class Pts(value: Double)
given ptsQuantifiable: Quantifiable[Pts, Inches[1]] = pts => (Inch*pts.value)/72.0
```

```scala fuel
run
##
Out.println((Pts(71).quantify < Inch).inspect)
Out.println((Pts(73).quantify > Inch).inspect)
```

- ### Calculations
+ ### Calculs

- A formula written in quantities is checked as it is written. Each step must combine its
- units coherently, and the units of the result are computed from the units of the inputs, so
- the dimensions of a calculation are verified before it ever runs. The
- [displacement of a body under constant acceleration](https://en.wikipedia.org/wiki/Equations_of_motion#Constant_translational_acceleration_in_a_straight_line),
- `s = ut + ½at²`, reads directly:
+ Une formule écrite en quantités est vérifiée telle qu'elle est écrite. Chaque étape doit combiner
+ ses unités de façon cohérente, et les unités du résultat sont calculées à partir des unités des
+ entrées ; les dimensions d'un calcul sont donc vérifiées avant même qu'il ne s'exécute. Le
+ [déplacement d'un corps sous accélération
+ constante](https://en.wikipedia.org/wiki/Equations_of_motion#Constant_translational_acceleration_in_a_straight_line),
+ `s = ut + ½at²`, se lit directement :

```scala
def displacement
    ( initial: Quantity[Metres[1] & Seconds[-1]],
      time:    Quantity[Seconds[1]],
      accel:   Quantity[Metres[1] & Seconds[-2]] )
:   Quantity[Metres[1]] =
  initial*time + 0.5*accel*time*time
```

```scala fuel
run
##
Out.println(displacement(3*Metre/Second, 2*Second, 9.8*Metre/(Second*Second)).show)
```

- Both terms of the sum come out as a distance: a velocity times a time, and an acceleration
- times a time squared. The addition therefore typechecks, and the result is a distance.
- Passing an argument in the wrong units is a compile error, not a number which is quietly
- wrong:
+ Les deux termes de la somme donnent une distance : une vitesse multipliée par un temps, et
+ une accélération multipliée par un temps au carré. L'addition passe donc la vérification
+ de types, et le résultat est une distance. Passer un argument dans les mauvaises unités
+ est une erreur de compilation, et non un nombre discrètement faux :

```scala fuel
compile
errors show
##
displacement(3*Metre, 2*Second, 9.8*Metre/(Second*Second))
```

- The same calculation can be written once for any consistent choice of units, instead of
- being fixed to meters and seconds. A `transparent inline def` resolves its units at each
- call, so its parameters can be left abstract. The relationships which the formula relies on
- are then stated as constraints: that a velocity times a time, and an acceleration times a
- time squared, can each be formed, and that the two results can be added.
+ Le même calcul peut être écrit une seule fois pour tout choix cohérent d'unités, au lieu
+ d'être figé en mètres et en secondes. Une `transparent inline def` résout ses unités à
+ chaque appel ; ses paramètres peuvent donc rester abstraits. Les relations sur lesquelles
+ repose la formule sont alors énoncées sous forme de contraintes : qu'une vitesse
+ multipliée par un temps, et une accélération multipliée par un temps au carré, puissent
+ chacune être formées, et que les deux résultats puissent être additionnés.

```scala fuel
parse
##
transparent inline def motion
    [ velocity     <: Measure,
      time         <: Measure,
      acceleration <: Measure ]
    ( initial: Quantity[velocity],
      elapsed: Quantity[time],
      rate:    Quantity[acceleration] )
    ( using vt:  Quantity[velocity] is Multiplicable by Quantity[time],
            at:  Quantity[acceleration] is Multiplicable by Quantity[time] )
    ( using att: at.Result is Multiplicable by Quantity[time] )
    ( using add: vt.Result is Addable by att.Result )
=   initial*elapsed + 0.5*rate*elapsed*elapsed

val fallen = motion(3*Metre/Second, 2*Second, 9.8*Metre/(Second*Second))
```

- Those constraints are nothing more than the statement that the equation's dimensions line
- up, expressed in types. A call whose units cannot satisfy them does not compile.
- [Numbers](numbers.md#overloading-the-operators) describes `Multiplicable` and `Addable`.
+ Ces contraintes ne sont rien de plus que l'énoncé, exprimé en types, de la cohérence des
+ dimensions de l'équation. Un appel dont les unités ne peuvent pas les satisfaire ne
+ compile pas. [Nombres](numbers.md#overloading-the-operators) décrit `Multiplicable` et
+ `Addable`.

- ### What it costs at runtime
+ ### Ce que cela coûte à l'exécution

- None of this survives to the bytecode. A `Quantity` is an opaque type over a `Double`, its
- units exist only in the type, and every operation is `inline`. A dimensional calculation
- therefore compiles to the same arithmetic instructions as the equivalent arithmetic on bare
- `Double`s would, with no boxing, no wrapper objects, and no virtual dispatch to a typeclass.
+ Rien de tout cela ne subsiste dans le bytecode. Une `Quantity` est un type opaque sur un
+ `Double`, ses unités n'existent que dans le type, et chaque opération est `inline`. Un
+ calcul dimensionnel se compile donc en les mêmes instructions arithmétiques que
+ l'arithmétique équivalente sur de simples `Double`s, sans boxing, sans objets enveloppes
+ et sans répartition virtuelle vers une classe de types.

- That is a property worth checking, not merely asserting, and it is checked. The test suite
- inspects the compiled bytecode of representative calculations, and fails if a typeclass
- dispatch appears where inlined arithmetic should be. A guarantee about performance which is
- only claimed tends to stop being true.
+ C'est une propriété qui mérite d'être vérifiée, et pas seulement affirmée, et elle est
+ vérifiée. La suite de tests inspecte le bytecode compilé de calculs représentatifs, et
+ échoue si une répartition vers une classe de types apparaît là où devrait se trouver de
+ l'arithmétique inlinée. Une garantie de performance qui n'est qu'affirmée tend à cesser
+ d'être vraie.

- ### Related
+ ### Voir aussi

- - [Numbers](numbers.md) describes the numeric types and the operators which quantities use.
- - [Time](time.md#durations-and-timespans) measures a `Duration` as a quantity of seconds.
- - [Mathematics](mathematics.md) carries units through vectors, matrices and complex numbers.
- - [Money](money.md) applies the same idea, of a unit in the type, to currencies.
- - [Geography](geography.md) and [Chemistry](chemistry.md) build on quantities.
+ - [Nombres](numbers.md) décrit les types numériques et les opérateurs qu'utilisent les
+   quantités.
+ - [Temps](time.md#durations-and-timespans) mesure une `Duration` comme une quantité de
+   secondes.
+ - [Mathématiques](mathematics.md) fait passer les unités à travers les vecteurs, les
+   matrices et les nombres complexes.
+ - [Monnaie](money.md) applique la même idée, d'une unité dans le type, aux devises.
+ - [Géographie](geography.md) et [Chimie](chemistry.md) s'appuient sur les quantités.
