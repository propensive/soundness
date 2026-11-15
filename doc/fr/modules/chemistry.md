- ## Chemistry
+ ## Chimie

- ### About
+ ### Présentation

- The [periodic table](https://en.wikipedia.org/wiki/Periodic_table), chemical formulas and
- chemical equations are modeled as values. All 118 elements are available by their symbols.
- Elements combine into molecules, and molecules into formulas, with ordinary operators. An
- equation joins two formulas with a typed reaction arrow, and can say whether it balances.
+ Le [tableau périodique](https://en.wikipedia.org/wiki/Periodic_table), les formules chimiques et
+ les équations chimiques sont modélisés comme des valeurs. Les 118 éléments sont tous disponibles
+ par leur symbole. Les éléments se combinent en molécules, et les molécules en formules, avec des
+ opérateurs ordinaires. Une équation joint deux formules par une flèche de réaction typée, et
+ peut dire si elle est équilibrée.

- ### On chemical notation
+ ### La notation chimique

- Chemical notation is a small formal language. A molecule is a collection of atoms, in which
- an atom may appear more than once, with a possible charge. A formula is a sum of molecules
- with coefficients. An equation asserts a relationship between two formulas, which is
- directed, reversible, or in equilibrium.
+ La notation chimique est un petit langage formel. Une molécule est une collection d'atomes, dans
+ laquelle un atome peut apparaître plus d'une fois, avec une charge éventuelle. Une formule est
+ une somme de molécules avec des coefficients. Une équation affirme une relation entre deux
+ formules, qui est orientée, réversible, ou à l'équilibre.

- Written as strings, none of that structure is available. `2H₂ + O₂` is just characters, and
- whether an equation balances is not a question which a string can answer.
+ Écrite sous forme de chaînes, aucune de ces structures n'est disponible. `2H₂ + O₂` n'est qu'une
+ suite de caractères, et savoir si une équation est équilibrée n'est pas une question à laquelle
+ une chaîne peut répondre.

- Here the structure is built from typed parts. Formulas are assembled by operators, not
- parsed from text. They are written out with correct subscripts and superscripts. And they
- can be interrogated: the two sides of an equation are compared atom by atom.
+ Ici, la structure est construite à partir de parties typées. Les formules sont assemblées par
+ des opérateurs, et non analysées à partir d'un texte. Elles s'écrivent avec des indices et des
+ exposants corrects. Et on peut les interroger : les deux membres d'une équation sont comparés
+ atome par atome.

- Everything comes from the `soundness` package. The second import lets the samples print
- their results:
+ Tout provient du package `soundness`. Le deuxième import permet aux exemples d'afficher leurs
+ résultats :

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Elements
+ ### Éléments

- Every element is a value on `PeriodicTable`, with its atomic number, its symbol and its
- name. Elements can also be reached by number, or by symbol, at [runtime]:
+ Chaque élément est une valeur de `PeriodicTable`, avec son numéro atomique, son symbole et son
+ nom. Les éléments sont aussi accessibles par numéro, ou par symbole, [à l'exécution][exécution] :

```scala
import PeriodicTable.{H, O, C, Na, Cl, Fe}
```

```scala fuel
run
##
Out.println(H.number.show)
Out.println(Fe.number.show)
Out.println(PeriodicTable(6).let(_.show).or(t"none"))
Out.println(PeriodicTable(119).let(_.show).or(t"none"))
```

- Looking an element up by number gives an [optional](optional-values.md) value, since a
- number may have no element.
+ Chercher un élément par numéro donne une valeur [optionnelle](optional-values.md), puisqu'un
+ numéro peut ne correspondre à aucun élément.

- ### Molecules
+ ### Molécules

- Atoms are bonded into a molecule with `*`, and an atom which is repeated takes its number as
- a type argument. A molecule is written out with proper subscripts:
+ Les atomes se lient en une molécule avec `*`, et un atome répété prend son nombre comme argument
+ de type. Une molécule s'écrit avec des indices corrects :

```scala
val water = H[2]*O
```

```scala fuel
run
##
Out.println(water.show)
Out.println((C*O[2]).show)
Out.println((C[6]*H[12]*O[6]).show)
```

- ### Formulas and equations
+ ### Formules et équations

- A molecule with a coefficient is a term of a formula. Terms are added with `+`, and two
- formulas are joined into an equation with a reaction arrow:
+ Une molécule dotée d'un coefficient est un terme d'une formule. Les termes s'additionnent avec
+ `+`, et deux formules se joignent en une équation par une flèche de réaction :

```scala
val combustion = H[2]*2 + O[2] --> H[2]*O*2
```

```scala fuel
run
##
Out.println(combustion.show)
```

- An equation knows whether its two sides contain the same atoms:
+ Une équation sait si ses deux membres contiennent les mêmes atomes :

```scala fuel
run
##
Out.println(combustion.balanced.inspect)
Out.println((H[2] + O[2] --> H[2]*O).balanced.inspect)
```

- Each side is a typed collection of atoms, not text, so balance is a real check, computed
- from the structure. `atoms` gives that collection for either side. A formula thereby reports
- its own composition, as the number of each element across every molecule and coefficient
- which it contains. That is what computing a molar mass, or checking a proposed reaction,
- needs.
+ Chaque membre est une collection typée d'atomes, et non du texte : l'équilibre est donc une
+ vraie vérification, calculée à partir de la structure. `atoms` donne cette collection pour
+ l'un ou l'autre membre. Une formule indique ainsi sa propre composition, sous forme du nombre de
+ chaque élément à travers toutes les molécules et tous les coefficients qu'elle contient. C'est
+ ce dont a besoin le calcul d'une masse molaire, ou la vérification d'une réaction proposée.

- ### The reaction arrows
+ ### Les flèches de réaction

- Five arrows are available. They mean different things, and are not variants of style:
+ Cinq flèches sont disponibles. Elles signifient des choses différentes, et ne sont pas des
+ variantes de style :

```tabular
- column  Arrow
- column  Reaction
- column  Renders as
- row  `-->`  `Reaction.NetForward`, a net forward reaction  `→`
- row  `<=>`  `Reaction.BothDirections`, a reaction proceeding in both directions  `⇄`
- row  `<~>`  `Reaction.Equilibrium`, a reaction at equilibrium  `⇋`
- row  `<->`  `Reaction.Resonance`  `=`
- row  `===`  `Reaction.Stoichiometric`  `↔`
+ column  Flèche
+ column  Réaction
+ column  Rendu
+ row  `-->`  `Reaction.NetForward`, une réaction nette dans le sens direct  `→`
+ row  `<=>`  `Reaction.BothDirections`, une réaction qui se déroule dans les deux sens  `⇄`
+ row  `<~>`  `Reaction.Equilibrium`, une réaction à l'équilibre  `⇋`
+ row  `<->`  `Reaction.Resonance`  `=`
+ row  `===`  `Reaction.Stoichiometric`  `↔`
```

- Which arrow a chemist writes carries information, and the equation keeps it.
+ La flèche qu'écrit un chimiste porte de l'information, et l'équation la conserve.

- ### Charge and state
+ ### Charge et état

- An ion has its charge as part of the molecule. A charged species is therefore a distinct
- value from its neutral counterpart, and the two cannot be confused when balancing. A unit
- charge is a prefix `+` or `-`, and any other charge comes from `ion`:
+ Un ion porte sa charge comme partie de la molécule. Une espèce chargée est donc une valeur
+ distincte de son homologue neutre, et les deux ne peuvent pas être confondues lors de
+ l'équilibrage. Une charge unitaire est un préfixe `+` ou `-`, et toute autre charge provient
+ d'`ion` :

```scala fuel
run
##
Out.println((+Na).show)
Out.println((-(C[2]*H[3]*O[2])).show)
Out.println(Fe.ion(3).show)
```

- These are the sodium cation, acetate, and iron(III). Charges add as species combine, so
- `+Na*(-Cl)` is neutral. That is what makes an ionic compound come out right, without its
- charge being stated separately.
+ Ce sont le cation sodium, l'acétate, et le fer(III). Les charges s'additionnent quand les
+ espèces se combinent : `+Na*(-Cl)` est donc neutre. C'est ce qui permet à un composé ionique
+ d'être correct, sans que sa charge soit indiquée séparément.

- A molecule can also note its physical state. There are four states, each written as a
- suffix:
+ Une molécule peut aussi noter son état physique. Il y a quatre états, chacun écrit en suffixe :

```tabular
- column  State
- column  Case
- column  Renders as
- row  solid  `PhysicalState.Solid`  `(s)`
- row  liquid  `PhysicalState.Liquid`  `(l)`
- row  gas  `PhysicalState.Gas`  `(g)`
- row  aqueous  `PhysicalState.Aqueous`  `(aq)`
+ column  État
+ column  Cas
+ column  Rendu
+ row  solide  `PhysicalState.Solid`  `(s)`
+ row  liquide  `PhysicalState.Liquid`  `(l)`
+ row  gaz  `PhysicalState.Gas`  `(g)`
+ row  aqueux  `PhysicalState.Aqueous`  `(aq)`
```

- Physical state is an annotation, not part of identity. It affects how a species is written,
- not what it is, so `H₂O(aq)` and `H₂O(l)` describe the same substance in different
- conditions. `inState` applies a state, and the parentheses matter:
+ L'état physique est une annotation, et ne fait pas partie de l'identité. Il influe sur la façon
+ dont une espèce s'écrit, non sur ce qu'elle est : `H₂O(aq)` et `H₂O(l)` décrivent donc la même
+ substance dans des conditions différentes. `inState` applique un état, et les parenthèses
+ comptent :

```scala fuel
run
##
Out.println(water.inState(PhysicalState.Aqueous).show)
Out.println((Na*Cl).inState(PhysicalState.Aqueous).show)
Out.println((Na*Cl.inState(PhysicalState.Aqueous)).show)
```

- Combining two molecules leaves the state of the result unset, because the state of a
- combination cannot be inferred from the states of its parts. A state which is applied before
- a bond is formed is therefore discarded by the bond, as the last line shows, and a state
- must be applied to the finished species.
+ Combiner deux molécules laisse l'état du résultat non défini, car l'état d'une combinaison ne
+ peut pas se déduire des états de ses parties. Un état appliqué avant la formation d'une liaison
+ est donc écarté par la liaison, comme le montre la dernière ligne, et un état doit être appliqué
+ à l'espèce achevée.

- ### Building blocks
+ ### Briques de base

- `Molecular` and `Formulable` are the two [typeclasses][typeclass] which the operators are
- defined over:
+ `Molecular` et `Formulable` sont les deux [classes de types][classe de types] sur lesquelles sont
+ définis les opérateurs :

```tabular
- column  Operation
- column  Defined on
- column  Yields
- row  `a*b`  `Molecular`  a molecule bonding the atoms of both
- row  `a*n`  `Molecular`  a formula term with coefficient `n`
- row  `a**n`  `Molecular`  a molecule with every atom and the charge multiplied by `n`
- row  `+a`, `-a`  `Molecular`  the molecule with its charge raised or lowered by one
- row  `a.ion(n)`  `Molecular`  the molecule with charge `n`
- row  `a.inState(state)`  `Molecular`  the molecule in a physical state
- row  `a + b`  `Formulable`  a formula holding both
- row  `a --> b`, and the other arrows  `Formulable`  an equation
+ column  Opération
+ column  Définie sur
+ column  Donne
+ row  `a*b`  `Molecular`  une molécule qui lie les atomes des deux
+ row  `a*n`  `Molecular`  un terme de formule de coefficient `n`
+ row  `a**n`  `Molecular`  une molécule dont chaque atome et la charge sont multipliés par `n`
+ row  `+a`, `-a`  `Molecular`  la molécule avec sa charge augmentée ou diminuée de un
+ row  `a.ion(n)`  `Molecular`  la molécule de charge `n`
+ row  `a.inState(state)`  `Molecular`  la molécule dans un état physique
+ row  `a + b`  `Formulable`  une formule qui contient les deux
+ row  `a --> b`, et les autres flèches  `Formulable`  une équation
```

- An element is molecular, a molecule is molecular, and both are formulable. That is why an
- element, a molecule and a full formula can all appear on either side of an equation, without
- conversion.
+ Un élément est moléculaire, une molécule est moléculaire, et les deux sont formulables. C'est
+ pourquoi un élément, une molécule et une formule complète peuvent tous apparaître dans l'un ou
+ l'autre membre d'une équation, sans conversion.

- ### Related
+ ### Voir aussi

- - [Quantities](quantities.md) describes the units, such as moles and grams, which chemical
-   amounts are measured in.
- - [Mathematical markup](mathematical-markup.md) typesets formulas of another kind.
- - [Numbers](numbers.md#overloading-the-operators) explains how `*` and `+` are defined for
-   types which are not numbers.
+ - [Grandeurs](quantities.md) décrit les unités, comme les moles et les grammes, dans lesquelles
+   se mesurent les quantités chimiques.
+ - [Balisage mathématique](mathematical-markup.md) compose des formules d'une autre sorte.
+ - [Nombres](numbers.md#overloading-the-operators) explique comment `*` et `+` sont définis pour
+   des types qui ne sont pas des nombres.
