## Chemistry

### About

The [periodic table](https://en.wikipedia.org/wiki/Periodic_table), chemical formulas and
chemical equations are modeled as values. All 118 elements are available by their symbols.
Elements combine into molecules, and molecules into formulas, with ordinary operators. An
equation joins two formulas with a typed reaction arrow, and can say whether it balances.

### On chemical notation

Chemical notation is a small formal language. A molecule is a collection of atoms, in which
an atom may appear more than once, with a possible charge. A formula is a sum of molecules
with coefficients. An equation asserts a relationship between two formulas, which is
directed, reversible, or in equilibrium.

Written as strings, none of that structure is available. `2H₂ + O₂` is just characters, and
whether an equation balances is not a question which a string can answer.

Here the structure is built from typed parts. Formulas are assembled by operators, not
parsed from text. They are written out with correct subscripts and superscripts. And they
can be interrogated: the two sides of an equation are compared atom by atom.

Everything comes from the `soundness` package. The second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Elements

Every element is a value on `PeriodicTable`, with its atomic number, its symbol and its
name. Elements can also be reached by number, or by symbol, at [runtime]:

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

Looking an element up by number gives an [optional](optional-values.md) value, since a
number may have no element.

### Molecules

Atoms are bonded into a molecule with `*`, and an atom which is repeated takes its number as
a type argument. A molecule is written out with proper subscripts:

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

### Formulas and equations

A molecule with a coefficient is a term of a formula. Terms are added with `+`, and two
formulas are joined into an equation with a reaction arrow:

```scala
val combustion = H[2]*2 + O[2] --> H[2]*O*2
```

```scala fuel
run
##
Out.println(combustion.show)
```

An equation knows whether its two sides contain the same atoms:

```scala fuel
run
##
Out.println(combustion.balanced.inspect)
Out.println((H[2] + O[2] --> H[2]*O).balanced.inspect)
```

Each side is a typed collection of atoms, not text, so balance is a real check, computed
from the structure. `atoms` gives that collection for either side. A formula thereby reports
its own composition, as the number of each element across every molecule and coefficient
which it contains. That is what computing a molar mass, or checking a proposed reaction,
needs.

### The reaction arrows

Five arrows are available. They mean different things, and are not variants of style:

```tabular
column  Arrow
column  Reaction
column  Renders as
row  `-->`  `Reaction.NetForward`, a net forward reaction  `→`
row  `<=>`  `Reaction.BothDirections`, a reaction proceeding in both directions  `⇄`
row  `<~>`  `Reaction.Equilibrium`, a reaction at equilibrium  `⇋`
row  `<->`  `Reaction.Resonance`  `=`
row  `===`  `Reaction.Stoichiometric`  `↔`
```

Which arrow a chemist writes carries information, and the equation keeps it.

### Charge and state

An ion has its charge as part of the molecule. A charged species is therefore a distinct
value from its neutral counterpart, and the two cannot be confused when balancing. A unit
charge is a prefix `+` or `-`, and any other charge comes from `ion`:

```scala fuel
run
##
Out.println((+Na).show)
Out.println((-(C[2]*H[3]*O[2])).show)
Out.println(Fe.ion(3).show)
```

These are the sodium cation, acetate, and iron(III). Charges add as species combine, so
`+Na*(-Cl)` is neutral. That is what makes an ionic compound come out right, without its
charge being stated separately.

A molecule can also note its physical state. There are four states, each written as a
suffix:

```tabular
column  State
column  Case
column  Renders as
row  solid  `PhysicalState.Solid`  `(s)`
row  liquid  `PhysicalState.Liquid`  `(l)`
row  gas  `PhysicalState.Gas`  `(g)`
row  aqueous  `PhysicalState.Aqueous`  `(aq)`
```

Physical state is an annotation, not part of identity. It affects how a species is written,
not what it is, so `H₂O(aq)` and `H₂O(l)` describe the same substance in different
conditions. `inState` applies a state, and the parentheses matter:

```scala fuel
run
##
Out.println(water.inState(PhysicalState.Aqueous).show)
Out.println((Na*Cl).inState(PhysicalState.Aqueous).show)
Out.println((Na*Cl.inState(PhysicalState.Aqueous)).show)
```

Combining two molecules leaves the state of the result unset, because the state of a
combination cannot be inferred from the states of its parts. A state which is applied before
a bond is formed is therefore discarded by the bond, as the last line shows, and a state
must be applied to the finished species.

### Building blocks

`Molecular` and `Formulable` are the two [typeclasses][typeclass] which the operators are
defined over:

```tabular
column  Operation
column  Defined on
column  Yields
row  `a*b`  `Molecular`  a molecule bonding the atoms of both
row  `a*n`  `Molecular`  a formula term with coefficient `n`
row  `a**n`  `Molecular`  a molecule with every atom and the charge multiplied by `n`
row  `+a`, `-a`  `Molecular`  the molecule with its charge raised or lowered by one
row  `a.ion(n)`  `Molecular`  the molecule with charge `n`
row  `a.inState(state)`  `Molecular`  the molecule in a physical state
row  `a + b`  `Formulable`  a formula holding both
row  `a --> b`, and the other arrows  `Formulable`  an equation
```

An element is molecular, a molecule is molecular, and both are formulable. That is why an
element, a molecule and a full formula can all appear on either side of an equation, without
conversion.

### Related

- [Quantities](quantities.md) describes the units, such as moles and grams, which chemical
  amounts are measured in.
- [Mathematical markup](mathematical-markup.md) typesets formulas of another kind.
- [Numbers](numbers.md#overloading-the-operators) explains how `*` and `+` are defined for
  types which are not numbers.
