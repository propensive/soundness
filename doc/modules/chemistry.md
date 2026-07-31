## Chemistry

### About

The [periodic table](https://en.wikipedia.org/wiki/Periodic_table), chemical formulas, and chemical
equations are modelled as values. All 118 elements are available by symbol; elements combine into
molecules and molecules into formulas with ordinary operators; and an equation joins two formulas
with a typed reaction arrow, and can say whether it balances.

### On chemical notation

Chemical notation is a small formal language: a molecule is a multiset of atoms with a possible
charge, a formula is a sum of molecules with coefficients, and an equation asserts a relationship —
directed, reversible, or in equilibrium — between two formulas. Written as strings, none of that
structure is available: `2H₂ + O₂` is just characters, and whether an equation balances is not a
question a string can answer.

Soundness builds the structure from typed parts, so formulas are assembled by operators rather than
parsed from text, rendered with correct subscripts and superscripts, and interrogated — an
equation's two sides can be compared atom by atom. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Elements

Every element is a value on `PeriodicTable`, carrying its atomic number, symbol and name; elements
are also reachable by number or symbol at runtime:

```scala
import PeriodicTable.{H, O, C}

H.number             // 1
PeriodicTable(6)     // carbon, as an Optional
```

### Molecules

Atoms bond into a molecule with `*`, and a repeated atom takes a count as a type argument; a
molecule renders with proper subscripts:

```scala
val water = H[2]*O
water.show   // t"H₂O"
```

An ion carries its charge, and a molecule can note its physical state — solid, liquid, gas or
aqueous — for rendering:

```scala
water.inState(PhysicalState.Aqueous)   // shows as H₂O(aq)
```

### Formulas and equations

A molecule with a coefficient is a formula term, terms add with `+`, and two formulas join into an
equation with a reaction arrow — `-->` for a net forward reaction, `<=>` for both directions, `<~>`
for an equilibrium:

```scala
val combustion = H[2]*O*2 --> (H[2]*2 + O[2])
```

An equation knows whether its two sides contain the same atoms:

```scala
(H[2]*2 + O[2] --> H[2]*O*2).balanced   // true
```

Since each side is a typed multiset of atoms rather than text, balance is a real check, computed
from the structure. `atoms` gives that multiset for either side, so a formula reports its own
composition — the element counts across every molecule and coefficient it contains — which is what
computing a molar mass or checking a proposed reaction needs.

### The reaction arrows

Five arrows are available, and they mean different things rather than being stylistic variants:
`-->` for a net forward reaction, `<->` for a reversible one, `<=>` for a reaction proceeding in
both directions, `<~>` for one at equilibrium, and `===` for a statement of equality between two
formulas. Which arrow a chemist writes carries information, and the equation keeps it.

### Charge and state

An ion carries its charge as part of the molecule, so a charged species is a distinct value from
its neutral counterpart and the two cannot be confused when balancing. Physical state is
annotation rather than identity — it affects how a species renders, not what it is — so
`H₂O(aq)` and `H₂O(l)` describe the same substance in different conditions.

### Building blocks

`Molecular` and `Formulable` are the two typeclasses the operators are defined over: anything
molecular can be bonded with `*` and multiplied by a coefficient, and anything formulable can be
added with `+` and joined into an equation. An element is molecular, a molecule is molecular, and
both are formulable, which is why an element, a molecule and a full formula can all appear on
either side of an equation without conversion.
