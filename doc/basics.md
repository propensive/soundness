### Chemical Entities

_Charisma_ provides representations of several increasingly-complex entities from chemistry,
- chemical elements
- molecules
- chemical formulas
- chemical equations
which can be constructed, usually by composing other entities.

#### Chemical Elements

The 118 chemical elements known, and assigned names since 2015 are all
represented in the `PeriodicTable` object, as members named after their
chemical symbol. For example, Hydrogen is `PeriodicTable.H` and Chlorine is
`PeriodicTable.Cl`. Each element is an instance of `ChemicalElement`, which
defines its atomic number, name in English (noting that "Sulphur" and
"Aluminium" are preferred over "Sulfur" and "Aluminum") and chemical symbol.

#### Molecules

Elements can be combined to produce molecules, instances of `Molecule`. To
combine multiple atoms of the same element into a molecule, we apply an integer
type parameter to that element, for example `PeriodicTable.O[2]` is `O₂` and
`PeriodicTable.C[60]` would be `C₆₀`.

Different elements can be combined using the `*` operator. So salt, `NaCl`,
would be `Na*Cl`. Or sulphuric acid, `H₂O₄S`, could be constructed as,
`H[2]*S*O[4]*S`.

#### Chemical Formulas

A `ChemicalFormula` is the addition of several molucules, combined in integer
multiples using the `*` operator, and with other molecules with the `+`
operator. For example, the products of photosynthesis, `C₆H₁₂O₆ + 6O₂` (sugar
and oxygen), could be written, `C[6]*H[12]*O[6] + O[2]*6`.

#### Chemical equation

A `ChemicalEquation` describes a relationship between two `ChemicalFormula`s,
and can be constructed using one the arrow operators between two chemical
formulas. These arrow operators represent different relationsips between the
sides of the equation, and are as follows:
- `-->`: net forwards
- `<->`: resonance
- `<=>`: both directions
- `<~>`: equilibrium
- `===`: stoichiometric

These different relationships are reprensented by the enumeration, `Reaction`.

No checking is currently done to ensure that the left and right sides of the
equation balance, but this may be added as a later feature.

### Generality

A `ChemicalElement` principally represents the _type_ of an atom and is not, in
general, interchangeable with a `Molecule`. But in many circumstances, it can
also serve to conveniently represent an atom of that element itself. So while a
`ChemicalElement` is _not_ a `Molecule`, both have the supertype, `Molecular`.

Likewise, `ChemicalElement`, `Molecule` and `ChemicalFormula` (as well as
`Molecular`) are all subtypes of a further generalization, `Formulable` for
types that can represent a chemical formula.

Generally, it is better to program to the `Molecular` and `Forbulable`
interfaces, rather than `Molecule` or `ChemicalFormula`, for the greatest
flexibility.

### Rendering

`ChemicalElement`s, `Molecule`s, `ChemicalFormula`s and `ChemicalEquation`s all
have `Show` instances which will render the types as text, using appropriate
Unicode characters for subscripts.

Since the different elements in a molecule could be written in any order with
the same meaning, they are canonically written using the [Hill
System](https://en.wikipedia.org/wiki/Chemical_formula#Hill_system).


