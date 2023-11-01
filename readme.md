[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/charisma/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/charisma/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Charisma

__Representations of chemicals__

__Charisma__ provides a simple representation of chemical elements and formulas in Scala.

## Features

- provides representations (with names) of the 118 chemical elements known since 2015
- lightweight syntax for constructing molecules from multiples of elements
- serialization to Unicode strings


## Availability

Charisma has not yet been published as a binary.

## Getting Started

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

The `ChemicalEquation#balance` method will determine if the equation is
balanced, that is, the number of atoms of each element is the same on the left
and right sides of the equation.

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

### Molecular properties

Molecular values may be ionized with a positive or negative integer charge. For
unit charges, a unary `+` or `-` may prefix a `Molecular` value, e.g. `+Na` or
`-C[2]*H[3]*O[2]` (acetate). The charges of ions or ionic compounds will be
added when combined, so `+Na*(-Cl)` (salt) will produce the chargeless
compound, NaCl.

A non-unit charge may be specified for any molecule with its `ion` method,
specifying its integral charge value.

Molecules in a chemical equation can also be identified as having a particular
physical state, namely solid, liquid, gas or aqueous. These four states are
represented by the enumeration, `PhysicalState` as `Solid`, `Liquid`, `Gas` and
`Aqueous`, and can be specified for any `Molecular` value with its `as` method.
For example, `(Na*Cl).as(Aqueous)` represents an aqueous salt solution.

By default, a molecule's state is unspecified, represented by the `Unset`
value, and any combination of molecules with physical states set (other than
integer multiplication) will unset their physical states, since the resultant
state cannot be easily and reliably predicted. Therefore, in the earlier
example, `(Na*Cl).as(Aqueous)`, the parentheses are important as the aqueous
state would otherwise be specified for Chlorine, but unset immediately upon
combining with Sodium.

### Rendering

`ChemicalElement`s, `Molecule`s, `ChemicalFormula`s and `ChemicalEquation`s all
have `Show` instances which will render the types as text, using appropriate
Unicode characters for subscripts.

Since the different elements in a molecule could be written in any order with
the same meaning, they are canonically written using the [Hill
System](https://en.wikipedia.org/wiki/Chemical_formula#Hill_system).




## Status

Charisma is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Charisma is designed to be _small_. Its entire source code currently consists
of 238 lines of code.

## Building

Charisma can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Charisma are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/charisma/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Charisma easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Charisma was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

One person's charisma may lead to figurative _chemistry_ with another.

### Pronunciation

`/kəˈɹɪzmə/`

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a deuterium atom, according to the [Bohr model](https://en.wikipedia.org/wiki/Bohr_model).

## License

Charisma is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
