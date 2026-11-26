# One arrow for elements, molecules and formulas

An element is a molecule of one atom, and a molecule is a formula of one term. The same `+`
and the same arrow therefore accept any of the three, on either side, and nothing is converted
by hand.

```scala fuel
compile
frame
  label Two elements make a molecule
  highlight Fe*S  a molecule, from two elements
  code
      import PeriodicTable.{Fe, S, C, O}

      val pyrrhotite = Fe + S --> Fe*S
frame
  label An element and a molecule
  highlight O[2]  a molecule beside an element
  code
      import PeriodicTable.{Fe, S, C, O}

      val pyrrhotite = Fe + S --> Fe*S
      val burning = C + O[2] --> C*O[2]
frame
  label Terms with coefficients
  highlight Fe*4  a term of a formula
  code
      import PeriodicTable.{Fe, S, C, O}

      val pyrrhotite = Fe + S --> Fe*S
      val burning = C + O[2] --> C*O[2]
      val rusting = Fe*4 + O[2]*3 --> Fe[2]*O[3]*2
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(pyrrhotite.show)
Out.println(burning.show)
Out.println(rusting.show)
Out.println(rusting.balanced.inspect)
```

[Building blocks](../../soundness/modules/chemistry.md#building-blocks) lists the operators
and what each is defined on.
