# An arrow which says what kind of reaction

A chemist's arrows mean different things, and each is an operator of its own. The equation
keeps the arrow it was written with, and shows it.

```scala fuel
compile
frame
  label A reaction which goes forward
  highlight -->  a net forward reaction
  code
      import PeriodicTable.{C, O, H, I, N}

      val burning = C + O[2] --> C*O[2]
frame
  label One which goes both ways
  highlight <=>  both directions
  code
      import PeriodicTable.{C, O, H, I, N}

      val burning = C + O[2] --> C*O[2]
      val iodide = H[2] + I[2] <=> H*I*2
frame
  label One at equilibrium
  highlight <~>  an equilibrium
  code
      import PeriodicTable.{C, O, H, I, N}

      val burning = C + O[2] --> C*O[2]
      val iodide = H[2] + I[2] <=> H*I*2
      val dimer = N[2]*O[4] <~> N*O[2]*2
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(burning.show)
Out.println(iodide.show)
Out.println(dimer.show)
```

[The reaction arrows](../../soundness/modules/chemistry.md#the-reaction-arrows) lists all
five.
