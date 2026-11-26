# An equation, written as a chemist writes it

Atoms are bonded with `*`, a repeated atom takes its count in brackets, terms are added with
`+`, and an arrow joins the two sides. The expression is ordinary Scala, and its value knows
whether it balances.

```scala fuel
compile
frame
  label A molecule
  highlight H[2]*O  two atoms of hydrogen, bonded to one of oxygen
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
frame
  label An equation
  highlight -->  a net forward reaction
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
      val combustion = H[2] + O[2] --> water
frame
  label With coefficients, so that it balances
  highlight *2  two molecules of hydrogen
  highlight *2  two molecules of water
    occurrence 2
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
      val combustion = H[2]*2 + O[2] --> water*2
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(combustion.show)
Out.println(combustion.balanced.inspect)
Out.println((H[2] + O[2] --> water).balanced.inspect)
```

The last line is the equation of the second step, which has two atoms of oxygen on the left
and one on the right.
[Formulas and equations](../../soundness/modules/chemistry.md#formulas-and-equations) has the
rest, and [elegant prose](../../philosophy/elegant-prose.md) the case for notation like this.
