# An ion carries its charge

A unit charge is a sign written before a species, as it is written after one on paper, and
charges add when species bond. A cation and an anion therefore make a neutral salt, with no
charge stated for it.

```scala fuel
compile
frame
  label A cation
  highlight +Na  one charge more
  code
      import PeriodicTable.{Na, Cl}

      val sodium = +Na
frame
  label An anion
  highlight -Cl  one charge fewer
  code
      import PeriodicTable.{Na, Cl}

      val sodium = +Na
      val chloride = -Cl
frame
  label A salt
  highlight sodium*chloride  the charges add to nothing
  code
      import PeriodicTable.{Na, Cl}

      val sodium = +Na
      val chloride = -Cl
      val salt = sodium*chloride
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(sodium.show)
Out.println(chloride.show)
Out.println(salt.show)
Out.println(salt.charge.show)
```

The salt has no charge, and is written in Hill order, in which the symbols of a compound
without carbon run alphabetically.
[Charge and state](../../soundness/modules/chemistry.md#charge-and-state) has charges
greater than one, and the physical states.
