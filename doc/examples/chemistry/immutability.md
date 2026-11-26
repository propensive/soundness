# A molecule in another state

A molecule is a value, and noting its physical state gives a new molecule. The water it was
made from is water still, in no state in particular.

```scala fuel
compile
frame
  label Water
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
frame
  label Steam
  highlight inState  gives a new molecule
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
      val steam = water.inState(PhysicalState.Gas)
frame
  label Ice, from the same water
  highlight water.inState(PhysicalState.Solid)  water has not changed
  code
      import PeriodicTable.{H, O}

      val water = H[2]*O
      val steam = water.inState(PhysicalState.Gas)
      val ice = water.inState(PhysicalState.Solid)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(water.show)
Out.println(steam.show)
Out.println(ice.show)
```

[Charge and state](../../soundness/modules/chemistry.md#charge-and-state) has the four states,
and [immutability](../../philosophy/immutability.md) the principle.
