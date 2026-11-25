# One method converts to any unit

There is no `toMetres` and no `feetToInches`. `convert` is given the units which are wanted,
and the ratio between the two is found by the compiler.

```scala fuel
compile
frame
  label Feet, in meters
  highlight convert[Metres]  the units wanted, as a type
  code
      val stage = (43*Foot).convert[Metres]
frame
  label Meters, in feet
  highlight convert[Feet]  the same method, the other way
  code
      val stage = (43*Foot).convert[Metres]
      val pit = (24*Metre).convert[Feet]
##
```

[Converting](../../soundness/modules/quantities.md#converting) describes `normalize`, which
names the power as well.
