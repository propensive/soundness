# Units computed by the compiler

The type of a quantity lists its units, each with a power. Multiplication adds the powers and
division subtracts them, so the compiler works out the units of a result and checks them
against the type which was declared.

```scala fuel
compile
frame
  label A speed is meters to the first, seconds to the minus first
  highlight Metres[1]..Seconds[-1]  worked out from the division
    line 4
  code
      val distance: Quantity[Metres[1]] = 100*Metre
      val time: Quantity[Seconds[1]] = 9.58*Second

      val speed: Quantity[Metres[1] & Seconds[-1]] = distance/time
frame
  label An acceleration is not a speed
  errors show
  code
      val distance: Quantity[Metres[1]] = 100*Metre
      val time: Quantity[Seconds[1]] = 9.58*Second

      val speed: Quantity[Metres[1] & Seconds[-1]] = distance/time/time
frame
  label It has seconds to the minus second
  highlight Seconds[-2]  one more division by time
  code
      val distance: Quantity[Metres[1]] = 100*Metre
      val time: Quantity[Seconds[1]] = 9.58*Second

      val acceleration: Quantity[Metres[1] & Seconds[-2]] = distance/time/time
##
```

[Quantities and units](../../soundness/modules/quantities.md#quantities-and-units) explains the
powers.
