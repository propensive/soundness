# A number which stays in its range

A type written with `~` admits only the numbers between its bounds. A literal outside them is
not a value of the type, so the mistake is found where it is written.

```scala fuel
compile
frame
  label A volume above the maximum
  errors show
  code
      val volume: 0.0 ~ 1.0 = 1.5
frame
  label A volume within the range
  highlight 0.0..1.0  the range is the type
  code
      val volume: 0.0 ~ 1.0 = 0.5
##
```

[Bounded numbers](../../soundness/modules/numbers.md#bounded-numbers) shows how a range follows
a value through arithmetic.
