# The variance of seconds is in seconds squared

A list of quantities reduces to a quantity, and each reduction knows what it does to the
units. The mean of some times is a time. Their variance is not, and its type says so.

```scala fuel
compile
frame
  label Three laps, and their mean
  highlight Quantity[Seconds[1]]  a time
    line 3
  code
      val laps = List(61*Second, 59*Second, 63*Second)

      val mean: Optional[Quantity[Seconds[1]]] = laps.mean
frame
  label Their variance, as though it were a time
  errors show
  code
      val laps = List(61*Second, 59*Second, 63*Second)

      val spread: Optional[Quantity[Seconds[1]]] = laps.variance
frame
  label Their variance, in seconds squared
  highlight Seconds[2]  the unit, squared
  code
      val laps = List(61*Second, 59*Second, 63*Second)

      val spread: Optional[Quantity[Seconds[2]]] = laps.variance
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(spread.let(_.show).or(t"none"))
Out.println(laps.std.let(_.show).or(t"none"))
```

[Aggregating](../../soundness/modules/quantities.md#aggregating) lists the reductions.
