# The mean of no measurements

A list of quantities has a total even when it is empty: zero. It has no mean. `mean` returns an
optional quantity, and the code which uses it says what an absent one becomes.

```scala fuel
compile
frame
  label A mean, which may be absent
  highlight Optional[Quantity[Seconds[1]]]  `Unset` for an empty list
  code
      def pace(laps: List[Quantity[Seconds[1]]]): Optional[Quantity[Seconds[1]]] =
        laps.mean
frame
  label Used as though it were present
  errors show
  code
      def pace(laps: List[Quantity[Seconds[1]]]): Quantity[Seconds[1]] =
        val mean: Optional[Quantity[Seconds[1]]] = laps.mean
        mean
frame
  label With a value for absence
  highlight or(0*Second)  what absence means here
  code
      def pace(laps: List[Quantity[Seconds[1]]]): Quantity[Seconds[1]] =
        val mean: Optional[Quantity[Seconds[1]]] = laps.mean
        mean.or(0*Second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(pace(List(61*Second, 59*Second, 63*Second)).show)
Out.println(pace(List()).show)
```

[Aggregating](../../soundness/modules/quantities.md#aggregating) lists the reductions, and
[optionality](../../philosophy/optionality.md) explains `or`.
