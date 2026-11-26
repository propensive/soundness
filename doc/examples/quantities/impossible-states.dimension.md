# A dimension of its own

A processor's speed in floating-point operations per second could be a bare rate, in inverse
seconds, and would then add to any frequency. Given a dimension of its own, it adds only to
another measurement of the same kind.

```scala fuel
compile
frame
  label A rate, which is a frequency like any other
  code
      val Flop: MetricUnit[Seconds[-1]] = MetricUnit(1)

      val nonsense = 3*Giga(Flop) + 50*Hertz
frame
  label A dimension, and its units
  errors show
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      val nonsense = 3*Giga(Flop) + 50*Hertz
frame
  label Two processors
  highlight 3*Giga(Flop)..500*Mega(Flop)  the same dimension
  code
      trait CpuPerformance extends Dimension
      trait Flops[power <: Nat] extends Units[power, CpuPerformance]

      val Flop: MetricUnit[Flops[1]] = MetricUnit(1)

      val cluster = 3*Giga(Flop) + 500*Mega(Flop)
##
```

[Defining a unit of your own](../../soundness/modules/quantities.md#defining-a-unit-of-your-own)
builds the unit up step by step.
