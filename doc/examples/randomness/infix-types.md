# A generator for a type of one's own

Where a random value of each field is not what is wanted, a type is given a generator in one
sentence: `Latitude is Randomizable`. The instance is made from another by `map`, and
`arbitrary` finds it by its type.

```scala fuel
compile
frame
  label A latitude, as any `Double` at all
  code
      import randomization.seededRandomization
      import randomDistributions.uniformUnitIntervalDistribution

      given seed: Seed = Seed(1L)

      case class Latitude(degrees: Double)

      val landfall = stochastic(arbitrary[Latitude]())
frame
  label A latitude, between the poles
  highlight Latitude..Randomizable  read as a sentence
    line 8
  highlight map  from 0 to 1, stretched over 180 degrees
  code
      import randomization.seededRandomization
      import randomDistributions.uniformUnitIntervalDistribution

      given seed: Seed = Seed(1L)

      case class Latitude(degrees: Double)

      given randomizable: (Latitude is Randomizable) =
        summon[Double is Randomizable].map(unit => Latitude(unit*180 - 90))

      val landfall = stochastic(arbitrary[Latitude]())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(landfall.inspect)
```

[Collections and custom types](../../soundness/modules/randomness.md#collections-and-custom-types)
has the original, and [infix types](../../philosophy/infix-types.md) explains how such a type
is read.
