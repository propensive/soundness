# A random list has a random length

A random `List` needs more than random elements: something must decide how many there are.
`arbitrary` asks its context for that too, as a `Random.Size`, and a list is not generated
until a bound on its length has been chosen.

```scala fuel
compile
frame
  label A list of no particular length
  errors show
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1L)

      val lots: List[Boolean] = stochastic(arbitrary[List[Boolean]]())
frame
  label At most ten elements
  highlight uniformSizeUpto10  the bound, chosen by name
  code
      import randomization.seededRandomization
      import randomSizes.uniformSizeUpto10

      given seed: Seed = Seed(1L)

      val lots: List[Boolean] = stochastic(arbitrary[List[Boolean]]())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lots.inspect)
```

[Collections and custom types](../../soundness/modules/randomness.md#collections-and-custom-types)
has the sizes which are provided.
