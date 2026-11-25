# The shape of a random number

`arbitrary[Double]` does not say how its values are spread. A distribution in scope does, and
the common ones are chosen by an import, which decides for every draw beneath it.

```scala fuel
compile
alternative Normal
  highlight gaussianDistribution  mean 0, standard deviation 1
  code
      import randomization.seededRandomization
      import randomDistributions.gaussianDistribution

      given seed: Seed = Seed(1L)

      val noise: Double = stochastic(arbitrary[Double]())
alternative Uniform, from 0 to 1
  highlight uniformUnitIntervalDistribution  every value as likely as any other
  code
      import randomization.seededRandomization
      import randomDistributions.uniformUnitIntervalDistribution

      given seed: Seed = Seed(1L)

      val noise: Double = stochastic(arbitrary[Double]())
alternative Uniform, from -1 to 1
  highlight uniformSymmetricUnitIntervalDistribution  centered on zero
  code
      import randomization.seededRandomization
      import randomDistributions.uniformSymmetricUnitIntervalDistribution

      given seed: Seed = Seed(1L)

      val noise: Double = stochastic(arbitrary[Double]())
##
```

The three are compiled, and not run. [Distributions](../../soundness/modules/randomness.md#distributions)
has the table, and [declarative context](../../philosophy/declarative-context.md) the reason
the shape is not an argument.
