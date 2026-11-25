# Randomness has a source

A random value comes from a source of randomness, and `arbitrary` takes one from its context.
A `stochastic` block supplies it, so where a program is random, and where it is not, can be
read from its structure.

```scala fuel
compile
frame
  label No source of randomness
  errors show
  code
      val roll = arbitrary[Int]()
frame
  label A source, for the length of a block
  focus stochastic:
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1L)

      val roll = stochastic:
        arbitrary[Int]()
##
```

The source here is seeded, so every run gives the same number.
[Randomness](../../soundness/modules/randomness.md#generating-values) has the other sources.
