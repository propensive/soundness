# One name for every random value

There is no `nextInt`, `nextBoolean` or `nextGaussian`. A random value is `arbitrary`, with
the type which is wanted, and a type which has no way to be random is found as the code
compiles.

```scala fuel
compile
frame
  label A number
  code
      import randomization.seededRandomization, randomSizes.uniformSizeUpto10

      given seed: Seed = Seed(1L)

      val number = stochastic(arbitrary[Int]())
frame
  label A coin
  highlight Boolean  the type is the only change
  code
      import randomization.seededRandomization, randomSizes.uniformSizeUpto10

      given seed: Seed = Seed(1L)

      val number = stochastic(arbitrary[Int]())
      val coin = stochastic(arbitrary[Boolean]())
frame
  label A list, of a random length
  highlight List[Char]  its length is drawn too
  code
      import randomization.seededRandomization, randomSizes.uniformSizeUpto10

      given seed: Seed = Seed(1L)

      val number = stochastic(arbitrary[Int]())
      val coin = stochastic(arbitrary[Boolean]())
      val letters = stochastic(arbitrary[List[Char]]())
frame
  label A type which nothing knows how to make
  errors show
  code
      import randomization.seededRandomization, randomSizes.uniformSizeUpto10

      given seed: Seed = Seed(1L)

      class Ghost()

      val ghost = stochastic(arbitrary[Ghost]())
##
```

[Generating values](../../soundness/modules/randomness.md#generating-values) has the types
which are provided, and [small APIs](../../philosophy/small-apis.md) the reason for one name.
