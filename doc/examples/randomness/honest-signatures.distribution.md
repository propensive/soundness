# A random number has a distribution

A random `Int` is any `Int`, but there is no such thing as any `Double`: a draw is uniform, or
normal, or something else. `arbitrary[Double]` asks its context for a `Distribution`, and does
not compile until one has been named.

```scala fuel
compile
frame
  label A draw of no particular shape
  errors show
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1L)

      val height: Double = stochastic(arbitrary[Double]())
frame
  label A normal distribution, around 170
  highlight Gaussian(170.0..8.0)  the mean and the standard deviation
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1L)
      given distribution: Distribution = Gaussian(170.0, 8.0)

      val height: Double = stochastic(arbitrary[Double]())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(height.inspect)
```

[Distributions](../../soundness/modules/randomness.md#distributions) lists the shapes which are
provided.
