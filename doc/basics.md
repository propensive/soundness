All terms and types are defined in the `capricious` package:
```scala
import capricious.*
```

### Generating random values

A random instance of some type, `Type` can be constructed by calling,
```scala
val randomInt = random[Int]()
val randomBoolean = random[Boolean]()
```
or, if the type can be inferred from the context, just,
```scala
val randomChar: Char = random()
```

Random values can only be generated for certain types, but this includes most primitive types, and any type for
which a `Randomizable` typeclass instance exists.

### Generating random `Double`s

Random `Double`s can be generated only if a probability distribution is specified. Since `Double`s are
a 64-bit approximation of the set of real numbers, which is an infinite set, there is no clear answer
for what probability each possible `Double` value should have of being chosen randomly. Hence, several
options are provided, which can be selected by importing them as contextual values:

- `import randomDistributions.gaussian` - the Gaussian distribution with mean, `0`, and variance, `1`
- `import randomDistributions.uniformUnitInterval` - uniform across the interval `[0, 1]`
- `import randomDistributions.uniformSymmetricUnitInterval` - uniform across the interval `[-1, 1]`
- `import randomDistributions.binary` - uniform across the 64-bit binary representations of IEEE 754
  double-precision values
- `given Distribution = Gamma(shape, scale)` - a Gamma distribution with a specified shape (k) and
  scale (θ)
- `given Distribution = Gaussian(mean, standardDeviation)` - a Gaussian (normal) distribution with
  specified mean (x̄) and standard deviation (σ)
- `given Distribution = UniformDistribution(start, end)` - a uniform distribution in the range `[start, end]`

### Random sources

Several pseudorandom number generators are available, sometimes in seeded and unseeded variants:

- `import randomNumberGenerators.unseeded` - a "standard" generator, with no seed
- `import randomNumberGenerators.seeded` - a "standard" generator, requiring a contextual `Seed` instance
- `import randomNumberGenerators.secureUnseeded` - a "secure" generator, with no seed
- `import randomNumberGenerators.secureSeeded` - a "secure" generator, requiring a contextual `Seed` instance
- `import randomNumberGenerators.stronglySecure` - a "strongly secure" generator, which cannot be seeded

Those generators which require a seed value can define it, as a `Long` value, with:
```scala
given Seed(23956242374982L)
```