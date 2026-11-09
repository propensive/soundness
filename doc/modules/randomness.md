## Randomness

### About

Random values of any type, not only numbers, come from the `Randomizable` [typeclass]. An
instance is [derived][derivation] for a case class or an enumeration from its structure, so a
whole tree of test data is one call.

The *source* of the randomness and the *shape* of the numbers are separate choices, and both
are explicit. The source is a [given] in scope, and is either seeded and reproducible, or
cryptographically secure. The distribution which a `Double` is drawn from is another given:
uniform, Gaussian or gamma.

### On randomness

Randomness hides assumptions:

- `math.random()` answers only one narrow question, a uniform `Double`, and everything else is
  assembled by hand.
- The seed is global and implicit, so a failing test which depended on chance cannot be
  replayed.
- Whether the generator is predictable is nowhere visible in the code. That is harmless in a
  simulation, and fatal in a generator of tokens.

Here the concerns are separate, and each has a name:

```tabular
column  Concern
column  Stated by
row  where randomness is used  a `stochastic` block
row  which generator supplies it  an import from `randomization`
row  what distribution shapes a number  a `Distribution` given
row  how a value of some type is generated  a `Randomizable` instance, usually derived
```

A source of randomness which is a [capability], not a global, is
[declarative context](../philosophy/declarative-context.md): reproducibility follows from the
scope, not from discipline.

Everything comes from the `soundness` package. A seeded source makes every run produce the same
values, and the last import lets the samples print their results:

```scala
import soundness.*
import randomization.seededRandomization
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given seed: Seed = Seed(1L)
```

### Generating values

`stochastic` opens a scope which has a source of randomness. Within it, `arbitrary` produces a
value of any type with a `Randomizable` instance. The primitive types have instances, and a
case class or an enumeration derives one from its members:

```scala
case class Point(x: Int, y: Int)
```

```scala fuel
run
##
stochastic:
  Out.println(arbitrary[Int]().show)
  Out.println(arbitrary[Point]().inspect)
```

The source is the seeded one, so running the code again with the same seed produces the same
values. That is what replaying a failing property test requires.

Outside a `stochastic` block there is no source, and no random value can be asked for:

```scala fuel
compile
errors show
##
arbitrary[Int]()
```

### Distributions

A random `Double` needs a shape, which a `Distribution` in scope supplies. Until the code says
which kind of `Double` it means, one cannot be generated, which prevents the classic mistake of
treating a uniform draw as though it were normal.

```tabular
column  Distribution
column  Parameters
column  Draws from
row  `Gaussian`  `mean`, defaulting to `0.0`, and `std`, to `1.0`  a [normal distribution](https://en.wikipedia.org/wiki/Normal_distribution)
row  `UniformDistribution`  `start` and `end`  a flat range
row  `Gamma`  `shape` and `scale`  a [gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution)
```

Common choices are also givens, to import from the package `randomDistributions`:

```tabular
column  Given
column  Draws from
row  `gaussianDistribution`  the standard normal distribution
row  `uniformUnitIntervalDistribution`  a flat range from 0 to 1
row  `uniformSymmetricUnitIntervalDistribution`  a flat range from -1 to 1
row  `binaryDistribution`  a random `Long`, converted to a `Double`
```

Ten thousand draws from a Gaussian with a standard deviation of 2 have a mean close to 0:

```scala
val draws = stochastic:
  given distribution: Distribution = Gaussian(0.0, 2.0)
  List.fill(10000)(arbitrary[Double]())
```

```scala fuel
run
##
Out.println((draws.fold(0.0)(_ + _)/10000).inspect)
```

[Mathematics](mathematics.md) has the statistics to describe such a sample.

### Sources

The generator behind a scope is chosen by an import from the package `randomization`. The
secure generators draw from the JDK's
[cryptographically secure](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
source, for values which an adversary must not predict:

```tabular
column  Import
column  Generator
column  Reproducible
row  `seededRandomization`  ordinary, from the `Seed` in scope  yes
row  `unseededRandomization`  ordinary  no, it varies run to run
row  `secureUnseededRandomization`  secure  no
row  `stronglySecureRandomization`  the platform's strongest secure source  no
row
  cell  `secureSeededRandomization`
  cell  secure, seeded from the `Seed` in scope
  cell
```

Simulation and the generation of secrets therefore cannot be confused: the difference is
written at the import. [Cryptography](cryptography.md) generates keys from the secure sources.

### Collections and custom types

A random collection needs a size, which is chosen by a `Random.Size` given.
`randomSizes.uniformSizeUpto100` bounds it at a hundred elements.

A type whose values need more care than generating each field defines an instance of its own,
or maps an existing one:

```scala
import randomDistributions.uniformUnitIntervalDistribution

case class Latitude(degrees: Double)

given latitudeRandomizable: (Latitude is Randomizable) =
  summon[Double is Randomizable].map(d => Latitude(d*180 - 90))
```

```scala fuel
run
##
stochastic:
  Out.println(arbitrary[Latitude]().inspect)
```

Where generating each field *is* right, the instance is derived from the type. A case class of
randomizable fields is randomizable, and a sealed hierarchy picks among its variants. That is
what property-based [testing](testing.md) needs, to generate values of an arbitrary type.

### Reproducing a run

A failing randomized test is worth nothing if it cannot be run again. Under
`seededRandomization`, the whole scope is a function of its `Seed`. Recording the seed of a
failing run, and supplying it again, reproduces exactly the same values. That includes the
sizes of generated collections, which are drawn from the same generator:

```scala
import randomSizes.uniformSizeUpto100

def sample(): List[Int] =
  given seed: Seed = Seed(42L)

  stochastic:
    arbitrary[List[Int]]().excerpt(0, 3)
```

```scala fuel
run
##
Out.println(sample().inspect)
Out.println(sample().inspect)
```

Nesting is deterministic too. A scope's generator is derived from that of the scope enclosing
it, so restoring the outer seed restores the inner sequence.

What is determined is the *sequence of draws*, and that is only as reproducible as the code
which makes them. Single-threaded code draws in a fixed order, and reproduces exactly.
[Concurrent](concurrency.md) code which draws from one scope does not. Its threads interleave
differently on each run, so the same seed hands different values to different threads. Where a
concurrent computation must be reproducible, each strand needs a scope of its own, seeded
deterministically.

### Shuffling and coin-tosses

A `Random` in scope also shuffles a sequence, and answers a fair coin-toss. These are small
conveniences which otherwise get written by hand, badly:

```scala fuel
run
##
stochastic: (random: Random) ?=>
  Out.println(random.shuffle(List(1, 2, 3, 4, 5)).inspect)
  Out.println(toss().inspect)
```

### Related

- [UUIDs](uuid.md) and [label placement](label-placement.md) draw from a seeded source of randomness.
