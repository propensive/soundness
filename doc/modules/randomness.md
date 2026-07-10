## Randomness

### About

Random values of any type — not just numbers — come from a `Randomizable` typeclass, derived for
case classes and enumerations from their structure, so a whole tree of test data is one call. The
*source* of randomness and the *shape* of the numbers are separate, explicit choices: the source —
seeded and reproducible, or cryptographically secure — is a given in scope, and the distribution a
`Double` is drawn from — uniform, Gaussian, gamma — is another.

### On randomness

Randomness hides assumptions. `math.random()` answers only one narrow question — a uniform `Double`
— and everything else is assembled by hand; the seed is global and implicit, so a failing test that
depended on chance cannot be replayed; and whether the generator is predictable, which is harmless
in a simulation and fatal in a token generator, is nowhere visible in the code.

Soundness separates the concerns and names each one. Where randomness is used is delimited by a
block; which generator supplies it — seeded for reproducibility, secure for secrets — is an import;
what distribution shapes a number is a given; and any type reachable from these gains a random
generator by derivation. Everything comes from the `soundness` package; a seeded source makes runs
reproducible:

```scala
import soundness.*
import randomization.seededRandomization

given Seed = Seed(1L)
```

### Generating values

`stochastic` opens a scope with a random source, within which `arbitrary` produces a value of any
type with a `Randomizable` instance. The primitives have instances, and a case class or enumeration
derives one from its members:

```scala
case class Point(x: Int, y: Int)

stochastic:
  arbitrary[Int]()     // a random Int
  arbitrary[Point]()   // a random Point, both fields random
```

Because the source is the seeded one, running again with the same seed produces the same values —
which is what replaying a failing property test requires.

### Distributions

A random `Double` needs a shape, supplied as a `Distribution` in scope. `Gaussian` draws from a
[normal distribution](https://en.wikipedia.org/wiki/Normal_distribution), `UniformDistribution`
from a flat range, and `Gamma` from a [gamma distribution](https://en.wikipedia.org/wiki/Gamma_distribution):

```scala
stochastic:
  given Distribution = Gaussian(0.0, 2.0)
  List.fill(10000)(arbitrary[Double]())   // mean ≈ 0, standard deviation ≈ 2
```

Making the distribution explicit prevents the classic mistake of treating a uniform draw as if it
were normal; a `Double` cannot be generated until the code says what kind of `Double` it means.

### Sources

The generator behind a scope is chosen by import. `seededRandomization` is reproducible from its
`Seed`; `unseededRandomization` varies run to run; and `secureUnseededRandomization` draws from the
operating system's [cryptographically secure](https://en.wikipedia.org/wiki/Cryptographically_secure_pseudorandom_number_generator)
source, for values an adversary must not predict. Simulation and secret-generation thereby cannot
be confused: the difference is written at the import.

### Collections and custom types

A random collection needs a size, chosen as a `RandomSize` given — `randomization.sizes.uniformUpto100`
bounds it at a hundred elements. A type whose values need more care than field-by-field generation
defines its own instance, or maps an existing one:

```scala
case class Latitude(degrees: Double)
given Latitude is Randomizable = summon[Double is Randomizable].map(d => Latitude(d % 90))
```

### Shuffling and coin-tosses

A `Random` in scope also shuffles a sequence and answers a fair coin-toss, the small conveniences
that otherwise get hand-rolled badly:

```scala
stochastic:
  random.shuffle(List(1, 2, 3, 4, 5))
  toss()   // true or false, evenly
```
