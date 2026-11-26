# The generator is an import

A `stochastic` block does not say where its randomness comes from. An import does, and the
difference between a simulation which can be replayed and a secret which cannot be predicted
is one word at the top of the scope.

```scala fuel
compile
alternative Reproducible
  highlight seededRandomization  the same sequence for the same seed
  code
      import randomization.seededRandomization

      given seed: Seed = Seed(1606L)

      val token: Long = stochastic(arbitrary[Long]())
alternative Different each run
  highlight unseededRandomization  an ordinary generator
  code
      import randomization.unseededRandomization

      val token: Long = stochastic(arbitrary[Long]())
alternative Unpredictable
  highlight secureUnseededRandomization  the secure source of the JDK
  code
      import randomization.secureUnseededRandomization

      val token: Long = stochastic(arbitrary[Long]())
##
```

Only the first of these gives the same number twice, so the example is compiled, and is not
run. [Sources](../../soundness/modules/randomness.md#sources) lists the generators, and
[declarative context](../../philosophy/declarative-context.md) explains why the choice is not
an argument.
