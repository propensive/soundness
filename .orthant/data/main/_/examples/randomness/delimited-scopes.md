# A scope which can be replayed

A `stochastic` block makes a new source of randomness when it starts, and the source is gone
when it ends. Under a seeded generator the whole block is therefore a function of its seed,
and running it again draws the same values in the same order.

```scala fuel
compile
frame
  label Three throws of a die
  code
      import randomization.seededRandomization

      def throws(): List[Int] =
        given seed: Seed = Seed(42L)

        List(1, 2, 3).map: turn =>
          1
frame
  label A source, for the length of the block
  highlight stochastic:  a new source, begun at the seed
  highlight arbitrary[Int]()  drawn from the block's source
  code
      import randomization.seededRandomization

      def throws(): List[Int] =
        given seed: Seed = Seed(42L)

        stochastic:
          List(1, 2, 3).map: turn =>
            1 + (arbitrary[Int]()%6).abs
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(throws().inspect)
Out.println(throws().inspect)
```

Each call opens a scope of its own, and each scope begins at the seed.
[Reproducing a run](../../soundness/modules/randomness.md#reproducing-a-run) says how far the
guarantee reaches, and [delimited scopes](../../philosophy/delimited-scopes.md) why a block
is the unit.
