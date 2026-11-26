# A grid with honest gaps

A test over two axes fills a grid. Where a combination does not apply, the body simply has no
case for it: the body is a partial function, and the position it leaves out is recorded as
empty, and not as a failure or a vacuous pass.

```scala fuel
compile
frame
  label Every combination
  code
      object Tests extends Suite(m"Casting tests"):
        def run(): Unit =
          test(m"parts shared among players").over(Axis(t"parts")(6, 12), Axis(t"players")(2, 3)):
            case (parts, players) => parts/players
          . assert((parts, players, share) => share*players == parts)
frame
  label All but one
  highlight if..12  a combination which does not apply
  code
      object Tests extends Suite(m"Casting tests"):
        def run(): Unit =
          test(m"parts shared among players").over(Axis(t"parts")(6, 12), Axis(t"players")(2, 3)):
            case (parts, players) if parts != 12 => parts/players
          . assert((parts, players, share) => share*players == parts)
##
```

The example is compiled, and is not run, since a test is run by a test runner.
[Spreading a test over axes](../../soundness/modules/testing.md#spreading-a-test-over-axes)
shows how the grid is reported, and [total transitions](../../philosophy/total-transitions.md)
why a gap is better than a pretense.
