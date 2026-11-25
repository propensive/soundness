# An enumeration is an axis

A test spread over an axis runs once for each value, and each verdict is recorded under that
value. The values can be listed, but for an enumeration there is nothing to list: its
companion is the axis, and a case added to it is a case tested.

```scala fuel
compile
frame
  label Values, listed
  highlight Axis(t"acts")(3,..5)
  code
      object Tests extends Suite(m"Play tests"):
        def run(): Unit =
          test(m"a play has an interval").over(Axis(t"acts")(3, 5)): acts =>
            acts/2
          . assert((acts, interval) => interval > 0)
frame
  label Every case of an enumeration
  highlight over(Genre)  the companion, with no list of cases
  code
      enum Genre:
        case Comedy, History, Tragedy

      object Tests extends Suite(m"Play tests"):
        def run(): Unit =
          test(m"every genre has a name").over(Genre): genre =>
            genre.toString.length
          . assert((genre, length) => length > 0)
##
```

The example is compiled, and is not run, since a test is run by a test runner.
[Spreading a test over axes](../../soundness/modules/testing.md#spreading-a-test-over-axes)
continues to grids of two axes.
