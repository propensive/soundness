# Equal, give or take

Floating-point results are compared within a tolerance, and the tolerance is written as it is
on paper: a value, `+/-`, and a margin. The assertion reads as the claim it makes.

```scala fuel
compile
frame
  label Exact equality, which floating point does not honor
  code
      object Tests extends Suite(m"Arithmetic tests"):
        def run(): Unit =
          test(m"a tenth and a fifth make three tenths"):
            0.1 + 0.2
          . assert(_ == 0.3)
frame
  label Equality within a margin
  highlight ===..0.001  three tenths, to within a thousandth
  code
      object Tests extends Suite(m"Arithmetic tests"):
        def run(): Unit =
          test(m"a tenth and a fifth make three tenths"):
            0.1 + 0.2
          . assert(_ === 0.3 +/- 0.001)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((0.1 + 0.2 == 0.3).inspect)
Out.println((0.1 + 0.2 === 0.3 +/- 0.001).inspect)
```

The comparisons are run here on their own, outside a test.
[Suites and tests](../../soundness/modules/testing.md#suites-and-tests) has the assertions which
use them.
