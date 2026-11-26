# A rearrangement which must be one

A permutation is built from a sequence of indexes, and not every sequence is a permutation:
an index may be repeated, or missing. Building one can therefore fail, and says so in its
type.

```scala fuel
compile
frame
  label Indexes which may not be a permutation
  errors show
  code
      val shuffle = Permutation(Sequence(3, 1, 1))
frame
  label The failure, handled
  highlight safely  Unset, since 1 is given twice
  code
      val shuffle = safely(Permutation(Sequence(3, 1, 1)))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shuffle.absent.inspect)
```

[Permutations](../../soundness/modules/mathematics.md#permutations) describes how a
permutation is stored and applied.
