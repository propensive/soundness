# A set of integers filled in a block

A `Bitset` is immutable, but one which is filled in a loop need not be copied at each step.
`Bitset.build` lends a builder to a block, which writes to it in place, and the finished set
is the value of the block.

```scala fuel
compile
frame
  label A new set for every member
  code
      val lines: List[Int] = List(3, 14, 15, 92)
      var marked: Bitset = Bitset()

      lines.each: line =>
        marked = marked + line.z
frame
  label One builder, for the length of the block
  highlight Bitset.build..=>  a builder, lent to the block
  highlight bits.add(line.z)  written in place
  code
      val lines: List[Int] = List(3, 14, 15, 92)

      val marked: Bitset = Bitset.build: bits =>
        lines.each: line =>
          bits.add(line.z)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(marked.size.show)
Out.println(marked.has(15.z).inspect)
```

The builder does not outlive the block, so nothing can write to the set once it has been
given back. [Sets of integers](../../soundness/modules/flags.md#sets-of-integers) has the rest
of a `Bitset`.
