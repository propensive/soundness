# The largest member, if there is one

An empty set has no largest member. `last` on a `Bitset` therefore gives an `Optional`
ordinal, and code which wants a number must say what an empty set means to it.

```scala fuel
compile
frame
  label Used as though the set had members
  errors show
  code
      def furthest(visited: Bitset): Int = visited.last.n0
frame
  label Transformed, if it is there
  highlight let(_.n0)  applied only to a member which exists
  code
      def furthest(visited: Bitset): Optional[Int] = visited.last.let(_.n0)
frame
  label And replaced, if it is not
  highlight or(-1)  what an empty set means here
  code
      def furthest(visited: Bitset): Int = visited.last.let(_.n0).or(-1)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(furthest(Bitset(3.z, 14.z, 15.z)).show)
Out.println(furthest(Bitset()).show)
```

[Sets of integers](../../soundness/modules/flags.md#sets-of-integers) has the other operations,
and [optionality](../../philosophy/optionality.md) explains `let` and `or`.
