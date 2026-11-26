# A slice longer than the text

Java's `substring` throws when its index is past the end. `keep` takes as many characters as
there are, so asking for more than the text has gives the whole of it, and there is no index
which makes it fail.

```scala fuel
compile
frame
  label The first two characters
  highlight keep(2)  two characters, from the left
  code
      val name = t"Lear"
      val short = name.keep(2)
frame
  label The first ten, of four
  highlight keep(10)  all four, and no exception
  code
      val name = t"Lear"
      val short = name.keep(2)
      val whole = name.keep(10)
frame
  label The last two
  highlight Rtl  counted from the right
  code
      val name = t"Lear"
      val short = name.keep(2)
      val whole = name.keep(10)
      val ending = name.keep(2, Rtl)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(short)
Out.println(whole)
Out.println(ending)
```

[Slicing](../../soundness/modules/text.md#slicing) has `skip`, `snip` and `segment`, and
[total transitions](../../philosophy/total-transitions.md) the principle.
