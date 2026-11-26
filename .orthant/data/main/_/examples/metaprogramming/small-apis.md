# One `sequence` for every collection

A collection of values which may each be missing is turned inside out by `sequence`: all of
them, or none. The method is written once, for anything which can be traversed, and the
result keeps the shape of the collection it was called on.

```scala fuel
compile
frame
  label A list, with every element present
  code
      val cast = List[Option[Text]](Some(t"Viola"), Some(t"Feste")).sequence
frame
  label A list, with one missing
  highlight None  one absence makes the whole result absent
  code
      val cast = List[Option[Text]](Some(t"Viola"), Some(t"Feste")).sequence
      val short = List[Option[Text]](Some(t"Viola"), None).sequence
frame
  label A set
  highlight Set  gives an Option of a Set
  code
      val cast = List[Option[Text]](Some(t"Viola"), Some(t"Feste")).sequence
      val short = List[Option[Text]](Some(t"Viola"), None).sequence
      val twins = Set[Option[Text]](Some(t"Viola"), Some(t"Sebastian")).sequence
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(cast.inspect)
Out.println(short.inspect)
Out.println(twins.inspect)
```

[Abstracting over containers](../../soundness/modules/metaprogramming.md#abstracting-over-containers)
describes `traverse` and the typeclasses beneath both.
