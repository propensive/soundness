# A pattern which decodes as it matches

`As[Int]` is an extractor which matches text that holds a number, and binds the number. It
nests inside any other pattern, so a list of text is taken apart and decoded in one `case`,
and anything which does not fit falls through to the next.

```scala fuel
compile
frame
  label One value
  highlight As[Int](hours)  matches text which holds a number
  code
      def hours(text: Text): Int = text match
        case As[Int](hours) => hours
        case _              => 0
frame
  label Three, inside a list pattern
  highlight As[Int](hours)..Nil  three decoders in one pattern
  code
      def seconds(parts: List[Text]): Int = parts match
        case As[Int](hours) :: As[Int](minutes) :: As[Int](seconds) :: Nil =>
          hours*3600 + minutes*60 + seconds

        case _ =>
          0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(seconds(List(t"12", t"13", t"14")).show)
Out.println(seconds(List(t"12", t"thirteen", t"14")).show)
```

[Extracting in patterns](../../soundness/modules/decoding.md#extracting-in-patterns) has more.
