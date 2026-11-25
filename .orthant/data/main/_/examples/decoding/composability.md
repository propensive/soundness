# Typed values from the groups of a pattern

A regular expression captures text, and `As` turns text into a typed value. Neither knows of
the other, but an `As` extractor written before a group binds the group as a number, so a
match validates, captures and converts in one step.

```scala fuel
compile
frame
  label Groups which bind text
  highlight $hours  binds the group as Text
  code
      def minutes(time: Text): Text = time match
        case r"$hours(\d+):$minutes(\d+)" => minutes
        case _                            => t"0"
frame
  label Groups which bind numbers
  highlight ${As[Int](hours)}  binds the group as an Int
  code
      def minutes(time: Text): Int = time match
        case r"${As[Int](hours)}(\d+):${As[Int](minutes)}(\d+)" => hours*60 + minutes
        case _                                                  => 0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(minutes(t"12:13").show)
```

[Patterns](../../soundness/modules/patterns.md#matching) describes the groups, and
[composability](../../philosophy/composability.md) how extractors from two libraries nest.
