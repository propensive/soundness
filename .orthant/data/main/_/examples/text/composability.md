# Styled text, centered like any other

The operations on text are written against the `Textual` typeclass and not against `Text`.
The styled text of a terminal is textual too, so it is centered by the same method, and keeps
its styles.

```scala fuel
compile
frame
  label Plain text
  code
      import textMetrics.uniformMetric

      val heading = t"Twelfth Night".center(21)
frame
  label Styled text
  highlight e"$Bold(Twelfth)..Night"  styled text, centered by the same method
  code
      import textMetrics.uniformMetric

      val heading = e"$Bold(Twelfth) Night".center(21)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(heading.plain.inspect)
```

[Any textual type](../../soundness/modules/text.md#any-textual-type) explains the typeclass,
and [Terminal](../../soundness/modules/terminal.md) the styles.
