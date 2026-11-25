# Styled text is cut like text

A `Teletype` is text with styles, and it takes the methods which `Text` takes: `length`,
`keep`, `skip` and `fit` have one name each, and the styles of the characters which remain are
preserved.

```scala fuel
compile
frame
  label Plain text
  code
      import textMetrics.uniformMetric

      val plain = t"Exit, pursued by a bear"

      val width = plain.length
      val start = plain.keep(4)
frame
  label The same, with styles
  highlight e"  styled text, where there was plain
  code
      import textMetrics.uniformMetric

      val styled = e"$Bold(Exit), pursued by a $Italic(bear)"

      val width = styled.length
      val start = styled.keep(4)
frame
  label Fitted to a column
  highlight fit(12)  twelve characters, and no broken escape code
  code
      import textMetrics.uniformMetric

      val styled = e"$Bold(Exit), pursued by a $Italic(bear)"

      val width = styled.length
      val start = styled.keep(4)
      val column = styled.fit(12)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(width.show)
Out.println(start)
Out.println(column)
```

The length counts characters and not escape codes, since there are none until the text is
printed. [Styled text](../../soundness/modules/terminal.md#styled-text) has the rest.
