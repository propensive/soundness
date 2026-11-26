# Reading markup like anything else

HTML is read from text with `read`, the method which reads JSON, a number or a date. The type
that is asked for says how much is expected: any markup, or one particular tag.

```scala fuel
compile
frame
  label Whatever markup the text holds
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*, strategies.throwUnsafely

      val line = t"Lord, what <em>fools</em> these mortals be".read[Html]
frame
  label A list, and nothing else
  highlight "ul"]  the tag expected at the top
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*, strategies.throwUnsafely

      val line = t"Lord, what <em>fools</em> these mortals be".read[Html]
      val cast = t"<ul><li>Puck<li>Oberon</ul>".read[Html of "ul"]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line.show)
Out.println(cast.show)
```

[Parsing](../../soundness/modules/html.md#parsing) covers whole documents, which `load` reads.
