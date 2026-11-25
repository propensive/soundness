# Closing tags the parser supplies

The specification lets some closing tags be left out, and a literal follows the same rules. Two
literals written differently are the same value where a browser would read them as the same
document.

```scala fuel
compile
frame
  label Every tag closed
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val full = h"<ol><li>Oberon</li><li>Titania</li></ol>"
frame
  label The optional ones left out
  highlight <li>Oberon<li>Titania  each item ends where the next begins
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val full = h"<ol><li>Oberon</li><li>Titania</li></ol>"
      val brief = h"<ol><li>Oberon<li>Titania</ol>"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((full == brief).inspect)
```

[Writing HTML literally](../../soundness/modules/html.md#writing-html-literally) describes the
literal, which is parsed as the code compiles and costs nothing to parse as it runs.
