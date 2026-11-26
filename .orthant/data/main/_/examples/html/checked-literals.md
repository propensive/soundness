# Markup the specification forbids

HTML written with `h"…"` is parsed by the compiler, by the rules of the specification. A
paragraph directly inside a list is not HTML, so it does not compile.

```scala fuel
compile
frame
  label A paragraph where only list items may go
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val cast = h"<ul><p>Oberon</p></ul>"
frame
  label A list item
  highlight <li>Oberon</li>  what a list may hold
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val cast = h"<ul><li>Oberon</li></ul>"
##
```

[Writing HTML literally](../../soundness/modules/html.md#writing-html-literally) says what may be
substituted into a literal.
