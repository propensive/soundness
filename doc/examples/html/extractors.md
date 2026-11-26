# Markup taken apart by markup

An `h"…"` literal is a pattern too. It matches by the structure of the nodes and not by their
text, and a substitution binds whatever stands in its place.

```scala fuel
compile
frame
  label A pattern which matches one item
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      def role(item: Html): Text = item match
        case h"<li>Oberon</li>" => t"king"
        case _                  => t"unknown"
frame
  label A pattern which binds the item's body
  highlight $body  whatever the item holds, as `Html`
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      def role(item: Html): Text = item match
        case h"<li>$body</li>" => body.show
        case _                 => t"unknown"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(role(h"<li><em>Titania</em></li>"))
```

[Pattern matching](../../soundness/modules/html.md#pattern-matching) lists what else a pattern
can bind: an attribute's value, the attributes together, or a whole element.
