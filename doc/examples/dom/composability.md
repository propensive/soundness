# An event handler which is not a string

An `onclick` attribute expects a script, which is a type of its own. A checked DOM expression
is one, and is written into the page as JavaScript. Text is not, however much it looks like
JavaScript.

```scala fuel
compile
frame
  label JavaScript in a string
  errors show
  code
      import htmlDoms.html4Transitional, html4Transitional.*

      val button = Button(onclick = t"document.getElementById('stage').focus()")
frame
  label The same script, checked
  highlight document.getElementById(t"stage").focus()  navigated, not quoted
  code
      import htmlDoms.html4Transitional, html4Transitional.*

      val button = Button(onclick = document.getElementById(t"stage").focus())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(button.show)
```

[Scripts in HTML attributes](../../soundness/modules/dom.md#scripts-in-html-attributes) explains
why only an expression from the browser's ecosystem will do.
