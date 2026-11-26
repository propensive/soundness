# A method the browser does not have

A script for a page is written by navigating `document` as though the DOM's interfaces were
Scala types. They are checked against the browser's own definitions, so a misspelt method is a
compile error and not `undefined is not a function` in somebody's browser.

```scala fuel
compile
frame
  label An `l` where an `I` belongs
  errors show
  code
      val heading = document.getElementByld(t"heading")
frame
  label A method which `Document` declares
  highlight getElementById  from the DOM's own interface definition
  code
      val heading = document.getElementById(t"heading")
frame
  label And one which its result declares
  highlight focus()  a member of `HTMLElement`
  code
      val heading = document.getElementById(t"heading")
      val focused = heading.focus()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(Javascript.serialize(focused.expr))
```

[Navigating the DOM](../../soundness/modules/dom.md#navigating-the-dom) describes how each step
gives a value of the declared type.
