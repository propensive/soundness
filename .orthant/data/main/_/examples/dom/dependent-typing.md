# Arguments at the browser's own types

An argument to a DOM method is checked against the type its interface declares, in the
browser's terms. `scrollTo` takes two 32-bit integers, so it takes two `S32` values, and an
identifier is text and not a number.

```scala fuel
compile
frame
  label A number where the DOM wants a string
  errors show
  code
      val stage = document.getElementById(42)
frame
  label Text for an identifier
  code
      val stage = document.getElementById(t"stage")
frame
  label Sized integers for a position
  highlight S32  what the interface calls `long`
    occurrence 2
  code
      val stage = document.getElementById(t"stage")
      val left: S32 = 0
      val top: S32 = 480
      val scrolled = window.scrollTo(left, top)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(Javascript.serialize(scrolled.expr))
```

[Values that cross the boundary](../../soundness/modules/dom.md#values-that-cross-the-boundary)
lists which Scala types stand for which foreign ones.
