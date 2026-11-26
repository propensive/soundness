# An attribute added, and nothing changed

An element is a value, and never changes. Assigning to an attribute reads as mutation, and is
an expression whose result is a new element.

```scala fuel
compile
frame
  label An image
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*, attributives.textAttributive

      val image: Html.Element of "img" in Whatwg = Img(src = t"puck.jpg")
frame
  label The same image, described
  highlight image.alt  an assignment which gives a new element
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*, attributives.textAttributive

      val image: Html.Element of "img" in Whatwg = Img(src = t"puck.jpg")
      val described = image.alt = t"Puck, laughing"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(image.show)
Out.println(described.show)
```

[Transforming](../../soundness/modules/html.md#transforming) has the operators which add
children in the same way.
