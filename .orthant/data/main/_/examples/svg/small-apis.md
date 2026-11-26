# One method writes every figure

No figure has a `toXml` of its own. A shape, a group of shapes and a whole drawing are each
written with `in[Xml]`, and SVG text is read back with `read[Svg]`, the method which reads
everything else.

```scala fuel
compile
frame
  label A shape
  highlight in[Xml]  a circle, as XML
  code
      import strategies.throwUnsafely
      import Svg.{Group, Rectangle}

      val moon = Circle((20, 20), 8)
      val shape = moon.in[Xml]
frame
  label A group
  highlight in[Xml]  a group, by the same method
    line 6
  code
      import strategies.throwUnsafely
      import Svg.{Group, Rectangle}

      val moon = Circle((20, 20), 8)
      val shape = moon.in[Xml]
      val group = Group(List(moon, Rectangle((0, 30), 40, 10))).in[Xml]
frame
  label A drawing, read from text
  highlight read[Svg]  the method which reads JSON, bytes and files
  code
      import strategies.throwUnsafely
      import Svg.{Group, Rectangle}

      val moon = Circle((20, 20), 8)
      val shape = moon.in[Xml]
      val group = Group(List(moon, Rectangle((0, 30), 40, 10))).in[Xml]
      val night = t"""<svg width="40" height="40"><circle cx="20" cy="20" r="8"/></svg>""".read[Svg]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(shape.show)
Out.println(group.show)
Out.println(night.in[Xml].show)
```

[Shapes](../../soundness/modules/svg.md#shapes) lists the element each figure is written as,
and [small APIs](../../philosophy/small-apis.md) says why they share a name.
