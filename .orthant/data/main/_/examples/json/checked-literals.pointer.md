# A pointer with a bad escape

A JSON Pointer has two escapes, `~0` for a tilde and `~1` for a slash. A `jp"…"` literal is
parsed by the compiler, so any other escape is a compile error at the character which is wrong.

```scala fuel
compile
frame
  label An escape which does not exist
  errors show
  code
      val house = jp"#/verona~2capulet"
frame
  label A slash within a segment
  highlight ~1  a slash, escaped
  code
      val house = jp"#/verona~1capulet"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(house.encode)
```

[JSON Pointers](../../soundness/modules/json.md#json-pointers) shows a pointer built a segment
at a time.
