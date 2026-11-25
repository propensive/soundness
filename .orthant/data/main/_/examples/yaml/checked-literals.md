# YAML which is checked where it is written

A `y"…"` literal is YAML which the compiler parses. A flow mapping which is never closed is a
compile error, and a substituted value takes its place in the document as that document is
built.

```scala fuel
compile
frame
  label A mapping which is not closed
  errors show
  code
      val prince = y"{name: Hamlet, age: 30"
frame
  label Well-formed YAML
  code
      val prince = y"{name: Hamlet, age: 30}"
frame
  label A value substituted, in block style
  highlight $name  encoded as a YAML string
  code
      val name = t"Hamlet"

      val prince = y"""
        name: $name
        age: 30
      """
##
```

[Writing YAML literally](../../soundness/modules/yaml.md#writing-yaml-literally) decodes such a
literal to a case class.
