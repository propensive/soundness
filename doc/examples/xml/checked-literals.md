# A closing tag which does not match

An `x"…"` literal is XML which the compiler parses, with its substitutions in place. A tag
closed by the wrong name is a compile error.

```scala fuel
compile
frame
  label A misspelt closing tag
  errors show
  code
      def user(name: Text): Xml = x"<user>$name</usr>"
frame
  label Matching tags
  code
      def user(name: Text): Xml = x"<user>$name</user>"
##
```

[XML](../../soundness/modules/xml.md) describes what may be substituted, and where.
