# JSON which is checked where it is written

A `j"…"` literal is JSON which the compiler parses. A stray comma is a compile error at the
comma, and a substituted value is encoded by its type.

```scala fuel
compile
frame
  label A trailing comma
  errors show
  code
      val fool = j"""{ "name": "Feste", "songs": 4, }"""
frame
  label Well-formed JSON
  code
      val fool = j"""{ "name": "Feste", "songs": 4 }"""
frame
  label A value substituted
  highlight $songs  encoded as a JSON number
  code
      val songs = 4
      val fool = j"""{ "name": "Feste", "songs": $songs }"""
##
```

[Building JSON](../../soundness/modules/json.md#building-json) describes substitution.
