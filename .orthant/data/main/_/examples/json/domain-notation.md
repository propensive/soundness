# JSON with Scala values in it

A `j"…"` literal is written as JSON is, and Scala values go where JSON values would. A value
in a value's place is encoded, a collection followed by `*` is spliced into an array, and a map
in a field's place adds its fields to an object.

```scala fuel
compile
frame
  label A value
  highlight $name  encoded as a string
  code
      val name = t"Feste"
      val fool = j"""{"name": $name}"""
frame
  label A collection, spliced
  highlight $songs*  its elements, not an array within an array
  code
      val name = t"Feste"
      val songs = List(t"O Mistress Mine", t"Come Away, Death")
      val fool = j"""{"name": $name, "songs": ["Hey, Robin", $songs*]}"""
frame
  label More fields, merged
  highlight $more  every entry becomes a field
    line 5
  code
      val name = t"Feste"
      val songs = List(t"O Mistress Mine", t"Come Away, Death")
      val more: Map[Text, Json] = Map(t"house" -> t"Olivia".in[Json])
      val fool = j"""{"name": $name, "songs": ["Hey, Robin", $songs*],
                      $more}"""
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(fool.show)
```

[Building JSON](../../soundness/modules/json.md#building-json) describes each substitution.
