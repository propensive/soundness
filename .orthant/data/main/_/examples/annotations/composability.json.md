# A field renamed on the wire

`@name` sits on the field it renames, and every serialization format reads it. Bare, it
applies to all of them, and with a type argument to one, so JSON may use a key which the
other formats do not.

```scala fuel
compile
frame
  label The field names, as keys
  code
      case class Player(firstName: Text, year: Int)

      val json: Json = Player(t"Rosalind", 1599).in[Json]
frame
  label A key for every format
  highlight @name(t"yob")  in every format
  code
      case class Player(firstName: Text, @name(t"yob") year: Int)

      val json: Json = Player(t"Rosalind", 1599).in[Json]
frame
  label A key for JSON alone
  highlight @name[Json](t"first_name")  in JSON only
  code
      case class Player(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)

      val json: Json = Player(t"Rosalind", 1599).in[Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(json.show)
```

[The `@name` annotation](../../soundness/modules/annotations.md#the-name-annotation) shows how a
derivation reads it.
