# A key which is not the field's name

The keys of a derived codec are the names of the fields, and JSON from elsewhere seldom spells
them as Scala does. An annotation on a field gives its key, and the encoder and the decoder
both follow it.

```scala fuel
compile
frame
  label Keys named as the fields are
  code
      case class Play(title: Text, firstFolio: Int)

      val json: Json = Play(t"Twelfth Night", 1623).in[Json]
frame
  label One key, renamed for JSON
  highlight @name[Json](t"first_folio")  in JSON alone
  code
      case class Play(title: Text, @name[Json](t"first_folio") firstFolio: Int)

      val json: Json = Play(t"Twelfth Night", 1623).in[Json]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(json.show)
```

[Renaming fields](../../soundness/modules/json.md#renaming-fields) has the form which renames a
field in every format.
