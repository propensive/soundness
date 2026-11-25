# A document taken apart by its shape

A `j"…"` literal is also a pattern. In a `case`, each substitution binds the JSON found in its
place, and a document of any other shape goes on to the next case.

```scala fuel
compile
frame
  label A value bound from a field
  highlight $title  bound as Json
    line 4
  code
      import strategies.throwUnsafely

      def title(song: Json): Text = song match
        case j"""{"title": $title}""" => title.as[Text]
        case _                        => t"untitled"
frame
  label The other fields, bound as an object
  highlight $rest  a Json object of whatever else is there
  code
      import strategies.throwUnsafely

      def title(song: Json): Text = song match
        case j"""{"title": $title, $rest}""" => title.as[Text]
        case _                               => t"untitled"
frame
  label An array, by its first element and the others
  highlight $others*  a Json array
  code
      import strategies.throwUnsafely

      def title(song: Json): Text = song match
        case j"""{"title": $title, $rest}""" => title.as[Text]
        case j"""[$first, $others*]"""       => title(first)
        case _                               => t"untitled"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(title(j"""{"title": "O Mistress Mine", "singer": "Feste"}"""))
Out.println(title(j"""[{"title": "Come Away, Death"}, {"title": "Hey, Robin"}]"""))
Out.println(title(j"""{"singer": "Feste"}"""))
```

[Matching JSON](../../soundness/modules/json.md#matching-json) lists what each substitution
binds.
