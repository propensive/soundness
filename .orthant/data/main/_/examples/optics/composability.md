# The same update, in a JSON document

A lens knows nothing of JSON, and JSON nothing of lenses, but a parsed document is updated
with the path and the assignment which update a case class. The imports supply the optics for JSON.

```scala fuel
compile
frame
  label A case class
  code
      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      val act = Act(t"Act I", List(Scene(t"A heath", 1), Scene(t"Forres", 2)))
      val moved = act.lens(_.scenes(Each).place = t"Dunsinane")
frame
  label The same value, as JSON
  highlight in[Json]  a JSON value, not a case class
  code
      import dynamicAccess.dynamicJson, conversions.encodableToJson
      import optics.{jsonLens, jsonEachOptical}, strategies.throwUnsafely

      case class Scene(place: Text, number: Int)
      case class Act(name: Text, scenes: List[Scene])

      val act = Act(t"Act I", List(Scene(t"A heath", 1), Scene(t"Forres", 2)))
      val json: Json = act.in[Json]
      val moved = json.lens(_.scenes(Each).place = t"Dunsinane")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import formatting.compactJsonFormatting

Out.println(moved.show)
```

[Documents too](../../soundness/modules/optics.md#documents-too) lists the formats which define
optics, and [composability](../../philosophy/composability.md) explains how two libraries meet
without an adapter.
