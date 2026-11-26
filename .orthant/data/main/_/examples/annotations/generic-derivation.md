# The fields of one type, with no cast

A renderer which wants every number in a record need not know the record. `membersOfType`
picks out the fields of one type, through accessors which the compiler generates, and gives
them at that type.

```scala fuel
compile
frame
  label A record of mixed fields
  code
      case class Scene(place: Text, act: Int, number: Int)

      val scene = Scene(t"Elsinore", 3, 1)
frame
  label Its numbers, and its text
  highlight membersOfType[Int]  a Map of Text to Int
  code
      case class Scene(place: Text, act: Int, number: Int)

      val scene = Scene(t"Elsinore", 3, 1)

      val numbers = scene.membersOfType[Int]
      val words = scene.membersOfType[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(numbers.inspect)
Out.println(words.inspect)
```

[Reaching fields by name](../../soundness/modules/annotations.md#reaching-fields-by-name) has
the typeclass beneath it.
