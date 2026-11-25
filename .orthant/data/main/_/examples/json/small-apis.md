# One method decodes every type

There is no `getInt`, `getString` or `getList`. `as` takes the type which is wanted, and reads
a number, a list or a map by the same name.

```scala fuel
compile
frame
  label A number
  highlight as[Int]  a primitive
  code
      import strategies.throwUnsafely

      val acts: Int = j"5".as[Int]
frame
  label A list
  highlight as[List[Text]]  a collection
  code
      import strategies.throwUnsafely

      val acts: Int = j"5".as[Int]
      val fools: List[Text] = j"""["Feste", "Touchstone"]""".as[List[Text]]
frame
  label A map
  highlight as[Map[Text..Int]]  an object, by its keys
  code
      import strategies.throwUnsafely

      val acts: Int = j"5".as[Int]
      val fools: List[Text] = j"""["Feste", "Touchstone"]""".as[List[Text]]
      val lines: Map[Text, Int] = j"""{"Feste": 318, "Touchstone": 516}""".as[Map[Text, Int]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(acts.show)
Out.println(fools.inspect)
Out.println(lines.inspect)
```

[Reading values](../../soundness/modules/json.md#reading-values) has the rest, and
[small APIs](../../philosophy/small-apis.md) the reason for one name.
