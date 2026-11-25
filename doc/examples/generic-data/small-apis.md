# The same method decodes text and generic data

There is no `fromPojo`. A value is read out of a `Pojo` with `as`, the method which reads one
out of text, and the type which is asked for chooses the decoder.

```scala fuel
compile
frame
  label A number, from text
  highlight as[Int]  decodes Text
  code
      import strategies.throwUnsafely

      val age: Int = t"35".as[Int]
frame
  label A case class, from generic data
  highlight as[Player]  decodes a Pojo
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val age: Int = t"35".as[Int]
      val data: Pojo = Player(t"Bottom", age).pojo
      val player: Player = data.as[Player]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(player.inspect)
```

[Encoding and decoding](../../soundness/modules/generic-data.md#encoding-and-decoding) has the
details, and [small APIs](../../philosophy/small-apis.md) explains why there is one name.
