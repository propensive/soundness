# A value which crosses without its class

`pojo` turns a value into arrays, strings and boxed primitives, which every classloader
shares, and `as` turns it back. Both directions are derived from the case class, so there is
no codec to write and no schema to send.

```scala fuel
compile
frame
  label A value
  code
      case class Player(name: Text, age: Int)
      case class Troupe(players: List[Player], size: Int)

      val troupe = Troupe(List(Player(t"Quince", 40), Player(t"Bottom", 35)), 2)
frame
  label Encoded with nothing but JDK types
  highlight pojo  arrays, strings and boxed primitives
  code
      case class Player(name: Text, age: Int)
      case class Troupe(players: List[Player], size: Int)

      val troupe = Troupe(List(Player(t"Quince", 40), Player(t"Bottom", 35)), 2)
      val transportable: Pojo = troupe.pojo
frame
  label And decoded on the far side
  highlight as[Troupe]  the type expected
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)
      case class Troupe(players: List[Player], size: Int)

      val troupe = Troupe(List(Player(t"Quince", 40), Player(t"Bottom", 35)), 2)
      val transportable: Pojo = troupe.pojo
      val arrived: Troupe = transportable.as[Troupe]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(arrived.inspect)
```

[Encoding and decoding](../../soundness/modules/generic-data.md#encoding-and-decoding) has the
details.
