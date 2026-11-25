# Records read while an image is open

A RaTEL image is read in place, through a reader which exists for the length of a block. Rows
are found and decoded inside the block, and what leaves it is an ordinary value, which needs
the image no longer.

```scala fuel
compile
frame
  label An image, in memory
  code
      import strategies.throwUnsafely

      case class Part(@key name: Text, play: Text)
      case class Playbill(part: List[Part])

      val playbill = Playbill(List(Part(t"Viola", t"Twelfth Night"), Part(t"Puck", t"Dream")))
      val image = playbill.ratel
frame
  label A block which reads one record
  highlight open[Ratel]()  the reader lives for this block
  highlight find(t"Puck")  by the primary index
  code
      import strategies.throwUnsafely

      case class Part(@key name: Text, play: Text)
      case class Playbill(part: List[Part])

      val playbill = Playbill(List(Part(t"Viola", t"Twelfth Night"), Part(t"Puck", t"Dream")))
      val image = playbill.ratel

      val puck: Optional[Part] = image.open[Ratel]():
        (ratel.root / t"part").find(t"Puck").let(_.as[Part])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(puck.inspect)
```

[RaTEL](../../soundness/modules/tel.md#ratel) opens a file in the same way, and
[delimited scopes](../../philosophy/delimited-scopes.md) explains the block.
