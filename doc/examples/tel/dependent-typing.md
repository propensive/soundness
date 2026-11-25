# An image which knows its type

RaTEL lays a document out as tables which are read in place. The image of a value, made with no
schema given, is laid out under the schema of the value's type, and that type is part of its
own: a `Ratel of Playbill` is not a `Ratel of` anything else.

```scala fuel
compile
frame
  label An image of a playbill
  highlight Ratel..Playbill  the schema, in the type
  code
      import strategies.throwUnsafely

      case class Part(@key name: Text, play: Text)
      case class Playbill(part: List[Part])

      val playbill = Playbill(List(Part(t"Viola", t"Twelfth Night")))
      val image: Ratel of Playbill = playbill.ratel
frame
  label The same image, taken for another type
  errors show
  code
      import strategies.throwUnsafely

      case class Part(@key name: Text, play: Text)
      case class Playbill(part: List[Part])
      case class Cast(player: List[Part])

      val playbill = Playbill(List(Part(t"Viola", t"Twelfth Night")))
      val image: Ratel of Cast = playbill.ratel
##
```

[RaTEL](../../soundness/modules/tel.md#ratel) describes the annotations which give an image
its indexes.
