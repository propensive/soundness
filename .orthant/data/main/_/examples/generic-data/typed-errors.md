# Data which may not have the expected shape

A `Pojo` has lost its class, so the side which receives one must say what it expects, and the
data may not match. Decoding therefore raises a `Pojo.Error`, and a method which decodes says
so, or does not compile.

```scala fuel
compile
frame
  label A method which assumes the data fits
  errors show
  code
      case class Player(name: Text, age: Int)

      def receive(data: Pojo): Player = data.as[Player]
frame
  label The failure, in the return type
  highlight raises..Pojo.Error  the caller must handle it
  code
      case class Player(name: Text, age: Int)

      def receive(data: Pojo): Player raises Pojo.Error = data.as[Player]
##
```

[Encoding and decoding](../../soundness/modules/generic-data.md#encoding-and-decoding) shows the
error for a string which is not a `Player`.
