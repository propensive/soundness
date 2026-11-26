# The narrowest integer which holds a pixel

A pixel layout is a tuple of channels, each with its depth in bits. The compiler adds the
depths up, and computes from the total the integer type which stores a pixel.

```scala fuel
compile
frame
  label Sixteen bits do not fit into a byte
  errors show
  code
      import anticipation.Channel, Channel.{Red, Green, Blue}

      type Rgb565 = (Red[5], Green[6], Blue[5])

      val storage = summon[Channel.Storage[Rgb565] =:= Byte]
frame
  label They fit into a short
  highlight Short  sixteen bits
  code
      import anticipation.Channel, Channel.{Red, Green, Blue}

      type Rgb565 = (Red[5], Green[6], Blue[5])

      val storage = summon[Channel.Storage[Rgb565] =:= Short]
##
```

[Pixel layouts](../../soundness/modules/colors.md#pixel-layouts) has the predefined layouts.
