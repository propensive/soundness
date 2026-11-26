# The format of an image, in its type

`Raster in Png` is read as it is written: a raster, in the PNG format. A `Raster in Jpeg` is
another type, so a PNG is never passed off as a JPEG by forgetting to convert it.

```scala fuel
compile
frame
  label A PNG where a JPEG is wanted
  errors show
  code
      import strategies.throwUnsafely

      def publish(photograph: Raster in Jpeg): Data = photograph.read[Data]

      def portrait(source: Data): Data = publish(source.read[Raster in Png])
frame
  label The PNG, converted
  highlight to[Jpeg]  gives a `Raster in Jpeg`
  code
      import strategies.throwUnsafely

      def publish(photograph: Raster in Jpeg): Data = photograph.read[Data]

      def portrait(source: Data): Data = publish(source.read[Raster in Png].to[Jpeg])
##
```

[Converting and writing](../../soundness/modules/images.md#converting-and-writing) lists the
formats, and [infix types](../../philosophy/infix-types.md) the words which join them.
