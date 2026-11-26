# Pixels written only through a canvas opened for writing

A raster is immutable. To draw on one, it is opened as a `Canvas` for the length of a block,
and the block may write only if it was opened with the `Write` grant.

```scala fuel
compile
frame
  label A canvas opened for reading
  errors show
  code
      import soundness.canvasOpenable

      val night = Raster[Rgba](8, 8)((x, y) => Pixel[Rgba](Srgb(0.0, 0.0, 0.2)))

      night.open[Canvas](Read): canvas ?=>
        canvas(4, 4) = Pixel[Rgba](Srgb(1.0, 1.0, 1.0))
frame
  label A canvas opened for writing
  highlight Read..Write  the grant which `update` asks for
  code
      import soundness.canvasOpenable

      val night = Raster[Rgba](8, 8)((x, y) => Pixel[Rgba](Srgb(0.0, 0.0, 0.2)))

      night.open[Canvas](Read & Write): canvas ?=>
        canvas(4, 4) = Pixel[Rgba](Srgb(1.0, 1.0, 1.0))
##
```

[Canvases](../../soundness/modules/images.md#canvases) describes the handle, and
[delimited scopes](../../philosophy/delimited-scopes.md) the pattern it shares with files.
