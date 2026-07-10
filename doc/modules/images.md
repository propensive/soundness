## Images

### About

Raster images — PNG, JPEG, GIF and BMP — read into a `Raster` value with the format in its type,
just as [audio](audio.md) carries its format. A `Raster in Png` and a `Raster in Jpeg` are distinct
types; converting between them is one method; and the image itself is an immutable grid of pixels,
each read as a typed [color](colors.md), with cropping, flipping and rotation producing new images.

### On raster images

The JVM's imaging API reads any format into a mutable `BufferedImage` and forgets which format it
was, leaving encoding decisions and pixel formats to flag-driven calls. For the common tasks — read
an image, ask its size, look at its pixels, convert its format, write it back — that machinery is
mostly ceremony, and the mutability makes an image unsafe to share.

A `Raster` is immutable and knows its format. Reading names the format expected, so a JPEG posing
as a PNG is a typed error, not a null; pixels come back as colors, ready for the color operations;
and writing is the same polymorphic reading-as-bytes used by every other format. Everything comes
from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Reading

A source of bytes reads as a raster of a named format:

```scala
val image = data.read[Raster in Png]

image.width       // in pixels
image.height
image.landscape   // true when wider than tall
```

Bytes that are not the named format raise a `RasterError` naming the format that failed:

```scala
capture[RasterError](data.read[Raster in Jpeg])   // when data is a PNG
```

### Pixels

Applying an image to coordinates reads a pixel as a `Chroma` — a color with byte channels — which
connects the image to all the [color](colors.md) machinery:

```scala
image(0, 0).red   // the red channel of the top-left pixel
```

An image is also *built* from a pixel function, which is how test images and generated graphics
are made:

```scala
val gradient = Raster(256, 1)((x, y) => Chroma(x, 0, 255 - x))
```

### Transforming

Cropping, flipping and quarter-turn rotation each produce a new image:

```scala
image.crop(top = 10, bottom = 10)
image.flipX
image.rotate(90)
```

The rotation argument admits only `90`, `180` or `270` — a rotation that would need resampling is
not quietly approximated.

### Converting and writing

`to` re-expresses an image in another format, and reading it as `Data` produces the encoded bytes,
symmetrically with reading:

```scala
val jpegBytes = image.to[Jpeg].read[Data]
```
