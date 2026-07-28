## Images

### About

Raster images — PNG, JPEG, WebP, GIF and BMP — read into a `Raster` value with the format in its
type, just as [audio](audio.md) carries its format. A `Raster in Png` and a `Raster in Jpeg` are
distinct types; converting between them is one method; and the image itself is an immutable grid
of pixels, each read as a typed [color](colors.md), with cropping, flipping and rotation
producing new images.

A raster may also carry its *pixel layout* in its type — how many bits each channel occupies and
in what order — so a pixel is read with a single shift and mask, computed as the code compiles
rather than dispatched at runtime. Every codec is implemented directly, so images read and write
on every platform, not only on the JVM.

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

### Pixel layouts

A layout is a tuple of channel types, most significant first — `(Red[10], Green[12], Blue[10])`
is exactly the packing of a 32-bit RGB pixel, and `Rgba` names the familiar eight-bit one. A
raster built with a layout gives typed access, and `pixel` compiles to one constant shift and
mask:

```scala
val raster = Raster[Rgba](2, 2): (x, y) =>
  Pixel[Rgba](Srgb(x.toDouble, y.toDouble, 1.0))

raster.pixel(1, 0).red
raster.pixel(1, 0).alpha
```

`repack` converts between layouts, adding a fully-opaque alpha channel where the target has one
and scaling the channels where their depths differ; repacking to the layout a raster already has
returns the same raster. A `descriptor` reports the layout at runtime, for the operations that
must work whatever it is.

### Canvases

A `Raster` is immutable, and generating one pixel at a time through `Raster(w, h)(…)` is not
always the shape a drawing algorithm wants. Opening a raster as a `Canvas` gives a scoped handle
over its buffer instead. Any canvas reads; only one opened with the `Write` grant may be written
to, and the write mutates in place:

```scala
raster.open[Canvas](Read & Write): canvas ?=>
  canvas(0, 0) = Pixel[Rgb](Srgb(1.0, 0.0, 0.0))
```

`snapshot` takes an independent copy, so a derived image is unaffected by later writes.

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
val webpBytes = image.to[Webp].read[Data]
```

The codecs — PNG with its filters, baseline and progressive JPEG, WebP in both its lossy VP8 and
lossless forms, GIF with its LZW, and BMP — are written directly against the formats. A backend
seam picks between them and the platform's own imaging library where one exists, so the same code
converts an image on the JVM, in a browser, and inside a WebAssembly component.
