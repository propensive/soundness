## Images

### About

Raster images, which are PNG, JPEG, WebP, GIF and BMP, read into a `Raster` value with the
format in its type, just as [audio](audio.md) carries its format. A `Raster in Png` and a
`Raster in Jpeg` are distinct types, and converting between them is one method. The image itself
is an [immutable] grid of pixels. Each pixel is read as a typed [color](colors.md), and
cropping, flipping and rotation produce new images.

A raster may also carry its *pixel layout* in its type: how many bits each channel occupies, and
in what order. A pixel is then read with a single shift and mask, computed as the code compiles
rather than dispatched at [runtime]. Every codec is implemented directly, so images read and
write on every platform, not only on the JVM.

### On raster images

The imaging API of the JVM reads any format into a mutable `BufferedImage` and forgets which
format it was, and leaves encoding decisions and pixel formats to flag-driven calls. For the
common tasks, which are to read an image, ask its size, look at its pixels, convert its format
and write it back, that machinery is mostly ceremony, and the mutability makes an image unsafe
to share.

A `Raster` is immutable and knows its format. Reading names the format expected, so a JPEG
posing as a PNG is a typed error, not a null. Pixels come back as colors, ready for the color
operations. Writing is the same polymorphic reading-as-bytes used by every other format. A
raster whose format and color model are in its type follows
[safety by construction](../philosophy/safety-by-construction.md): a conversion which cannot be
done cannot be written.

Everything comes from the `soundness` package. The other imports choose a [strategy] for errors,
and let the samples print their results:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Reading

A source of bytes reads as a raster of a named format. These bytes are a complete PNG, a
two-by-one image whose left pixel is red and whose right pixel is blue. It is small enough to
write down:

```scala
val data = hex"""89504e470d0a1a0a0000000d49484452000000020000000108020000007b40e8dd
                 0000000d4944415478da63f8cf0004ff01070001ff3d7d8c490000000049454e44ae426082"""

val image = data.read[Raster in Png]
```

```scala fuel
run
##
Out.println(image.width.show)
Out.println(image.height.show)
Out.println(image.landscape.inspect)
```

Bytes which are not in the named format raise a `Raster.Error`, which names the format that
failed:

```scala fuel
run
errors show
##
data.read[Raster in Jpeg]
```

### Pixels

Applying an image to coordinates reads a pixel as a `Chroma`, a color with byte channels, which
connects the image to all the [color](colors.md) machinery:

```scala fuel
run
##
Out.println(image(0, 0).red.show)
Out.println(image(0, 0).blue.show)
Out.println(image(1, 0).blue.show)
```

An image is also *built* from a pixel function, which is how test images and generated graphics
are made:

```scala
val gradient = Raster(256, 1)((x, y) => Chroma(x, 0, 255 - x))
```

```scala fuel
run
##
Out.println(gradient(0, 0).blue.show)
Out.println(gradient(255, 0).blue.show)
```

### Pixel layouts

A layout is a tuple of channel types, most significant first. `(Red[10], Green[12], Blue[10])`
is exactly the packing of a 32-bit RGB pixel, and `Rgba` names the familiar eight-bit one. A
raster built with a layout gives typed access, and `pixel` compiles to one constant shift and
mask:

```scala
val raster = Raster[Rgba](2, 2): (x, y) =>
  Pixel[Rgba](Srgb(x.toDouble, y.toDouble, 1.0))
```

```scala fuel
run
##
Out.println(raster.pixel(1, 0).red.show)
Out.println(raster.pixel(1, 0).alpha.show)
```

`repack` converts between layouts. It adds a fully opaque alpha channel where the target has
one, and scales the channels where their depths differ. Repacking to the layout which a raster
already has returns the same raster. A `descriptor` reports the layout at runtime, for the
operations which must work whatever it is.

### Canvases

A `Raster` is immutable, and generating one pixel at a time through `Raster(w, h)(…)` is not
always the shape which a drawing algorithm wants. Opening a raster as a `Canvas` gives a scoped
handle over its buffer instead. Any canvas reads. Only one opened with the `Write` grant may be
written to, and the write mutates in place:

```scala
import soundness.canvasOpenable

raster.open[Canvas](Read & Write): canvas ?=>
  canvas(0, 0) = Pixel[Rgba](Srgb(1.0, 0.0, 0.0))
```

`snapshot` takes an independent copy, so a derived image is unaffected by later writes.

### Transforming

Cropping, flipping and quarter-turn rotation each produce a new image:

```scala fuel
run
##
Out.println(image.crop(right = 1).width.show)
Out.println(image.flipX(0, 0).blue.show)
Out.println(image.rotate(90).height.show)
```

The argument of `rotate` admits only `90`, `180` or `270`. A rotation which would need
resampling is not quietly approximated:

```scala fuel
compile
errors show
##
image.rotate(45)
```

### Converting and writing

`to` re-expresses an image in another format, and reading it as `Data` produces the encoded
bytes, symmetrically with reading:

```scala fuel
run
##
val jpegBytes = image.to[Jpeg].read[Data]
val webpBytes = image.to[Webp].read[Data]

Out.println(webpBytes.read[Raster in Webp](1, 0).blue.show)
```

The codecs are written directly against the formats:

```tabular
column  Format
column  Reads
column  Writes
column  On the JVM
row  PNG  every color type, bit depths 1 to 16, all five filters, Adam7 interlacing  8-bit truecolor, with alpha when the layout has it  `javax.imageio`
row  JPEG  baseline and progressive, grayscale, YCbCr and CMYK  baseline  `javax.imageio`
row  WebP  lossy VP8, lossless VP8L and extended VP8X  lossless VP8L  the pure codec
row  GIF  GIF87a and GIF89a, first frame only  one GIF89a frame of at most 256 colors  `javax.imageio`
row  BMP  1- to 32-bit, `BI_RGB` or `BI_BITFIELDS`, no run-length encoding  24-bit `BI_RGB`  `javax.imageio`
```

A backend seam picks between each codec and the platform's own imaging library, where one
exists, so the same code converts an image on the JVM, in a browser, and inside a WebAssembly
component.

### Related

- [Colors](colors.md) describes `Chroma`, `Srgb` and the operations on a pixel's color.
- [Audio](audio.md) carries its format in its type in the same way.
- [Binary data](binary-data.md) describes `hex"…"` and the `Data` which encoded images are.
- [SVG](svg.md) describes vector graphics, which have no pixels until they are rendered.
- [Media types](media-types.md) names the formats.
- [Web automation](web-automation.md) captures a screenshot as a `Raster in Png`.
