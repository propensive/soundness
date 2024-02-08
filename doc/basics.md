All terms and types are define in the `hallucination` package, and can be used with:
```scala
import hallucination.*
```

### Reading images

To read an image of a known type, the `read` method of its codec object should
be used. Currently, four codecs are defined:
 - `Png`
 - `Gif`
 - `Jpeg`, and
 - `Bmp`

The source of the image may be any source that can be read as `Bytes` by
Turbulence, for example:
```scala
import galilei.*

val image = Png.read(% / p"home" / p"work" / p"image.png")
```

The resultant value will be an instance of `Image[Png]`, that is, an `Image`
parameterized with the erased phantom type `Png`.

It's possible to use Turbulence's `readAs` method to read an `Image[?]` from a
source of `Bytes`, for example:
```scala
import turbulence.*
import hellenism.*

val icon = (Classpath / p"icon.bmp").readAs[Image[Bmp]]
```
