- ## Images
+ ## Images

- ### About
+ ### Présentation

- Raster images, which are PNG, JPEG, WebP, GIF and BMP, read into a `Raster` value with the
- format in its type, just as [audio](audio.md) carries its format. A `Raster in Png` and a
- `Raster in Jpeg` are distinct types, and converting between them is one method. The image itself
- is an [immutable] grid of pixels. Each pixel is read as a typed [color](colors.md), and
- cropping, flipping and rotation produce new images.
+ Les images matricielles, c'est-à-dire PNG, JPEG, WebP, GIF et BMP, se lisent en une valeur
+ `Raster` dont le type porte le format, tout comme l'[audio](audio.md) porte le sien. Un
+ `Raster in Png` et un `Raster in Jpeg` sont des types distincts, et convertir l'un en l'autre
+ tient en une méthode. L'image elle-même est une grille [immuable] de pixels. Chaque pixel se
+ lit comme une [couleur](colors.md) typée, et le recadrage, le retournement et la rotation
+ produisent de nouvelles images.

- A raster may also carry its *pixel layout* in its type: how many bits each channel occupies, and
- in what order. A pixel is then read with a single shift and mask, computed as the code compiles
- rather than dispatched at [runtime]. Every codec is implemented directly, so images read and
- write on every platform, not only on the JVM.
+ Une image matricielle peut aussi porter dans son type sa *disposition des pixels* : le nombre de
+ bits qu'occupe chaque canal, et dans quel ordre. Un pixel se lit alors par un seul décalage et
+ un seul masque, calculés pendant la compilation du code plutôt que choisis
+ [à l'exécution][exécution]. Chaque codec est implémenté directement, si bien que les images se
+ lisent et s'écrivent sur toutes les plateformes, et pas seulement sur la JVM.

- ### On raster images
+ ### Des images matricielles

- The imaging API of the JVM reads any format into a mutable `BufferedImage` and forgets which
- format it was, and leaves encoding decisions and pixel formats to flag-driven calls. For the
- common tasks, which are to read an image, ask its size, look at its pixels, convert its format
- and write it back, that machinery is mostly ceremony, and the mutability makes an image unsafe
- to share.
+ L'API d'imagerie de la JVM lit n'importe quel format en une `BufferedImage` mutable et oublie
+ de quel format il s'agissait ; elle laisse les choix d'encodage et les formats de pixels à des
+ appels pilotés par des drapeaux. Pour les tâches courantes, qui sont de lire une image, de
+ demander sa taille, d'examiner ses pixels, de convertir son format et de la réécrire, cette
+ machinerie relève surtout du cérémonial, et la mutabilité rend une image dangereuse à partager.

- A `Raster` is immutable and knows its format. Reading names the format expected, so a JPEG
- posing as a PNG is a typed error, not a null. Pixels come back as colors, ready for the color
- operations. Writing is the same polymorphic reading-as-bytes used by every other format. A
- raster whose format and color model are in its type follows
- [safety by construction](../../philosophy/safety-by-construction.md): a conversion which cannot be
- done cannot be written.
+ Un `Raster` est immuable et connaît son format. La lecture nomme le format attendu : un JPEG
+ qui se fait passer pour un PNG est donc une erreur typée, et non un null. Les pixels sont
+ renvoyés sous forme de couleurs, prêts pour les opérations sur les couleurs. L'écriture est la
+ même lecture polymorphe en octets qu'utilisent tous les autres formats. Une image matricielle
+ dont le format et le modèle de couleur sont dans le type suit le principe de la
+ [sûreté par construction](../../philosophy/safety-by-construction.md) : une conversion
+ impossible ne peut pas être écrite.

- Everything comes from the `soundness` package. The other imports choose a [strategy] for errors,
- and let the samples print their results:
+ Tout provient du package `soundness`. Les autres imports choisissent une [stratégie] pour les
+ erreurs, et permettent aux exemples d'afficher leurs résultats :

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

- ### Reading
+ ### Lecture

- A source of bytes reads as a raster of a named format. These bytes are a complete PNG, a
- two-by-one image whose left pixel is red and whose right pixel is blue. It is small enough to
- write down:
+ Une source d'octets se lit comme une image matricielle d'un format nommé. Ces octets forment un
+ PNG complet, une image de deux pixels sur un dont le pixel de gauche est rouge et celui de
+ droite bleu. Elle est assez petite pour être écrite en entier :

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

- Bytes which are not in the named format raise a `Raster.Error`, which names the format that
- failed:
+ Des octets qui ne sont pas dans le format nommé lèvent une `Raster.Error`, qui nomme le format
+ en échec :

```scala fuel
run
errors show
##
data.read[Raster in Jpeg]
```

- ### Pixels
+ ### Pixels

- Applying an image to coordinates reads a pixel as a `Chroma`, a color with byte channels, which
- connects the image to all the [color](colors.md) machinery:
+ Appliquer une image à des coordonnées lit un pixel sous la forme d'un `Chroma`, une couleur à
+ canaux d'un octet, ce qui relie l'image à toute la machinerie des [couleurs](colors.md) :

```scala fuel
run
##
Out.println(image(0, 0).red.show)
Out.println(image(0, 0).blue.show)
Out.println(image(1, 0).blue.show)
```

- An image is also *built* from a pixel function, which is how test images and generated graphics
- are made:
+ Une image se *construit* aussi à partir d'une fonction de pixels ; c'est ainsi que l'on produit
+ des images de test et des graphismes générés :

```scala
val gradient = Raster(256, 1)((x, y) => Chroma(x, 0, 255 - x))
```

```scala fuel
run
##
Out.println(gradient(0, 0).blue.show)
Out.println(gradient(255, 0).blue.show)
```

- ### Pixel layouts
+ ### Dispositions des pixels

- A layout is a tuple of channel types, most significant first. `(Red[10], Green[12], Blue[10])`
- is exactly the packing of a 32-bit RGB pixel, and `Rgba` names the familiar eight-bit one. A
- raster built with a layout gives typed access, and `pixel` compiles to one constant shift and
- mask:
+ Une disposition est un tuple de types de canaux, le plus significatif en premier.
+ `(Red[10], Green[12], Blue[10])` correspond exactement à l'empaquetage d'un pixel RVB de 32 bits,
+ et `Rgba` nomme le pixel familier à huit bits par canal. Une image matricielle construite avec
+ une disposition offre un accès typé, et `pixel` se compile en un seul décalage et un seul masque
+ constants :

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

- `repack` converts between layouts. It adds a fully opaque alpha channel where the target has
- one, and scales the channels where their depths differ. Repacking to the layout which a raster
- already has returns the same raster. A `descriptor` reports the layout at runtime, for the
- operations which must work whatever it is.
+ `repack` convertit d'une disposition à l'autre. Elle ajoute un canal alpha entièrement opaque
+ lorsque la cible en possède un, et met les canaux à l'échelle lorsque leurs profondeurs
+ diffèrent. Réempaqueter vers la disposition qu'une image matricielle a déjà renvoie la même
+ image. Un `descriptor` indique la disposition à l'exécution, pour les opérations qui doivent
+ fonctionner quelle qu'elle soit.

- ### Canvases
+ ### Canevas

- A `Raster` is immutable, and generating one pixel at a time through `Raster(w, h)(…)` is not
- always the shape which a drawing algorithm wants. Opening a raster as a `Canvas` gives a scoped
- handle over its buffer instead. Any canvas reads. Only one opened with the `Write` grant may be
- written to, and the write mutates in place:
+ Un `Raster` est immuable, et en générer un pixel par pixel avec `Raster(w, h)(…)` n'est pas
+ toujours la forme que souhaite un algorithme de dessin. Ouvrir une image matricielle comme
+ `Canvas` donne plutôt un accès délimité à son tampon. Tout canevas peut être lu. Seul un canevas
+ ouvert avec l'autorisation `Write` peut être écrit, et l'écriture le modifie en place :

```scala
import soundness.canvasOpenable

raster.open[Canvas](Read & Write): canvas ?=>
  canvas(0, 0) = Pixel[Rgba](Srgb(1.0, 0.0, 0.0))
```

- `snapshot` takes an independent copy, so a derived image is unaffected by later writes.
+ `snapshot` en prend une copie indépendante, de sorte qu'une image dérivée n'est pas affectée par
+ les écritures ultérieures.

- ### Transforming
+ ### Transformations

- Cropping, flipping and quarter-turn rotation each produce a new image:
+ Le recadrage, le retournement et la rotation par quart de tour produisent chacun une nouvelle
+ image :

```scala fuel
run
##
Out.println(image.crop(right = 1).width.show)
Out.println(image.flipX(0, 0).blue.show)
Out.println(image.rotate(90).height.show)
```

- The argument of `rotate` admits only `90`, `180` or `270`. A rotation which would need
- resampling is not quietly approximated:
+ L'argument de `rotate` n'admet que `90`, `180` ou `270`. Une rotation qui exigerait un
+ rééchantillonnage n'est pas approximée en silence :

```scala fuel
compile
errors show
##
image.rotate(45)
```

- ### Converting and writing
+ ### Conversion et écriture

- `to` re-expresses an image in another format, and reading it as `Data` produces the encoded
- bytes, symmetrically with reading:
+ `to` exprime une image dans un autre format, et la lire comme `Data` produit les octets encodés,
+ de façon symétrique à la lecture :

```scala fuel
run
##
val jpegBytes = image.to[Jpeg].read[Data]
val webpBytes = image.to[Webp].read[Data]

Out.println(webpBytes.read[Raster in Webp](1, 0).blue.show)
```

- The codecs are written directly against the formats:
+ Les codecs sont écrits directement d'après les formats :

```tabular
- column  Format
- column  Reads
- column  Writes
- column  On the JVM
- row  PNG  every color type, bit depths 1 to 16, all five filters, Adam7 interlacing  8-bit truecolor, with alpha when the layout has it  `javax.imageio`
- row  JPEG  baseline and progressive, grayscale, YCbCr and CMYK  baseline  `javax.imageio`
- row  WebP  lossy VP8, lossless VP8L and extended VP8X  lossless VP8L  the pure codec
- row  GIF  GIF87a and GIF89a, first frame only  one GIF89a frame of at most 256 colors  `javax.imageio`
- row  BMP  1- to 32-bit, `BI_RGB` or `BI_BITFIELDS`, no run-length encoding  24-bit `BI_RGB`  `javax.imageio`
+ column  Format
+ column  Lecture
+ column  Écriture
+ column  Sur la JVM
+ row  PNG  tous les types de couleur, profondeurs de 1 à 16 bits, les cinq filtres, entrelacement Adam7  couleurs vraies 8 bits, avec alpha lorsque la disposition en a un  `javax.imageio`
+ row  JPEG  baseline et progressif, niveaux de gris, YCbCr et CMYK  baseline  `javax.imageio`
+ row  WebP  VP8 avec perte, VP8L sans perte et VP8X étendu  VP8L sans perte  le codec pur
+ row  GIF  GIF87a et GIF89a, première image seulement  une image GIF89a d'au plus 256 couleurs  `javax.imageio`
+ row  BMP  de 1 à 32 bits, `BI_RGB` ou `BI_BITFIELDS`, sans encodage par plages  `BI_RGB` 24 bits  `javax.imageio`
```

- A backend seam picks between each codec and the platform's own imaging library, where one
- exists, so the same code converts an image on the JVM, in a browser, and inside a WebAssembly
- component.
+ Une jonction de backends choisit entre chaque codec et la bibliothèque d'imagerie propre à la
+ plateforme, lorsqu'il en existe une, de sorte que le même code convertit une image sur la JVM,
+ dans un navigateur et à l'intérieur d'un composant WebAssembly.

- ### Related
+ ### Voir aussi

- - [Colors](colors.md) describes `Chroma`, `Srgb` and the operations on a pixel's color.
- - [Audio](audio.md) carries its format in its type in the same way.
- - [Binary data](binary-data.md) describes `hex"…"` and the `Data` which encoded images are.
- - [SVG](svg.md) describes vector graphics, which have no pixels until they are rendered.
- - [Media types](media-types.md) names the formats.
- - [Web automation](web-automation.md) captures a screenshot as a `Raster in Png`.
- - [Filesystem](filesystem.md) describes the files which images are read from and written to.
+ - [Couleurs](colors.md) décrit `Chroma`, `Srgb` et les opérations sur la couleur d'un pixel.
+ - [Audio](audio.md) porte son format dans son type de la même manière.
+ - [Données binaires](binary-data.md) décrit `hex"…"` et les `Data` que sont les images encodées.
+ - [SVG](svg.md) décrit les graphismes vectoriels, qui n'ont pas de pixels avant d'être rendus.
+ - [Types de médias](media-types.md) nomme les formats.
+ - [Automatisation web](web-automation.md) capture une copie d'écran sous la forme d'un
+   `Raster in Png`.
+ - [Système de fichiers](filesystem.md) décrit les fichiers depuis lesquels les images sont lues
+   et dans lesquels elles sont écrites.
