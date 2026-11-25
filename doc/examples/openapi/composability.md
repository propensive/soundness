# An image from an API

A response of the media type `image/png` is carried by a `Raster in Png`, the type of an
image everywhere else. The client knows nothing of images. One import says which type carries
the media type, and the image's own reader does the rest.

```scala fuel
compile
frame
  label A ticket's QR code
  highlight pngConstruable  image/png is a Raster in Png
  code
      import strategies.throwUnsafely, internetAccess.online, httpBackends.javaNetHttp
      import logging.silentLogging, construables.jsonConstruable, codepages.utf8Codepage
      import errorDiagnostics.stackTracesDiagnostics, construables.pngConstruable

      given museumAuth: ("MuseumPlaceholderAuth" is Credential to Auth) =
        Credential(Auth.Basic(t"feste", t"hey-ho"))

      val museum = Api(cp"/openapi/redocly/museum.yaml")

      val ticket: Raster in Png = museum.tickets(t"a1").qr.get.call()
##
```

The example is compiled, and is not run, since it needs the network.
[Media types and their carriers](../../soundness/modules/openapi.md#media-types-and-their-carriers)
has the carrier of each media type, and [images](../../soundness/modules/images.md) what can
be done with a `Raster`.
