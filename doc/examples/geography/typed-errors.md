# A geo URI which may be malformed

A `geo:` URI arrives as text, and text may be anything. Decoding it raises a
`Geolocation.Error`, and a method which decodes one must say so in its type.

```scala fuel
compile
frame
  label A method which does not say that it can fail
  errors show
  code
      def locate(uri: Text): Location = uri.as[Geolocation].location
frame
  label The failure, in the return type
  highlight raises..Error  the way it can fail
  code
      def locate(uri: Text): Location raises Geolocation.Error =
        uri.as[Geolocation].location
frame
  label Or no location at all
  highlight safely  a failure becomes an absent value
  code
      def locate(uri: Text): Optional[Location] =
        safely(uri.as[Geolocation].location)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(locate(t"geo:56.04,12.62").inspect)
Out.println(locate(t"geo:elsinore").inspect)
```

[Geo URIs](../../soundness/modules/geography.md#geo-uris) describes the format.
