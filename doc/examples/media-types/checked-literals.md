# A media type the registry does not have

A misspelled media type looks plausible, and no consumer recognizes it. A `media"…"` literal
is looked up in the IANA registry as the code compiles, and a near miss is answered with what
was probably meant.

```scala fuel
compile
frame
  label A slip of the finger
  errors show
  code
      val accept = media"application/jsom"
frame
  label A registered type
  highlight media"application/json"  registered with IANA
  code
      val accept = media"application/json"
frame
  label A vendor's own type, which is not looked up
  highlight vnd.globe.script  the vendor tree
  code
      val accept = media"application/vnd.globe.script+json"
##
```

[Writing a media type](../../soundness/modules/media-types.md#writing-a-media-type) says which
trees are checked, and
[safety by construction](../../philosophy/safety-by-construction.md) why a literal is.
