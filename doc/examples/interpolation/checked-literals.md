# A media type the compiler has read

An interpolator parses its literal as the code compiles. A `media"…"` which is not a
registered media type is an error at the place it is misspelled, and one which compiles is a
`MediaType`, not text.

```scala fuel
compile
frame
  label A misspelled subtype
  errors show
  code
      val plain: MediaType = media"text/plian"
frame
  label A media type
  highlight media"text/plain"  a MediaType, not text
  code
      val plain: MediaType = media"text/plain"
##
```

[Interpolation](../../soundness/modules/interpolation.md#on-interpolators) explains what an
interpolator does with a literal, and
[safety by construction](../../philosophy/safety-by-construction.md) why it matters.
