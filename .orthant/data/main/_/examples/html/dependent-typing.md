# An attribute knows what it holds

Each attribute has the type the specification gives it. The width of an image is a whole number
of pixels, so `width` takes an `Int`, and a fraction is a compile error.

```scala fuel
compile
frame
  label A fractional width
  errors show
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val portrait = Img(width = 64.5)
frame
  label A whole number of pixels
  highlight 64  an `Int`, as the specification says
  code
      import htmlDoms.whatwg, htmlDoms.whatwg.*

      val portrait = Img(width = 64)
##
```

[Attributes](../../soundness/modules/html.md#attributes) describes the types the attributes take.
