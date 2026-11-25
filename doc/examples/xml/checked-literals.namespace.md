# A prefix which is bound to nothing

A prefixed name means nothing until its prefix is bound to a namespace. An `x"…"` literal
which uses a prefix that nothing declares does not compile, and a `Namespace` given, which
names the prefix and the URI in its type, binds one.

```scala fuel
compile
frame
  label A prefix with no binding
  errors show
  code
      val shape = x"<svg:rect/>"
frame
  label The prefix, bound to a namespace
  highlight "svg"..svg"  the prefix and the URI, as types
  code
      given svg: ("svg" is Namespace of "http://www.w3.org/2000/svg") = Namespace()

      val shape = x"<svg:rect/>"
##
```

[Namespaces](../../soundness/modules/xml.md#namespaces) describes how names are resolved as a
document is read and written.
