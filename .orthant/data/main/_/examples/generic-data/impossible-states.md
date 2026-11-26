# Only an encoder makes a Pojo

A `Pojo` is an opaque type over the few JDK types which it permits. An arbitrary object is
not one, even an object of a permitted type, so anything which is typed as a `Pojo` was
produced by an encoder and can be decoded.

```scala fuel
compile
frame
  label A JDK object is not a Pojo
  errors show
  code
      val count: Pojo = java.lang.Integer.valueOf(35)
frame
  label An encoded value is
  highlight pojo  the only way to make one
  code
      val count: Pojo = 35.pojo
##
```

[The representation](../../soundness/modules/generic-data.md#the-representation) lists what
each Scala value becomes.
