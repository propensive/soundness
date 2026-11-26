# A resource which the compiler has looked for

A reference to a resource on the classpath, written with `cp"…"`, is looked up as the code
compiles, on the classpath of the compiler. One which is not there is a compile error, and not
a `null` found when the program first asks for it.

```scala fuel
compile
frame
  label A resource which does not exist
  errors show
  code
      val resource = cp"/scala/Nothing/Here.txt"
frame
  label One which does
  highlight cp"/scala/Option.class"  found on the classpath
  code
      val resource = cp"/scala/Option.class"
frame
  label Its bytes
  highlight read[Data]
  code
      import classloaders.threadContextClassloader, strategies.throwUnsafely

      val resource = cp"/scala/Option.class"
      val bytes = resource.read[Data]
##
```

[Reading a resource](../../soundness/modules/classpath.md#reading-a-resource) describes the
check, and [safety by construction](../../philosophy/safety-by-construction.md) the reason for
it.
