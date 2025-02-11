makes an instance of `:Annotations[A, T]` contextually available, if it can be constructed

This makes it possible to access annotations on a generic case class by using this value, for example:
```scala
case class id() extends StaticAnnotation
def getIdAnnotations[T](using a: Annotations[id, T]): Unit =
  println(a.annotations)
```