# One method reads every type

There is no `toInt`, `parseDate` or `fromString`. Text is read with `as`, which takes the type
that is wanted, and each library teaches it a type of its own.

```scala fuel
compile
frame
  label A number
  code
      import strategies.throwUnsafely

      val port = t"8080".as[Int]
frame
  label An address
  highlight Ipv4
  code
      import strategies.throwUnsafely

      val port = t"8080".as[Int]
      val address = t"192.168.0.1".as[Ipv4]
frame
  label A version
  highlight Semver
  code
      import strategies.throwUnsafely

      val port = t"8080".as[Int]
      val address = t"192.168.0.1".as[Ipv4]
      val version = t"2.1.0".as[Semver]
frame
  label A type which nothing knows how to read
  errors show
  code
      import strategies.throwUnsafely

      class Widget()

      val widget = t"sprocket".as[Widget]
##
```

[Decoding](../../soundness/modules/decoding.md) shows how a new type joins in, and
[small APIs](../../philosophy/small-apis.md) why there is one name.
