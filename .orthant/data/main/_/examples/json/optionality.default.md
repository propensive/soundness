# Three ways for a field to be missing

A field's type says what a document may leave out. A plain field is required, a field with a
default takes the default, and an `Optional` field reads as `Unset`. There is no fourth case in
which a missing field becomes a `null`.

```scala fuel
compile
frame
  label A required field
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int)

      val fool: Optional[Player] = safely(j"""{"name": "Fool"}""".as[Player])
frame
  label A default
  highlight age..18  used when the key is missing
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int = 18)

      val fool: Optional[Player] = safely(j"""{"name": "Fool"}""".as[Player])
frame
  label An optional field
  highlight Optional[Int]  Unset when the key is missing
  code
      import strategies.throwUnsafely

      case class Player(name: Text, age: Int = 18, patron: Optional[Int])

      val fool: Optional[Player] = safely(j"""{"name": "Fool"}""".as[Player])
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(fool.inspect)
```

The first frame's `fool` is `Unset`, since the document does not decode.
[Optional fields and defaults](../../soundness/modules/json.md#optional-fields-and-defaults)
has each case.
