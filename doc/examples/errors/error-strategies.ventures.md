# Every fault of a document, in one pass

A decoder which stops at the first bad field reports one fault at a time. A `venture` records
its errors and lets its siblings run, and its value can be forced only inside a `guard`, which
is skipped if anything failed.

```scala fuel
compile
frame
  label A venture forced where nothing can be skipped
  errors show
  code
      import dynamicAccess.dynamicJson

      case class Player(name: Text, age: Int)

      def player(json: Json): Player raises Json.Error =
        val name = venture(json.name.as[Text])
        val age = venture(json.age.as[Int])
        Player(name(), age())
frame
  label Forced within a guard
  highlight guard  skipped if a venture failed
  code
      import dynamicAccess.dynamicJson

      case class Player(name: Text, age: Int)

      def player(json: Json): Player raises Json.Error =
        val name = venture(json.name.as[Text])
        val age = venture(json.age.as[Int])
        guard(Player(name(), age()))
##
```

[Ventures and guards](../../soundness/modules/errors.md#ventures-and-guards) explains what is
skipped and what still runs.
