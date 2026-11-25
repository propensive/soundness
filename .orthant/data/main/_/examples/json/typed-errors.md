# Decoding declares how it fails

Decoding a case class from JSON can fail, and the type of the method which does it says
so. A method which neither handles the failure nor declares it does not compile.

```scala fuel
compile
frame
  label A failure which is not accounted for
  errors show
  code
      case class Player(name: Text, age: Int)

      def player(json: Json): Player = json.as[Player]
frame
  label The failure, declared
  highlight raises..Error  part of the type
  code
      case class Player(name: Text, age: Int)

      def player(json: Json): Player raises Json.Error = json.as[Player]
##
```

[Errors](../../soundness/modules/json.md#errors) lists the reasons a `Json.Error` gives, and
[error handling](../../philosophy/error-handling.md) explains `raises`.
