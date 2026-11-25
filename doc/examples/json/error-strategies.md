# The caller decides what bad JSON does

`as[Player]` raises a `Json.Error` when a document is not a player. What becomes of the error
is decided where the decoder is called, and each decision gives the result a different type.

```scala fuel
compile
alternative An absent value
  highlight safely  a failure becomes Unset
  code
      case class Player(name: Text, age: Int)

      def player(json: Json): Optional[Player] = safely(json.as[Player])
alternative A replacement
  highlight recover  a failure becomes a value
  code
      case class Player(name: Text, age: Int)

      def player(json: Json): Player =
        recover:
          case error: Json.Error => Player(t"Feste", 30)
        . protect(json.as[Player])
alternative The caller's decision
  highlight raises..Error  a failure goes to the caller
  code
      case class Player(name: Text, age: Int)

      def player(json: Json): Player raises Json.Error = json.as[Player]
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) compares them, and
[collecting every error](../../soundness/modules/json.md#collecting-every-error) shows a
strategy which reports all of a document's faults at once.
