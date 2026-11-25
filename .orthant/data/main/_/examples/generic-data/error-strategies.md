# What happens to data of the wrong shape

The decoder of a `Pojo` does not decide what a mismatch does. The code which receives the data
chooses, and the same `as[Player]` serves each choice.

```scala fuel
compile
alternative An absent value
  highlight safely  a mismatch is Unset
  code
      case class Player(name: Text, age: Int)

      def receive(data: Pojo): Optional[Player] = safely(data.as[Player])
alternative An understudy
  highlight recover  a mismatch is replaced
  code
      case class Player(name: Text, age: Int)

      def receive(data: Pojo): Player =
        recover:
          case error: Pojo.Error => Player(t"Snug", 30)
        . protect(data.as[Player])
alternative The caller's decision
  highlight raises..Pojo.Error  a mismatch is the caller's
  code
      case class Player(name: Text, age: Int)

      def receive(data: Pojo): Player raises Pojo.Error = data.as[Player]
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) has the full set.
