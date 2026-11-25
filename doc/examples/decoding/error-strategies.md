# One decoder, three ways to fail

`as[Int]` does not decide what happens when text is not a number. The code which calls it
does, and each choice gives the result a different type.

```scala fuel
compile
alternative An absent value
  highlight safely
  code
      def port(setting: Text): Optional[Int] = safely(setting.as[Int])
alternative A replacement
  highlight recover
  code
      def port(setting: Text): Int =
        recover:
          case error: Number.Error => 8080
        . protect(setting.as[Int])
alternative The caller's decision
  highlight raises..Error  the caller chooses
  code
      def port(setting: Text): Int raises Number.Error = setting.as[Int]
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) has the full set.
