# A failure the signature admits to

Text may not hold a number, so `as[Int]` can fail. A method which calls it must either say so
in its own type, with `raises`, or deal with the failure. One which does neither does not
compile.

```scala fuel
compile
frame
  label A method which does not say that it can fail
  errors show
  code
      def port(setting: Text): Int = setting.as[Int]
frame
  label The failure, in the return type
  highlight raises..Error  the failure is part of the type
  code
      def port(setting: Text): Int raises Number.Error = setting.as[Int]
##
```

[Declaring that code can fail](../../soundness/modules/errors.md#declaring-that-code-can-fail)
explains what `raises` is.
