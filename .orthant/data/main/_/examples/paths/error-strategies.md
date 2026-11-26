# Three answers to text which is not a path

Reading a path from text does not decide what happens when the text breaks the platform's
rules. The code which reads it does, and each choice gives the method a different type.

```scala fuel
compile
alternative An absent value
  highlight safely  a failure becomes an absent value
  code
      def locate(text: Text): Optional[Path on Linux] =
        safely(text.as[Path on Linux])
alternative A replacement
  highlight recover  a path to use instead
  code
      def locate(text: Text): Path on Linux =
        recover:
          case error: Path.Error => (% / "tmp").on[Linux]
        . protect(text.as[Path on Linux])
alternative The caller's decision
  highlight raises..Error  the way it can fail
  code
      def locate(text: Text): Path on Linux raises Path.Error =
        text.as[Path on Linux]
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) has the full set,
and [Text and paths](../../soundness/modules/paths.md#text-and-paths) the decoding.
