# A fallible method is called where its error is handled

A method which `raises` an error takes its handler as a contextual parameter. Passing the method
itself out of `safely`, uncalled, would bind it to a handler which is about to disappear, and the
compiler rejects that as it would reject any other value which outlives its scope.

```scala fuel
compile
feature experimental.captureChecking
frame
  label The method, taken out uncalled
  errors show
  highlight safely(lines)
  code
      def lines(part: Text): Int raises Number.Error = part.as[Int]

      val later = safely(lines)
frame
  label The method, called inside the block
  highlight lines(t"1,495")
  code
      def lines(part: Text): Int raises Number.Error = part.as[Int]

      val hamlet = safely(lines(t"1,495"))
##
```

The text has a comma in it, so `hamlet` is `Unset`, and the failure went no further than the
block. [Declaring that code can fail](../../soundness/modules/errors.md#declaring-that-code-can-fail)
describes `raises`.
