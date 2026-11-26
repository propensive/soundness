# A function cannot leave its error handler

`safely` turns a failure inside its block into an absent value. A function made inside the block
could fail later, when it is called, long after `safely` has returned and there is nothing left
to catch the failure. The function's type records the handler it relies on, so it cannot be the
block's result.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which would fail outside the block
  errors show
  highlight t"thrice".as[Int]  would run later, with nothing to handle its error
  code
      val count = safely(() => t"thrice".as[Int])
frame
  label The fallible code, run inside it
  highlight safely
  code
      val count = safely(t"thrice".as[Int])
##
```

The result is `Unset`: the text is not a number, and the failure went no further than the
block. [Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) lists the handlers
which a block may be given, and [capture checking](../../philosophy/capture-checking.md) explains
how one is kept from outliving its block.
