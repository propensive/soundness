# One method shows every type

There is no `toString`, `mkString` or `format` to choose between. A value is written for a
person with `show`, and a typeclass supplies the form for each type which has one.

```scala fuel
compile
frame
  label A number
  code
      val lines: Text = 4042.show
frame
  label Text
  highlight t"Illyria".show  the same method
  code
      val lines: Text = 4042.show
      val setting: Text = t"Illyria".show
frame
  label A list
  highlight List(1,..3).show  and again
  code
      val lines: Text = 4042.show
      val setting: Text = t"Illyria".show
      val acts: Text = List(1, 2, 3).show
frame
  label A type which has not said how it is shown
  errors show
  code
      case class Purse(pence: Int)

      val purse: Text = Purse(1099).show
##
```

[Showing a value](../../soundness/modules/display.md#showing-a-value) explains why there is no
fallback, and [small APIs](../../philosophy/small-apis.md) why there is one name.
